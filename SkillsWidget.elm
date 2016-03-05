module SkillsWidget where

import Html exposing (Html)
import Html.Attributes as Attr
import Dict exposing (Dict)
import List.Extra as ListEx
import StartApp.Simple as StartApp

import Selectable as Sel
import MultiSelect as MultSel
import Data exposing (Model, LinkedSelectable)


main : Signal Html
main =
  StartApp.start
    { model = data
    , update = update
    , view = view
    }


-- MODEL

data : Model
data = Data.data

type alias ID = Int


-- UPDATE

type Action
    = PosCats MultSel.Action
    | CoreComps MultSel.Action


update : Action -> Model -> Model
update action model =
    case action of
        PosCats msAction ->
            { model |
                positionCategories = 
                    updateLinkedSels msAction model.positionCategories
            }
        CoreComps msAction ->
            { model |
                coreCompetencies =
                    updateLinkedSels msAction model.coreCompetencies          
            }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    Html.div []
        [ multiSelectView
            (Signal.forwardTo address PosCats)
            (extractSelectables model.positionCategories)
            "Position Categories"
        , multiSelectView
            (Signal.forwardTo address CoreComps)
            (extractSelectables <| availableCompetencies model)
            "Core Competencies"
        ]

multiSelectView : Signal.Address MultSel.Action -> MultSel.Model -> String -> Html
multiSelectView msAddress msModel msName =
    Html.div []
        [ Html.h3 [] [ Html.text msName ]
        , MultSel.view msAddress msModel
        ]



------------------------
-- LOGIC
------------------------

{-| extract selectables from their LinkedSelectable wrappers, 
    update them according to the MultiSelect Action that just occured, 
    then merge the new selectables back into the wrappers
-}
updateLinkedSels : MultSel.Action -> List LinkedSelectable -> List LinkedSelectable
updateLinkedSels msAction linkedSels =
    linkedSels
        |> extractSelectables
        |> MultSel.update msAction 
        |> (flip mergeNewSelectables) linkedSels


{-| merge a list of (new) Selectables into a list of (old) LinkedSelectable wrappers, 
    replacing each oldLinkedSel.selectable with its matching new Selectable (the one
    with the same ID) unless no matching Selectable could be found in the list.
-}
mergeNewSelectables : List Sel.Model -> List LinkedSelectable -> List LinkedSelectable
mergeNewSelectables newSels =
    let replaceSelIfMatchFound linkedSel =
            { linkedSel |
                selectable = 
                    ListEx.find (\s -> s.id == linkedSel.selectable.id) newSels
                        |> Maybe.withDefault linkedSel.selectable
            }
    in 
        List.map replaceSelIfMatchFound


{-| return only the core competencies that should be available / visible
    for the user, given the current state of selected and deselected 
    position categories
-}
availableCompetencies : Model -> List LinkedSelectable
availableCompetencies model =
    let availableCompetencyIds =
            model.positionCategories
                |> List.filter (.isSelected << .selectable)
                |> List.concatMap Data.coreCompDependencies 
                |> ListEx.dropDuplicates
    in
        List.filter 
            (\cc -> List.member cc.selectable.id availableCompetencyIds) 
            model.coreCompetencies


{-| Unwrap the Selectables from a list of LinkedSelectable wrappers
-}
extractSelectables : List LinkedSelectable -> List Sel.Model
extractSelectables =
    List.map .selectable
