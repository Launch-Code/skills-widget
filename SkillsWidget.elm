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
-- Working with the Data
------------------------

{-| extract selectables from their LinkedSelectable wrappers, 
    update them, 
    then merge the new selectables back into the wrappers
-}
updateLinkedSels : MultSel.Action -> List LinkedSelectable -> List LinkedSelectable
updateLinkedSels msAction linkedSels =
    linkedSels
        |> extractSelectables
        |> MultSel.update msAction 
        |> (flip mergeNewSelectables) linkedSels


mergeNewSelectables : List Sel.Model -> List LinkedSelectable -> List LinkedSelectable
mergeNewSelectables newSels oldLinkedSels =
    oldLinkedSels 
        |> List.map 
            (\linkedSel ->
                { linkedSel |
                    selectable = 
                        ListEx.find (\s -> s.id == linkedSel.selectable.id) newSels
                            |> Maybe.withDefault linkedSel.selectable
                }
            )

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

extractSelectables =
    List.map .selectable
