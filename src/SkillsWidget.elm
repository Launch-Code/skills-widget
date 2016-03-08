module SkillsWidget where

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Dict exposing (Dict)
import List.Extra as ListEx
import StartApp.Simple as StartApp

import MultiSelect as MultSel
import JsonParser
import Json.Decode as Decode
import Selectable
import Model exposing (Model, LinkedSelectable)
import Styles
-- import Effects exposing (Effects)
-- import Http
-- import Task

import Debug

-- Initialization
main : Signal Html
-- main = app.html

main =
  StartApp.start
    { model = model
    , update = update
    , view = view
    }

-- init : String -> (Model, Effects Action)
-- init json = (JsonParser.parseJson json, Effects.none)
model : Model
model = JsonParser.parseJson JsonParser.testData --jsonData

port jsonData : String
port output : Signal String
port output = Signal.map JsonParser.encode signalOfModel
signalOfModel : Signal Model
signalOfModel =
-- UPDATE

type Action
    = PosCats MultSel.Action
    | CoreComps MultSel.Action
    | Skills MultSel.Action

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
        Skills msAction ->
            { model |
                skills =
                    updateLinkedSels msAction model.skills
            }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    Html.div
        [ Attr.style Styles.skillsWidget ]
        [ multiSelectView
            (Signal.forwardTo address PosCats)
            (extractSelectables model.positionCategories)
            "Position Categories"
        , Html.hr [ Attr.style Styles.horizontalDivider ] []
        , multiSelectView
            (Signal.forwardTo address CoreComps)
            (extractSelectables <| availableCompetencies model)
            "Core Competencies"
        , Html.hr [ Attr.style Styles.horizontalDivider ] []
        , multiSelectView
            (Signal.forwardTo address Skills)
            (extractSelectables <| availableSkills model)
            "Skills"
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


{-| merge a list of (new) Selectables into a list of (old) LinkedSeletable wrappers,
    replacing each oldLinkedSel.selectable with its matching new Selectable (the one
    with the same ID) unless no matching Selectable could be found in the list.
-}
mergeNewSelectables : List Selectable.Model -> List LinkedSelectable -> List LinkedSelectable
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
                |> currentlySelected
                |> List.concatMap Model.coreCompDependencies
                |> ListEx.dropDuplicates
    in
        List.filter
            (\cc -> List.member cc.selectable.id availableCompetencyIds) 
            model.coreCompetencies


{-| return only the skills that should be available / visible
    for the user, given the current state of selected and deselected 
    position categories and core competencies
-}
availableSkills : Model -> List LinkedSelectable
availableSkills model =
    let availableIDs parents =
            parents
                |> currentlySelected
                |> List.concatMap Model.skillDependencies
                |> ListEx.dropDuplicates
        idsFromPosCats =
            availableIDs model.positionCategories
        idsFromCoreComps =
            availableIDs model.coreCompetencies
        isMemberOfBoth id =
            List.member id idsFromPosCats && List.member id idsFromCoreComps
    in
        List.filter
            (isMemberOfBoth << .id << .selectable)
            model.skills

extractSelectables : List LinkedSelectable -> List Selectable.Model
extractSelectables = List.map .selectable

currentlySelected : List LinkedSelectable -> List LinkedSelectable
currentlySelected = List.filter (.isSelected << .selectable)
