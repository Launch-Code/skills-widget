module SkillsWidget where

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Dict exposing (Dict)
import List.Extra as ListEx
import StartApp

import MultiSelect as MultSel
import JsonParser
import Json.Decode as Decode
import Selectable
import Model exposing (Model, LinkedSelectable, Output)
import Styles
import Effects exposing (Effects)

import Debug

-- Initialization
main : Signal Html
main =
  app.html

app : StartApp.App Model
app =
  StartApp.start
    { init = (JsonParser.parseJson JsonParser.testData) |> setInitialSelected |> noEffects
    , update = update
    , view = view
    , inputs = []
    }

port jsonData : String
port selected : Output
port output : Signal Output
port output = Signal.map (outputModel) app.model

outputModel : Model -> Output
outputModel model =
  let filterIsOn =  List.filter (.isSelected << .selectable)
      collectIds =  List.map (.id << .selectable)
      selectedPosCatIds = collectIds <| filterIsOn <| model.positionCategories
      selectedCoreCompIds = collectIds <| filterIsOn <| availableCompetencies <| model
      selectedSkillIds = collectIds <| filterIsOn <| availableSkills <| model
  in
    { positionCategoryIds = selectedPosCatIds
    , coreCompetencyIds = selectedCoreCompIds
    , skillIds = selectedSkillIds
    }

setInitialSelected : Model -> Model
setInitialSelected model =
  let turnOnLinkedSelectable linkedSel =
        let sel = linkedSel.selectable
            updatedSel = { sel | isSelected = True }
        in
          { linkedSel | selectable = updatedSel }
  in
  { model |
      positionCategories = List.map (\s -> if List.member s.selectable.id selected.positionCategoryIds
                                          then turnOnLinkedSelectable s
                                          else s
                                   ) model.positionCategories
  }


-- UPDATE

type Action
    = PosCats MultSel.Action
    | CoreComps MultSel.Action
    | Skills MultSel.Action

update : Action -> Model -> (Model, Effects Action)
update action model =
  noEffects <|
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

-- when we don't care about the effects.
-- This method applies a empty effect to all out our actions
-- making the the return value be (Model, Effects Action) instead of just Model
-- We need to use effects here because we are creating a side effect when we update our outgoing port
noEffects : a -> (a, Effects Action)
noEffects =
  flip (,) Effects.none

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
