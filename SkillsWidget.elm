module SkillsWidget where

import Html exposing (Html)
import Html.Attributes as Attr
import Dict exposing (Dict)
import List.Extra as ListEx
import StartApp.Simple as StartApp

import Selectable as Sel
import MultiSelect as MultSel
import Data exposing (Model, CoreCompetency, PositionCategory)

main : Signal Html
main =
  StartApp.start
    { model = Data.parseJson jsonData
    , update = update
    , view = view
    }

port jsonData : String

type alias ID = Int

-- UPDATE

type Action
    = PosCats MultSel.Action
    | CoreComps MultSel.Action

update : Action -> Model -> Model
update action model =
  case action of
    PosCats msAction ->
      let newSelectables = MultSel.update msAction (toSelectable model.positionCategories)
      in
        { model |
          positionCategories = mergeNewSelectables newSelectables model.positionCategories
        }

    CoreComps msAction ->
      model

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
    let forwardTo = Signal.forwardTo address
    in
      Html.div []
        [ multiSelectView
          (forwardTo PosCats)
          (toSelectable model.positionCategories)
          "Position Categories"
          , multiSelectView
          (forwardTo CoreComps)
          (toSelectable <| availableCompetencies model)
          "Core Competencies"
        ]

multiSelectView : Signal.Address MultSel.Action -> MultSel.Model -> String -> Html
multiSelectView msAddress msModel msName =
    Html.div []
        [ Html.h3 [] [ Html.text msName ]
        , MultSel.view msAddress msModel
        ]

availableCompetencies : Model -> List CoreCompetency
availableCompetencies model =
  let selectedPositionCategories = List.filter (.isSelected << .selectable) model.positionCategories
      availableCompetencyIds =
        ListEx.dropDuplicates <|
              List.concatMap .coreCompetencyIds selectedPositionCategories
  in
    List.filter (\cc -> List.member cc.selectable.id availableCompetencyIds) model.coreCompetencies


-- Helpers

-- mergeNewSelectables : List Sel.Model -> List {selectable:Sel.Model} -> List {selectable:Sel.Model}
mergeNewSelectables newSels oldSelWrappers =
  List.map (\selWrapper ->
    let maybeNewSel = ListEx.find (\s -> s.id == selWrapper.selectable.id) newSels
        updatedSel =
          case maybeNewSel of
            Just newSel -> newSel
            Nothing -> selWrapper.selectable
          in
            { selWrapper |
              selectable = updatedSel
            }
  ) oldSelWrappers

toSelectable =
  List.map .selectable
