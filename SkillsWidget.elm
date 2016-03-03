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
    { model = data
    , update = update
    , view = view
    }

-- MODEL
-- type alias Model =
--   { positionCategories : List Data.PositionCategory
--   , coreCompetencies : List Data.CoreCompetency
--   }

-- init : MultSel.Model -> MultSel.Model -> Model
-- init posCatsMS coreCompsMS =
--     Model posCatsMS coreCompsMS

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
        -- ,  coreCompetencies = updateCoreComps newPosCats model.coreCompetencies
        }

    CoreComps msAction ->
      model
            -- { model |
            --     coreCompetencies =
            --         MultSel.update msAction model.coreCompetencies
            -- }


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

toSelectable =
  List.map .selectable

data : Model
data = Data.data

-- DATA

-- type alias Model =
--   { positionCategories : List PositionCategory
--   , coreCompetencies : List CoreCompetency
--   }

-- pcNames : List Sel.Model
-- pcNames =
--   List.map2 Sel.init
--         [ "Web", "Software", "LulzSec" ]
--         [ False, True, True ]


-- ccNames : List Sel.Model
-- ccNames =
--     List.map2 Sel.init
--         [ "Java", "Javascript", "Elm" ]
--         [ True, False, True ]


-- coreCompDependencies : Dict String (List String)
-- coreCompDependencies = 
--     Dict.fromList <|
--         [ ( "Web", [ "Java", "Javascript", "Elm" ] )
--         , ( "Software", [ "Java" ] )
--         , ( "LulzSec", [ "Java", "Elm" ] )
--         ]


-- coreCompChildren : List String -> List String
-- coreCompChildren posCatNames =
--     List.concatMap 
--         (\pcName -> 
--             Maybe.withDefault [] 
--                 (Dict.get pcName coreCompDependencies)
--         )
--         posCatNames


-- updateCoreComps : MultSel.Model -> MultSel.Model -> MultSel.Model
-- updateCoreComps posCats oldCoreComps =
--     let selectedPosCatNames = 
--             List.filter .isSelected (unwrapSelectables posCats)
--                 |> List.map .name
--         availableCCNames = 
--             coreCompChildren selectedPosCatNames
--     in 
--         oldCoreComps.items
--             |> List.map
--                 (\(_, selShowHide) ->
--                     { selShowHide |
--                         isVisible = List.member selShowHide.selectable.name availableCCNames
--                     }
--                 )
--             |> MultSel.initWithShowHides


-- unwrapSelectables : MultSel.Model -> List Sel.Model 
-- unwrapSelectables msModel =
--     msModel.items
--         |> List.map (snd >> .selectable)

--  Helpers

