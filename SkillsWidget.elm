module SkillsWidget where

import Html exposing (Html)
import Html.Attributes as Attr
import Dict exposing (Dict)
import List.Extra as ListEx
import StartApp.Simple as StartApp

import Selectable as Sel
import MultiSelect as MultSel
import Data exposing (Model)

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
      let newPosCats = MultSel.update msAction (toSelectableShowHide model.positionCategories)
      in
        { model |
          positionCategories = newPosCats
        -- ,  coreCompetencies = updateCoreComps newPosCats model.coreCompetencies
        }

    CoreComps msAction ->
      model
            -- { model |
            --     coreCompetencies =
            --         MultSel.update msAction model.coreCompetencies
            -- }


-- VIEW
view : Signal.Address Action -> Model -> Html
view address { positionCategories, coreCompetencies } =
    let forwardTo = Signal.forwardTo address
    in
      Html.div []
        [ multiSelectView
          (forwardTo PosCats)
          (toSelectableShowHide positionCategories)
          "Position Categories"
          , multiSelectView
          (forwardTo CoreComps)
          (toSelectableShowHide coreCompetencies)
          "Core Compet tencies"
        ]

multiSelectView : Signal.Address MultSel.Action -> MultSel.Model -> String -> Html
multiSelectView msAddress msModel msName =
    Html.div []
        [ Html.h3 [] [ Html.text msName ]
        , MultSel.view msAddress msModel
        ]

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
toSelectableShowHide arr =
  { items = (List.map (\i -> (i.id, i.selectable)) arr) }
