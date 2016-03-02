import Debug
import Signal exposing (Address)
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attrs
import StartApp.Simple as StartApp
import Json.Decode as Decode exposing (Decoder, (:=) )

main =
  StartApp.start { model = model, view = view, update = update }


type alias Model = { selected: List (ItemType, ID), pc : List PC, cc : List CC }
type alias PC = { name : String, id : ID }
type alias CC = { name : String, id : ID }
-- type alias SS = { isOn : Bool, name : String, id : ID }
type alias ID = Int
type ItemType = PositionCategory
              | CoreCompetency


decoderCcs : Decoder (List CC)
decoderCcs = "coreCompetencies" := Decode.list decodeCc
decoderPcs : Decoder (List PC)
decoderPcs = "positionCategories" := Decode.list decodePc
decodeCc : Decoder CC
decodeCc = Decode.object2 CC  ("name" := Decode.string) ("id" := Decode.int)
decodePc : Decoder PC
decodePc = Decode.object2 PC  ("name" := Decode.string) ("id" := Decode.int)
decoder : Decoder Model
decoder = Decode.object2 (\pc cc -> {pc=pc, cc=cc, selected=[]}) decoderPcs decoderCcs

model : Model
model =
  case Decode.decodeString decoder jsonData of
    Ok m -> m
    Err e -> {
      pc = [{name = (toString e), id = 1}],
      cc = [],
      selected = [(PositionCategory, 1)]
    }

  -- {pc = [{isOn = True, name = "pos cat 1", id = 1, cc = [{isOn = True, name = "core comp 1", id = 1, ss = []}], ss = []},
  --              {isOn = False, name = "pos cat 2", id = 2, cc = [], ss = []}
  --             ],
  --        cc = [],
  --        ss = []
  --       }

view : Address Action -> Model -> Html
view address model =
-- div : List Attribute -> List Html -> Html
  Html.div [Attrs.class "skill-widget"]
    [(Html.div [Attrs.id "position-category-container"]
        ((Html.p [] [Html.text "Position Categories"]) :: (List.map
          (\i -> Html.span [Attrs.class "position-category", Events.onClick address (PositionCategoryClicked i.id)] [Html.text i.name]) model.pc
        ))
      ),
      (Html.div [Attrs.id "core-competency-container"]
        (List.map
          (\i -> Html.span [Attrs.class "core-competency", Events.onClick address (CoreCompetencyClicked i.id)] [Html.text i.name]) model.cc
        )
      ),
      (Html.div [] [Html.text <| toString model.selected])
    ]

type Action = PositionCategoryClicked ID
            | CoreCompetencyClicked ID

update : Action -> Model -> Model
update action model =
  case action of
    PositionCategoryClicked id ->
      let
        newPc = List.map (\i -> if i.id == id then  i else i) model.pc
        -- newCc = List.foldr (\pc ccs -> List.append pc.cc ccs) [] (List.filter (\pc -> pc.isOn) model.pc)
      in
      { model |
          pc = newPc
          selected = updateSelectedPositionCategories model.selected id
          -- cc = newCc
      }
    CoreCompetencyClicked id ->
      model
updateSelectedPositionCategories : List (ItemType, ID) -> ID -> List (ItemType, ID)
updateSelectedPositionCategories selected id =
  let selectedPcs = List.filter (\s -> fst s == PositionCategory) in



jsonData : String
jsonData = """ {"positionCategories":[{"id": 1, "name": "Front End Web"}],
                "coreCompetencies":[{"id": 1, "name": "Javascript"}]} """
--       {"name":"Front End Web",
--        "id":1,
--        "coreCompetencies":[
--          {
--            "name":"Javascript",
--            "id":1,
--         }
--       ]
--     },
--     {
--       "name":"Back End Web",
--       "id":2,
--       "coreCompetencies":[
--         {
--           "name":"Javascript",
--           "id":2,
--         }
--       ]
--     }
--   ],
--   {"coreCompetencies": []}
-- }
