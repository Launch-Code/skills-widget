module Data (Model, data) where

import MultiSelect exposing (SelectableShowHide)
import Json.Decode as Decode exposing (Decoder, (:=))

-- Data Models
type alias Model =
  { positionCategories : List PositionCategory
  , coreCompetencies : List CoreCompetency
  }

type alias PositionCategory =
  { id : ID
  , selectable : SelectableShowHide
  , coreCompetencyIds : List ID
  }

type alias CoreCompetency =
  { id : Int
  , selectable : SelectableShowHide
  }

type alias ID = Int


-- JSON parsing
json = """
  {
    "positionCategories": [
      {"name": "Front End", "id": 1, "coreCompetencyIds":[1,2]},
      {"name": "Back End", "id": 2, "coreCompetencyIds":[1,3]}
   ],
   "coreCompetencies": [
     {"name": "Javascript", "id": 1},
     {"name": "Html/CSS", "id": 2},
     {"name": "Python", "id": 3}
   ]
 }
"""

data : Model
data = parseJson json

parseJson : String -> Model
parseJson json =
  case Decode.decodeString modelDecoder json of
    Ok m -> m
    Err e -> {
      positionCategories = [],
      coreCompetencies = []
    }

modelDecoder : Decoder Model
modelDecoder = Decode.object2 Model posCatsDecoder coreCompsDecoder

posCatsDecoder : Decoder (List PositionCategory)
posCatsDecoder = "positionCategories" := Decode.list posCatDecoder
posCatDecoder : Decoder PositionCategory
posCatDecoder = Decode.object3 PositionCategory ("id" := Decode.int) selectableDecoder ("coreCompetencyIds" := Decode.list Decode.int)

coreCompsDecoder : Decoder (List CoreCompetency)
coreCompsDecoder = "coreCompetencies" := Decode.list coreCompDecoder
coreCompDecoder : Decoder CoreCompetency
coreCompDecoder = Decode.object2 CoreCompetency ("id" := Decode.int) selectableDecoder

selectableDecoder : Decoder SelectableShowHide
selectableDecoder = Decode.object1 (\n -> { selectable = {name = n, isSelected = False}, isVisible = True }) ("name" := Decode.string)
