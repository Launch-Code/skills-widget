module Data (Model, CoreCompetency, PositionCategory, data, parseJson) where

import MultiSelect
import Selectable
import Json.Decode as Decode exposing (Decoder, (:=))

-- Data Models
type alias Model =
  { positionCategories : List PositionCategory
  , coreCompetencies : List CoreCompetency
  }

type alias PositionCategory =
  { selectable : Selectable.Model
  , coreCompetencyIds : List ID
  }

type alias CoreCompetency =
  { selectable : Selectable.Model
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
posCatDecoder = Decode.object2 PositionCategory selectableDecoder ("coreCompetencyIds" := Decode.list Decode.int)

coreCompsDecoder : Decoder (List CoreCompetency)
coreCompsDecoder = "coreCompetencies" := Decode.list coreCompDecoder
coreCompDecoder : Decoder CoreCompetency
coreCompDecoder = Decode.object1 CoreCompetency selectableDecoder

selectableDecoder : Decoder Selectable.Model
selectableDecoder = Decode.object2 (\id n -> Selectable.init id n False) ("id" := Decode.int) ("name" := Decode.string) 
