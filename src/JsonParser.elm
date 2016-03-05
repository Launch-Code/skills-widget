module JsonParser (parseJson) where
import Selectable
import Model exposing (PositionCategory, CoreCompetency, Model)
import Json.Decode as Decode exposing (Decoder, (:=))

-- JSON parsing
exampleJsonString = """
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
