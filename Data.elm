module 
  Data 
    ( Model
    , LinkedSelectable
    , data
    , coreCompDependencies
    ) 
  where

import MultiSelect
import Selectable
import Json.Decode as Decode exposing (Decoder, (:=))



-- Data Models

type alias Model =
    { positionCategories : List LinkedSelectable
    , coreCompetencies : List LinkedSelectable
    }


{-| A Selectable in a network with other "dependent" Selectables who
    should only presented as available choices to the user when 
    this instance is actually in a state of being affirmatively selected.
-}
type alias LinkedSelectable =
    { selectable : Selectable.Model 
    , dependents : Dependents 
    }


{-| For a given LinkedSelectable instance, keep track of all the other instances 
    whose availability should depend upon thisInstance.selectable.isSelected == True
-}
type Dependents
    = PositionCategory (List ID) 
    | CoreCompetency 


type alias ID = Int


-- JSON parsing

json = 
    """
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
data = 
    parseJson json

parseJson : String -> Model
parseJson json =
    case Decode.decodeString modelDecoder json of
        Ok m -> m
        Err e -> 
            { positionCategories = []
            , coreCompetencies = []
            }

modelDecoder : Decoder Model
modelDecoder = 
  Decode.object2 Model posCatsDecoder coreCompsDecoder

posCatsDecoder : Decoder (List LinkedSelectable)
posCatsDecoder = 
  "positionCategories" := Decode.list posCatDecoder
posCatDecoder : Decoder LinkedSelectable
posCatDecoder = 
    Decode.object2 
        (\s ccIDs -> LinkedSelectable s <| PositionCategory ccIDs)
        selectableDecoder 
        ("coreCompetencyIds" := Decode.list Decode.int)

coreCompsDecoder : Decoder (List LinkedSelectable)
coreCompsDecoder = 
  "coreCompetencies" := Decode.list coreCompDecoder
coreCompDecoder : Decoder LinkedSelectable
coreCompDecoder = 
  Decode.object1 (\s -> LinkedSelectable s CoreCompetency) selectableDecoder

selectableDecoder : Decoder Selectable.Model
selectableDecoder = 
  Decode.object2 (\id n -> Selectable.init id n False) ("id" := Decode.int) ("name" := Decode.string) 



-- HELPERS

coreCompDependencies : LinkedSelectable -> List ID 
coreCompDependencies linkedSel =
    case linkedSel.dependents of 
        PositionCategory coreCompIDs ->
            coreCompIDs 
        _ ->
            []