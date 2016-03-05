module 
  Data 
    ( Model
    , LinkedSelectable
    , data
    , coreCompDependencies
    , skillDependencies
    ) 
  where

import MultiSelect
import Selectable
import Json.Decode as Decode exposing (Decoder, (:=))
import Debug


-- Data Models

type alias Model =
    { positionCategories : List LinkedSelectable
    , coreCompetencies : List LinkedSelectable
    , skills : List LinkedSelectable
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
    = PositionCategory 
        (List ID) -- dependent core competencies
        (List ID) -- dependent skills
    | CoreCompetency 
        (List ID) -- dependent skills
    | Skill 


type alias ID = Int


-- JSON parsing

json = 
    """
    {
        "positionCategories": [
            {"name": "Front End", "id": 1, "coreCompetencyIds":[1,2], "skillIds":[1, 3]},
            {"name": "Back End", "id": 2, "coreCompetencyIds":[1,3], "skillIds":[2, 4]}
        ],
        "coreCompetencies": [
            {"name": "Javascript", "id": 1, "skillIds":[1, 2]},
            {"name": "Html/CSS", "id": 2, "skillIds":[3]},
            {"name": "Python", "id": 3, "skillIds":[4]}
        ],
        "skills": [
            {"name": "jQuery", "id": 1},
            {"name": "Node.js", "id": 2},
            {"name": "Bootstrap", "id": 3},
            {"name": "Django", "id": 4}
        ]
    }
    """

data : Model
data = 
    parseJson json

parseJson : String -> Model
parseJson json =
    Decode.decodeString modelDecoder json
        |> (\result ->
                case result of
                    Ok m -> m
                    Err e ->
                        let _ = Debug.log "parse error" e in
                            Model [] [] []
            )


modelDecoder : Decoder Model
modelDecoder = 
    Decode.object3 Model posCatsDecoder coreCompsDecoder skillsDecoder

posCatsDecoder : Decoder (List LinkedSelectable)
posCatsDecoder = 
    "positionCategories" := Decode.list posCatDecoder

posCatDecoder : Decoder LinkedSelectable
posCatDecoder = 
    Decode.object3 
        (\sel ccIDs skillIDs -> 
            LinkedSelectable sel <| PositionCategory ccIDs skillIDs
        )
        selectableDecoder 
        ("coreCompetencyIds" := Decode.list Decode.int)
        ("skillIds" := Decode.list Decode.int)
        |> Debug.log "pos cat decoder"

coreCompsDecoder : Decoder (List LinkedSelectable)
coreCompsDecoder = 
    "coreCompetencies" := Decode.list coreCompDecoder

coreCompDecoder : Decoder LinkedSelectable
coreCompDecoder = 
    Decode.object2 
        (\sel skillIDs-> 
            LinkedSelectable sel <| CoreCompetency skillIDs
        ) 
        selectableDecoder
        ("skillIds" := Decode.list Decode.int)    

skillsDecoder : Decoder (List LinkedSelectable)
skillsDecoder =
    "skills" := Decode.list skillDecoder 

skillDecoder : Decoder LinkedSelectable 
skillDecoder =
    Decode.object1
        (\sel -> LinkedSelectable sel Skill)    
        selectableDecoder

selectableDecoder : Decoder Selectable.Model
selectableDecoder = 
    Decode.object2 
        (\id n -> Selectable.init id n False)
        ("id" := Decode.int) 
        ("name" := Decode.string) 


-- HELPERS

coreCompDependencies : LinkedSelectable -> List ID 
coreCompDependencies linkedSel =
    case linkedSel.dependents of 
        PositionCategory coreCompIDs _ -> coreCompIDs 
        _ -> []

skillDependencies : LinkedSelectable -> List ID
skillDependencies linkedSel =
    case linkedSel.dependents of
        PositionCategory _ skillIDs -> skillIDs
        CoreCompetency skillIDs -> skillIDs
        _ -> []
