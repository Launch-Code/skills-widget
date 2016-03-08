module JsonParser (parseJson, testData, decodeResponse, encode) where
import Selectable
import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Encode as Encode
import Model exposing (Model, LinkedSelectable, Dependents (..))
import Debug

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
            {"name": "Html / CSS", "id": 2, "skillIds":[3]},
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

testData : String
testData = json

-- JSON to Model
parseJson : String -> Model
parseJson json =
    Decode.decodeString modelDecoder json
        |> Result.withDefault (Model [] [] [])


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

-- Model to JSON
encode : Model -> String
encode model =
  Encode.encode 0 (encodeModel model)

encodeModel : Model -> Encode.Value
encodeModel model =
  Encode.object
    -- TODO should we filter out only things that are on here or in the Save action???????
    [ ("positionCategories", linkedSelectablesEncoder <| model.positionCategories)
    , ("coreCompetencies", linkedSelectablesEncoder <| model.coreCompetencies)
    , ("skills", linkedSelectablesEncoder <| model.skills)
    ]

linkedSelectablesEncoder : List LinkedSelectable -> Encode.Value
linkedSelectablesEncoder linkedSelectables =
  Encode.list (List.map linkedSelectableEncoder linkedSelectables)

linkedSelectableEncoder : LinkedSelectable -> Encode.Value
linkedSelectableEncoder linkedSelectable =
  Encode.object
    [ ("id", Encode.int linkedSelectable.selectable.id)
    , ("name", Encode.string linkedSelectable.selectable.name)
    ]


-- Decode server response
decodeResponse : Decoder Int
decodeResponse =
  let debug = Debug.log "some shit" in
  "status" := Decode.int
