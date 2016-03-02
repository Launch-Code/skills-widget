module 
    SkillsWidget
        ( Model
        , init
        , update
        , view
        )
    where


import Html exposing (Html)
import Html.Attributes as Attr
import Dict exposing (Dict)
import List.Extra as ListEx

import Selectable as Sel
import MultiSelect as MultSel


-- MODEL


type alias Model =
    { positionCategories : MultSel.Model
    , coreCompetencies : MultSel.Model
    }


init : MultSel.Model -> MultSel.Model -> Model
init posCatsMS coreCompsMS =
    Model posCatsMS coreCompsMS


type alias ID = 
    Int


-- UPDATE

type Action
    = PosCats MultSel.Action
    | CoreComps MultSel.Action


update : Action -> Model -> Model
update action model =
    case action of 
        PosCats msAction ->
            let newPosCats = 
                    MultSel.update msAction model.positionCategories
            in 
                { model |
                    positionCategories = newPosCats
                ,   coreCompetencies =
                        updateCoreComps newPosCats model.coreCompetencies
                }

        CoreComps msAction ->
            { model | 
                coreCompetencies =
                    MultSel.update msAction model.coreCompetencies
            }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address { positionCategories, coreCompetencies } =
    let forwardTo = Signal.forwardTo address
    in 
        Html.div []
            [ multiSelectView 
                (forwardTo PosCats) 
                positionCategories
                "Position Categories"
            , multiSelectView
                (forwardTo CoreComps) 
                coreCompetencies
                "Core Cometencies"
            ]



multiSelectView : Signal.Address MultSel.Action
               -> MultSel.Model
               -> String
               -> Html 
multiSelectView msAddress msModel msName =
    Html.div []
        [ Html.h3 [] [ Html.text msName ]
        , MultSel.view msAddress msModel
        ]



-- DATA

coreCompDependencies : Dict String (List String)
coreCompDependencies = 
    Dict.fromList <|
        [ ( "Web", [ "Java", "Javascript", "Elm" ] )
        , ( "Software", [ "Java" ] )
        , ( "LulzSec", [ "Java", "Elm" ] )
        ]


coreCompChildren : List String -> List String
coreCompChildren posCatNames =
    List.concatMap 
        (\pcName -> 
            Maybe.withDefault [] 
                (Dict.get pcName coreCompDependencies)
        )
        posCatNames


updateCoreComps : MultSel.Model -> MultSel.Model -> MultSel.Model
updateCoreComps posCats oldCoreComps =
    let selectedPosCatNames = 
            List.filter .isSelected (unwrapSelectables posCats)
                |> List.map .name
        availableCCNames = 
            coreCompChildren selectedPosCatNames
    in 
        oldCoreComps.items
            |> List.map
                (\(_, selShowHide) ->
                    { selShowHide |
                        isVisible = 
                            List.member 
                                selShowHide.selectable.name 
                                availableCCNames
                    }
                )
            |> MultSel.initWithShowHides


unwrapSelectables : MultSel.Model -> List Sel.Model 
unwrapSelectables msModel =
    msModel.items
        |> List.map (snd >> .selectable)


