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
import Dict 
import List.Extra as ListEx

import Selectable as Sel
import MultiSelect as MultSel


-- MODEL



type alias DependentSelectable =
    { isVisible : Bool 
    , selType : TypeOfSelectable
    , depencies : List Sel.Model 
    }

type TypeOfSelectable 
    = PositionCategory
    | CoreCompetency 


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
            { model |
                positionCategories = 
                    MultSel.update msAction model.positionCategories
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


