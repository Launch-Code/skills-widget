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


{-| a MultiSelect Model along with a list indicating which items should
    be visible and which should be hidden
-}
type alias MultiSelectShowHide =
    { multiSelect : MultSel.Model
    , visibilities : List (ID, Bool)
    }


type alias Model =
    { positionCategories : MultiSelectShowHide
    , coreCompetencies : MultiSelectShowHide
    }


init : MultSel.Model -> MultSel.Model -> Model
init posCatsMS coreCompsMS =
    Model (withShowHide posCatsMS) (withShowHide coreCompsMS)


withShowHide : MultSel.Model -> MultiSelectShowHide
withShowHide ms =
    MultiSelectShowHide ms <|
        List.map (\(id, _) -> (id, True)) ms.items


type alias ID = 
    Int


-- UPDATE

type Action
    = PosCats MultSel.Action
    | CoreComps MultSel.Action


update : Action -> Model -> Model
update action model =
    let updateMS msAction msShowHide =
            { msShowHide |
                multiSelect =
                    MultSel.update msAction msShowHide.multiSelect
            }
    in
    case action of 
        PosCats msAction ->
            { model |
                positionCategories = 
                    updateMS msAction model.positionCategories
            }

        CoreComps msAction ->
            { model | 
                coreCompetencies =
                    updateMS msAction model.coreCompetencies
            }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address { positionCategories, coreCompetencies } =
    let forwardTo = Signal.forwardTo address
    in 
        Html.div []
            [ multiSelectView 
                (forwardTo PosCats) 
                (positionCategories |> onlyVisibles)
                "Position Categories"
            , multiSelectView
                (forwardTo CoreComps) 
                (coreCompetencies |> onlyVisibles)
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


onlyVisibles : MultiSelectShowHide -> MultSel.Model
onlyVisibles { multiSelect, visibilities } =
    { multiSelect |
        items = 
            ListEx.zip multiSelect.items visibilities
                |> List.filter 
                    ( \(_, (_, isVisible)) -> isVisible )
                |> List.map fst
    }


-- HELPERS


