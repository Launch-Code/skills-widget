module
    Selectable
        ( Model
        , init
        , Action
        , update
        , view
        )
    where


import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Evnt
import String
import Graphics.Input as Input

import Styles

-- MODEL

{-| an option that the user can toggle on and off
-}
type alias Model =
    { id : ID
    , name : String
    , isSelected : Bool
    }

type alias ID = Int

init : ID -> String -> Bool -> Model
init =
  Model


-- UPDATE

type Action
    = Toggle
    | NoOp


update : Action -> Model -> Model
update action model =
    case action of
        Toggle ->
            { model |
                isSelected = not model.isSelected
            }

        _ ->
            model

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    let checkbox =
            Html.input 
                [ Attr.type' "checkbox" 
                , Attr.checked model.isSelected
                , Attr.style Styles.checkbox
                ] 
                []
        onClickAddress = 
            Signal.forwardTo address (\_ -> Toggle)
    in
        Html.button 
            [ Evnt.onClick onClickAddress NoOp
            , Attr.style <| Styles.selectable model.isSelected
            ]
            [ --checkbox,
             Html.text model.name
            ]
