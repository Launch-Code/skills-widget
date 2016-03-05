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
  let allCapsIfSelected =
    if model.isSelected
      then String.reverse << String.toUpper
      else identity
      classSuffix =
    if model.isSelected
      then "on"
      else "off"
  in
    Html.button
      [ Evnt.onClick (Signal.forwardTo address (\_ -> Toggle)) NoOp
      , Attr.class ("selectable-" ++ classSuffix)
      ]
      [ Html.text (model.name |> allCapsIfSelected) ]
