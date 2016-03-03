module
    MultiSelect
        ( Model
        , init
        , Action
        , update
        , view
        )
    where

import Html exposing (Html)
import Html.Events as Evnt
import List.Extra as ListEx
import Array

import Selectable as Sel


-- MODEL

-- type alias SelectableShowHide =
--     { selectable : Sel.Model
--     , isVisible : Bool
--     }


type alias Model =
    List Sel.Model


init : List Sel.Model -> Model
init selModel = selModel

type alias ID = Int

-- UPDATE
type Action
  = Selectable ID Sel.Action

update : Action -> Model -> Model
update action model =
    case action of
        Selectable selectedID selAction ->
            let updateItem sel =
              if sel.id == selectedID then
                Sel.update selAction sel
              else
                sel
            in
               List.map updateItem model

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
  Html.div []
    <| List.map (itemView address) model


itemView : Signal.Address Action -> Sel.Model -> Html
itemView address selectable =
    Sel.view
        (Signal.forwardTo address <| Selectable selectable.id)
        selectable
