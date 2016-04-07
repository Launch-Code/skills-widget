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
import Html.Attributes as Attr
import Html.Events as Evnt
import List.Extra as ListEx
import Array

import Selectable as Sel
import Styles


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

update : Action -> Bool -> Model -> Model
update action isMultiple model =
    case action of
        Selectable selectedID selAction ->
            let
              updateItem sel =
                if sel.id == selectedID
                then Sel.update selAction sel
                else sel
              clearSelected sel =
                if sel.isSelected
                then Sel.update selAction sel
                else sel
            in
              if isMultiple
              then List.map updateItem model
              else List.map (updateItem << clearSelected) model

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
  Html.div [ Attr.style Styles.multiSelect ]
    <| List.map (itemView address) model


itemView : Signal.Address Action -> Sel.Model -> Html
itemView address selectable =
    Sel.view
        (Signal.forwardTo address <| Selectable selectable.id)
        selectable
