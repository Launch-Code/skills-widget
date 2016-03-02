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

type alias Model =
    { items : List (ID, Sel.Model) }


init : List Sel.Model -> Model
init items = 
    Model (indexedList items)


type alias ID =
    Int


-- UPDATE

type Action 
    = Selectable ID Sel.Action
    | NoOp


update : Action -> Model -> Model
update action model =
    case action of 
        Selectable selID selAction ->
            let updateItem (id, item) =
                    ( id
                    , if id == selID 
                        then Sel.update selAction item
                        else  item
                    )
            in 
                { model |
                    items = List.map updateItem model.items 
                }

        _ -> 
            model


-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
    Html.div []
        <| List.map (itemView address) model.items


itemView : Signal.Address Action -> (ID, Sel.Model) -> Html
itemView address (id, item) =
    Sel.view 
        (Signal.forwardTo address <| Selectable id)
        item


-- HELPERS

indexedList : List a -> List (ID, a)
indexedList xs =
    let length = Array.length << Array.fromList <| xs
    in 
        ListEx.zip [ 0 .. length - 1] xs