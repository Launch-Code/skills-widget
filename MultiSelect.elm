module 
    MultiSelect 
        ( SelectableShowHide
        , Model
        , init
        , initWithShowHides
        , initWithIDs
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

type alias SelectableShowHide =
    { selectable : Sel.Model
    , isVisible : Bool 
    }


type alias Model =
    { items : List (ID, SelectableShowHide) }


init : List Sel.Model -> Model
init selectables = 
    selectables 
        |> List.map (\i -> SelectableShowHide i True)
        |> indexedList
        |> Model


initWithShowHides : List SelectableShowHide -> Model
initWithShowHides showHides =
    showHides
        |> indexedList
        |> Model


initWithIDs : List (ID, SelectableShowHide) -> Model
initWithIDs =
    Model


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
            let updateItem (id, selShowHide) =
                    (,) id <|
                        if id == selID then
                            { selShowHide | 
                                selectable =
                                    Sel.update selAction selShowHide.selectable
                            }
                        else 
                            selShowHide
            in 
                { model |
                    items = List.map updateItem model.items 
                }

        _ -> 
            model


-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
    let onlyVisibles = List.filter (\(_, {isVisible}) -> isVisible)
    in 
        Html.div []
            <| List.map (itemView address) (onlyVisibles model.items)


itemView : Signal.Address Action -> (ID, SelectableShowHide) -> Html
itemView address (id, {selectable}) =
    Sel.view 
        (Signal.forwardTo address <| Selectable id)
        selectable


-- HELPERS

indexedList : List a -> List (ID, a)
indexedList xs =
    let length = Array.length << Array.fromList <| xs
    in 
        ListEx.zip [ 0 .. length - 1] xs