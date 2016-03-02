module Main where

import Graphics.Element as Elt exposing (Element)
import Html exposing (Html)
import StartApp.Simple as StartApp

import Selectable as Sel
import MultiSelect as MultSel
import SkillsWidget


main : Signal Html
main = 
    StartApp.start 
        { model = data
        , update = SkillsWidget.update 
        , view = SkillsWidget.view 
        }


-- DATA
-- hard-coded. TODO replace with JSON

data : SkillsWidget.Model
data =
    SkillsWidget.init 
        (MultSel.init pcNames)
        (MultSel.init ccNames)


pcNames : List Sel.Model
pcNames =
    List.map2 Sel.init 
        [ "Web", "Software", "LulzSec" ]
        [ False, True, True ]


ccNames : List Sel.Model 
ccNames =
    List.map2 Sel.init 
        [ "Java", "Javascript", "Elm" ]
        [ True, False, True ]
