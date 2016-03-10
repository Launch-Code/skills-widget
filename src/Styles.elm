module Styles where

import Color exposing (..)
import Html exposing (Attribute)
import Html.Attributes exposing (style)


type alias Styles = List (String, String)


skillsWidget: Styles
skillsWidget =
    [ ("padding", "20px")
    ]


multiSelectContainer : Styles 
multiSelectContainer =
    [ ("margin-top", "6px")
    , ("color", colors.titleColor)
    , ("padding-top", "6px")
    , ("padding-left", "6px")
    , ("padding", "6px")
    , ("align", "left")
    ]

multiSelectHeading : Styles
multiSelectHeading =
    [ ("margin-top", "12px")
    ]

multiSelect : Styles
multiSelect =
    [ ("min-height", "50px")
    ]


selectable : Bool -> Styles
selectable isSelected =
    [ ("display", "inline-block")
    , ("font-size", "14px")
    , ("padding", "12px")
    , ("margin-bottom", "10px")
    , ("margin-right", "8px")
    , ("border-radius", "2px")
    , ("border-width", "0")
    , ("color", colors.buttonText)
    , ("background-color", colors.buttonBG)
    ]
    ++ if isSelected then selectableOn else []


selectableOn : Styles
selectableOn =
    [ ("background-color", colors.buttonSelected)
    , ("border-color", colors.buttonSelected)
    , ("border-bottom", "2px solid #5c93ce")
    ]


checkbox : Styles
checkbox =
    [ ("margin-right", "4px") ]


horizontalDivider : Styles
horizontalDivider =
    [ ("width", "50%")
    , ("margin-left", "0")
    , ("opacity", "0.5")
    ]

type alias ColorSet =
    { buttonBG : String
    , buttonSelected : String
    , buttonText : String
    , titleColor : String
    }

colors : ColorSet
colors =
    { buttonBG = "white"
    , buttonSelected = "#5c93ce"
    , buttonText = "#344a5f"
    , titleColor = "#aaa"
    }
