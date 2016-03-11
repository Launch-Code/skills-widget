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
    [ ("line-height", "1.75")
    , ("margin-bottom", "20px")
    ]

multiSelect : Styles
multiSelect =
    [ ("min-height", "50px")
    ]


selectable : Bool -> Styles
selectable isSelected =
    [ ("display", "inline-block")
    , ("font-size", "14px")
    , ("padding", "10px")
    , ("margin-bottom", "10px")
    , ("margin-right", "10px")
    , ("border-radius", "4px")
    , ("border-width", "0")
    , ("color", colors.buttonText)
    ]
    ++ if isSelected then selectableOn else []


selectableOn : Styles
selectableOn =
    [ ("background-color", colors.buttonBgSelected)
    , ("border-color", colors.buttonBgSelected)
    , ("color", colors.buttonTextSelected)
    ]


checkbox : Styles
checkbox =
    [ ("margin-right", "4px") ]


horizontalDivider : Styles
horizontalDivider =
    [ ("width", "100%")
    , ("margin-left", "0")
    , ("opacity", "0.5")
    ]

type alias ColorSet =
    { buttonBgUnselected : String
    , buttonBgSelected : String
    , buttonText : String
    , buttonTextSelected : String
    , titleColor : String
    }

colors : ColorSet
colors =
    { buttonBgUnselected = "white"
    , buttonBgSelected = "#5c93ce"
    , buttonText = "#344a5f"
    , buttonTextSelected = "white"
    , titleColor = "#aaa"
    }
