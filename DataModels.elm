module
  DataModels
    ( PositionCategory
    , CoreCompetency
    )
  where

import Selectable as Sel

type alias PositionCategory =
  { id : Int
  , selectable : Sel.Model
  , coreCompetencyIds : List ID
  }


type alias CoreCompetency =
  { id : Int
  , selectable : Sel.Model
  }

type alias ID = Int
