module DataModels where

import Selectable as Sel



type alias PositionCategory =
    { selectable : Sel.Model
    , coreCompetencies : List CoreCompetency
    }


type alias CoreCompetency =
    { selectable : SkillItem }


type alias ID = Int