module Model ( Model
             , LinkedSelectable
             , Dependents (Skill, PositionCategory, CoreCompetency)
             , Output
             , coreCompDependencies
             , skillDependencies
             ) where
import Selectable

type alias Model =
  { positionCategories : List LinkedSelectable
  , coreCompetencies : List LinkedSelectable
  , skills : List LinkedSelectable
  }

{-| A Selectable in a network with other "dependent" Selectables who
should only presented as available choices to the user when
this instance is actually in a state of being affirmatively selected.
-}
type alias LinkedSelectable =
  { selectable : Selectable.Model
  , dependents : Dependents
  }

{-| For a given LinkedSelectable instance, keep track of all the other instances
whose availability should depend upon thisInstance.selectable.isSelected == True
-}
type Dependents
  = PositionCategory
    (List ID) -- dependent core competencies
    (List ID) -- dependent skills
  | CoreCompetency
    (List ID) -- dependent skills
  | Skill

{-| A Record that we can supply to out output port.
This will be translated into a POJO for any js subscribed to the port -}
type alias Output =
  { positionCategoryIds: List ID
  , coreCompetencyIds: List ID
  , skillIds: List ID
  }

type alias ID = Int

-- HELPERS
coreCompDependencies : LinkedSelectable -> List ID
coreCompDependencies linkedSel =
  case linkedSel.dependents of
    PositionCategory coreCompIDs _ -> coreCompIDs
    _ -> []

skillDependencies : LinkedSelectable -> List ID
skillDependencies linkedSel =
  case linkedSel.dependents of
    PositionCategory _ skillIDs -> skillIDs
    CoreCompetency skillIDs -> skillIDs
    _ -> []
