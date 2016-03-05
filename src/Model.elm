module Model (Model, PositionCategory, CoreCompetency) where
import Selectable

type alias Model =
  { positionCategories : List PositionCategory
  , coreCompetencies : List CoreCompetency
  }

type alias PositionCategory =
  { selectable : Selectable.Model
  , coreCompetencyIds : List ID
  }

type alias CoreCompetency =
  { selectable : Selectable.Model }

type alias ID = Int
