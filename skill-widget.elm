import Debug
import Signal exposing (Address)
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attrs
import StartApp.Simple as StartApp

main =
  StartApp.start { model = model, view = view, update = update }

type alias Model = { pc : List PC , cc : List CC, ss: List SS }
type alias PC = { isOn : Bool , name : String, id : ID, cc: List CC, ss: List SS }
type alias CC = { isOn : Bool , name : String, id : ID, ss: List SS }
type alias SS = { isOn : Bool , name : String, id : ID }
type alias ID = Int

model : Model
model = {pc = [{isOn = True, name = "pos cat 1", id = 1, cc = [{isOn = True, name = "core comp 1", id = 1, ss = []}], ss = []},
               {isOn = False, name = "pos cat 2", id = 2, cc = [], ss = []}
              ],
         cc = [],
         ss = []
        }

view : Address Action -> Model -> Html
view address model =
-- div : List Attribute -> List Html -> Html
  Html.div [Attrs.class "skill-widget"]
    [(Html.div [Attrs.id "position-category-container"]
        ((Html.p [] [Html.text "Position Categories"]) :: (List.map
          (\i -> Html.span [Attrs.class "position-category", Events.onClick address (PositionCategoryClicked i.id)] [Html.text <| toString i.name]) model.pc
        ))
      ),
      (Html.div [Attrs.id "core-competency-container"]
        (List.map
          (\i -> Html.span [Attrs.class "core-competency", Events.onClick address (CoreCompetencyClicked i.id)] [Html.text i.name]) model.cc
        )
      ),
      (Html.div [Attrs.id "skill-container"]
        (List.map
          (\i -> Html.span [Attrs.class "skill", Events.onClick address (SkillClicked i.id)] [Html.text i.name]) model.ss
        )
      )
    ]

type Action = PositionCategoryClicked ID
            | CoreCompetencyClicked ID
            | SkillClicked ID

update : Action -> Model -> Model
update action model =
  case action of
    PositionCategoryClicked id ->
      let
        newPc = List.map (\i -> if i.id == id then { i | isOn = not i.isOn } else i) model.pc
        newCc = List.foldr (\pc ccs -> List.append pc.cc ccs) [] (List.filter (\pc -> pc.isOn) model.pc)
      in
      { model |
          pc = newPc,
          cc = newCc
      }
    CoreCompetencyClicked id ->
      model
    SkillClicked id ->
      model
--        -- List.foldr updatePcs model model.pc

-- updatePcs : PC -> Model -> Model
-- updatePcs item model =
--   if item.isOn
--   then
--     { model |
--         cc = uniqueAppend item.cc model.cc,
--         ss = uniqueAppend item.ss model.ss
--     }
--   else model

-- -- uniqueAppend : List a -> List a -> List a

