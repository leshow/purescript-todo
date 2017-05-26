module App.View.Homepage where

import App.Events (Event(..))
import App.Routes (Route(..))
import App.State (State(..), Todo(..))
import Data.Array (length)
import Data.Foldable (for_)
import Data.Monoid (mempty)
import Pux.DOM.Events (onChange, onClick, onDoubleClick, onKeyUp)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (focused, key)
import Text.Smolder.HTML (a, button, div, footer, h1, header, input, label, li, section, span, strong, ul)
import Text.Smolder.HTML.Attributes (checked, className, for, href, placeholder, type', value)
import Text.Smolder.Markup (text, (!), (!?), (#!))
import Prelude hiding (div)


view :: State -> HTML Event
view (State s) =
  section ! className "todoapp" $ do
    header ! className "header" $ do
      h1 $ text "todos" 
      input ! className "new-todo" ! placeholder "Pux todolist, enter stuff?"
      -- button ! className "edit" #! onClick (const GetTodos) $ text "Fetch todos"
    section ! className "main" $ do
      input ! className "toggle-all" ! type' "checkbox" #! onClick (const MarkAll)
      label ! for "toggle-all" $ text "Mark all as complete"
      ul ! className "todo-list" $ do
        for_ s.todos todoItem
    footer ! className "footer" $ do
      span ! className "todo-count" $ do
        strong $ text (show (length s.todos)) <> text " item left"
      ul ! className "filters" $ do
        filterLink (s.route == All) "/" "All"
        filterLink (s.route == Active) "/active" "Active"
        filterLink (s.route == Completed) "/completed" "Completed"
  
todoItem :: Todo -> HTML Event
todoItem (Todo todo) = 
        ((li 
          !? todo.completed) (className "completed") !? todo.editing) (className "editing")
          ! key (show todo.id) $ do
            if todo.editing 
              then itemEditing (Todo todo)
              else itemView (Todo todo)

itemEditing :: Todo -> HTML Event
itemEditing (Todo todo) = 
  input 
    #! onKeyUp (\ev -> TodoInput todo.id ev)
    ! type' "text"
    ! className "edit"
    ! focused
    ! value todo.new

itemView :: Todo -> HTML Event
itemView (Todo todo) = 
      div ! className "view" $ do
        (input
            !? todo.completed) (checked "checked")
            ! className "toggle" 
            ! type' "checkbox" 
            #! onChange (ToggleCompleted todo.id)
        label #! onDoubleClick (ToggleEdit todo.id) $ text todo.text
        button ! className "destroy" #! onClick (DeleteTodo todo.id) $ mempty

filterLink :: Boolean -> String -> String -> HTML Event 
filterLink selected route name = 
  li $ (a !? selected) (className "selected") ! href route #! onClick (ChangeRoute route) $ text name
 
