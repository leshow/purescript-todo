module App.View.Homepage where

import App.Events (Event(..))
import App.Routes (Route(..))
import App.State (State(..), Todo(..))
import Data.Array (filter, length)
import Data.Foldable (for_)
import Data.Monoid (mempty)
import Pux.DOM.Events (onChange, onClick, onDoubleClick, onKeyDown, onKeyUp)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (focused, key)
import Text.Smolder.HTML (a, button, div, footer, h1, header, input, label, li, p, section, span, strong, ul)
import Text.Smolder.HTML.Attributes (checked, className, for, href, placeholder, type', value)
import Text.Smolder.Markup (text, (!), (!?), (#!))
import Prelude hiding (div)


view :: State -> HTML Event
view (State s) =
  let
    activeTodos = filter (\(Todo t) -> not t.completed) s.todos
    showlen = show <<< length
    todoList = case s.route of
                  Completed -> filter (\(Todo t) -> t.completed) s.todos
                  Active -> activeTodos
                  All -> s.todos
                  _ -> s.todos
  in 
  div $ do
    section ! className "todoapp" $ do
      header ! className "header" $ do
        h1 $ text "todos" 
        input ! className "new-todo" ! placeholder "Pux todolist, enter stuff?" #! onKeyDown (\e -> AddTodo e) ! value s.input
      section ! className "main" $ do
        input ! className "toggle-all" ! type' "checkbox" #! onChange (const MarkAll)
        label ! for "toggle-all" $ text "Mark all as complete"
        ul ! className "todo-list" $ do
          for_ todoList todoItem
      footer ! className "footer" $ do
        span ! className "todo-count" $ do
          strong $ text (showlen activeTodos) <> text " item(s) left"
        ul ! className "filters" $ do
          filterLink (s.route == All) "/" "All"
          filterLink (s.route == Active) "/active" "Active"
          filterLink (s.route == Completed) "/completed" "Completed"
    footer ! className "info" $ do
      p $ text "Double click to edit, click below to request some todos"
      a #! onClick (const GetTodos) $ text "FetchTodos"
  
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
 
