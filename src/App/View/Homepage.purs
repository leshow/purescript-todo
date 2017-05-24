module App.View.Homepage where

import App.Events (Event(..))
import App.State (State(..), Todo(..))
import Control.Bind (discard)
import Data.Array (length)
import Data.Foldable (for_)
import Data.Function (const, ($))
import Data.Monoid (mempty)
import Pux.DOM.Events (onClick, onDoubleClick, onKeyUp)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (focused, key)
import Text.Smolder.HTML (a, button, div, h1, header, input, label, li, section, span, ul)
import Text.Smolder.HTML.Attributes (autofocus, checked, className, for, href, placeholder, type', value)
import Text.Smolder.Markup (text, (!), (#!))
import Prelude hiding (div)


view :: State -> HTML Event
view (State s) =
  section ! className "todoapp" $ do
    header ! className "header" $ do
      h1 $ text "todos" 
      input ! className "new-todo" ! focused ! placeholder "Pux todolist, enter stuff?"
      button ! className "edit" #! onClick (const GetTodos) $ text "Fetch todos"
    section ! className "main" $ do
      input ! className "toggle-all" ! type' "checkbox"
      label ! for "toggle-all" $ text "Mark all as complete"
      ul ! className "todo-list" $ do
        for_ s.todos todoItem

todoItem :: Todo -> HTML Event
todoItem (Todo todo) = 
        li 
          ! className (if todo.completed then "completed" else (if todo.editing then "editing" else ""))
          ! key (show todo.id) $ do
          div ! className "view" $ do
            input ! className "toggle" ! type' "checkbox"
            label #! onDoubleClick (ToggleEdit todo.id) $ text todo.text
            button ! className "destroy" $ mempty
          input ! className "edit" ! value "Some Value" 

itemEditing :: Todo -> HTML Event
itemEditing (Todo todo) = 
  input 
    #! onKeyUp (\ev -> TodoInput todo.id ev)
    ! type' "text"
    ! className "edit"
    ! focused
    ! value todo.text

--   div $ do
--     h1 $ text "Todos"
--     a ! className "github" ! href "#" #! onClick (const GetTodos) $ text "Increment"
--     span $ text (show s.count)
--     a ! className "github" ! href "#" #! onClick (const Decrement) $ text "Decrement"
--     section $ do
--       if ((length s.todos) == 0)
--         then span $ text "No todos found"
--         else
--           ul $ do
--             for_ s.todos item

  
  


