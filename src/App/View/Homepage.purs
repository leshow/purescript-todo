module App.View.Homepage where

import App.Events (Event(..))
import App.State (State(..), Todo(..))
import Control.Bind (discard)
import Data.Array (length)
import Data.Foldable (for_)
import Data.Function (const, ($))
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (key)
import Text.Smolder.HTML (a, button, div, h1, header, input, label, li, section, span, ul)
import Text.Smolder.HTML.Attributes (autofocus, checked, className, for, href, placeholder, type')
import Text.Smolder.Markup (text, (!), (#!))
import Prelude hiding (div)


view :: State -> HTML Event
view (State s) =
  section ! className "todoapp" $ do
    header ! className "header" $ do
      h1 $ text "todos" 
      input ! className "new-todo" ! placeholder "Pux todolist, enter stuff?"
    section ! className "main" $ do
      input ! className "toggle-all" ! type' "checkbox"
      label ! for "toggle-all" $ text "Mark all as complete"
      ul ! className "todo-list" $ do
        li ! className "completed" $ do
          div ! className "view" $ do
            input ! className "toggle" ! type' "checkbox"
            label $ text "Purescript Pux!"
            button ! className "destroy" $ text "Button"

    
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

-- item :: Todo -> HTML Event
-- item (Todo todo) = 
--   li ! key (show todo.id) $ do
--     label $ text (show todo.title)
--     label $ text (show todo.text) 
  
  


