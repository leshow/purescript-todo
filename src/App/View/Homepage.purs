module App.View.Homepage where

import App.Events (Event(..))
import App.State (State(..), Todo(..))
import Control.Bind (discard)
import Data.Foldable (for_)
import Data.Function (const, ($))
import Prelude (show)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (key)
import Text.Smolder.HTML (a, div, h1, li, section, span, ul)
import Text.Smolder.HTML.Attributes (href, className)
import Text.Smolder.Markup (text, (!), (#!))


view :: State -> HTML Event
view (State s) =
  div do
    h1 $ text "Test"
    a ! className "github" ! href "#" #! onClick (const Increment) $ text "Increment"
    span $ text (show s.count)
    a ! className "github" ! href "#" #! onClick (const Decrement) $ text "Decrement"
    section $ do
      ul $ do
        for_ s.todos item

item :: Todo -> HTML Event
item (Todo todo) = 
  li ! key (show todo.id) $ do
    span $ text (show todo.title)
    span $ text (show todo.text) 
  
  


