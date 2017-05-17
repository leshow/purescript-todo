module App.View.Homepage where

import Prelude (show)
import App.Events (Event(..))
import App.State (State(..))
import Control.Bind (discard)
import Data.Function (($), const)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML (a, div, h1, span)
import Text.Smolder.HTML.Attributes (href, className)
import Text.Smolder.Markup (text, (!), (#!))


view :: State -> HTML Event
view (State s) =
  div do
    h1 $ text "Test"
    a ! className "github" ! href "#" #! onClick (const Increment) $ text "Increment"
    span $ text (show s.count)
    a ! className "github" ! href "#" #! onClick (const Decrement) $ text "Decrement"
