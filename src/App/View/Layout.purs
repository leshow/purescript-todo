module App.View.Layout where

import App.View.Homepage as Homepage
import App.View.NotFound as NotFound
import App.Events (Event)
import App.Routes (Route(..))
import App.State (State(..))
import CSS (CSS, fromString, (?), fontSize, display, inlineBlock, marginTop, marginRight, marginLeft, px, value, key, color, backgroundColor, padding, borderRadius)
import CSS.Border (border, solid)
import CSS.Text (textDecoration, noneTextDecoration, letterSpacing)
import CSS.Text.Transform (textTransform, uppercase)
import CSS.TextAlign (center, textAlign)
import Color (rgb)
import Control.Bind (discard)
import Data.Function (($), (#))
import Data.Monoid (mempty)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!))

view :: State -> HTML Event
view (State st) =
    case st.route of
      (Home) -> Homepage.view (State st)
      (NotFound url) -> NotFound.view (State st)
      (Todos tid) -> Homepage.view (State st) -- these bottom 2 routes will change
      (Todo tid) -> Homepage.view (State st)



