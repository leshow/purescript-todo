module App.View.Layout where

import App.View.Homepage as Homepage
import App.View.NotFound as NotFound
import App.Events (Event)
import App.Routes (Route(..))
import App.State (State(..))
import Pux.DOM.HTML (HTML)

view :: State -> HTML Event
view (State st) =
    case st.route of
      All -> Homepage.view (State st)
      Active -> Homepage.view (State st)
      Completed -> Homepage.view (State st)
      (NotFound url) -> NotFound.view (State st)



