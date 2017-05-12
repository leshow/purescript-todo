module App.Events where

import Prelude
import Data.Foldable
import App.Routes (Route)
import App.State (State(..))
import Data.Function (($))
import Data.Newtype (traverse)
import Data.Traversable (class Traversable, sequenceDefault)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)

data Event 
    = PageView Route
    | Increment
    | Decrement

type AppEffects fx = (ajax :: AJAX | fx)

foldp :: forall fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }

foldp Increment (State st) = noEffects $ State st { count = st.count + 1 }
foldp Decrement (State st) = noEffects $ State st { count = st.count - 1}

