module App.Events where

import Prelude
import App.Routes (Route)
import App.State (State(..), Todos)
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get)
import Pux (EffModel, noEffects, onlyEffects)
import Data.Argonaut (decodeJson)

data Event 
    = PageView Route
    | Increment
    | Decrement

type AppEffects fx = (ajax :: AJAX, console :: CONSOLE | fx)

foldp :: forall fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }

foldp Increment (State st) = onlyEffects 
    (State st { count = st.count + 1 }) 
    [ do
        res <- attempt $ get "http://jsonplaceholder.typicode.com/users/1/todos"
        let decode r = decodeJson r.response :: Either String Todos
        let todos = either (Left <<< show) decode res
        log (show todos)
        pure Nothing
        -- pure (Just $ Todos todos)
    ]

foldp Decrement (State st) = 
    let
        newState = State st { count = st.count - 1 }
    in 
        onlyEffects 
            (State st { count = st.count - 1}) [ do
                log (show newState)
                log "decrement" 
                pure Nothing 
         ]


