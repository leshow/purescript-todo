module App.Events where

import App.Routes (Route)
import App.State (State(..), Todos, Todo(..), TodoId)
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import DOM.Event.KeyboardEvent (eventToKeyboardEvent, key)
import Data.Argonaut (decodeJson)
import Data.Array (filter)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.Events (DOMEvent, targetValue)
import Prelude hiding (div)

data Event 
    = PageView Route
    | GetTodos
    | Decrement
    | ReceiveTodos (Either String Todos)
    | ToggleEdit TodoId DOMEvent
    | TodoInput TodoId DOMEvent
    | ToggleCompleted TodoId DOMEvent
    | MarkAll 
    | DeleteTodo TodoId DOMEvent



type AppEffects fx = (ajax :: AJAX, console :: CONSOLE | fx)

foldp :: forall fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }

foldp GetTodos (State st) = onlyEffects 
    (State st { count = st.count + 1 }) 
    [ do
        res <- attempt $ get "http://jsonplaceholder.typicode.com/users/1/todos"
        let decode r = decodeJson r.response :: Either String Todos
        let todos = either (Left <<< show) decode res
        log (show todos)
        pure (Just $ ReceiveTodos todos)
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

foldp (ReceiveTodos res) (State st) = noEffects $
    case res of
        Right todos -> State st { todos = todos, error = "" }
        Left err -> State st { error = "Error fetching " <> (show err) }

foldp (ToggleEdit id ev) (State st) = noEffects $
    State st 
        { todos = map (\(Todo x) -> 
            if x.id == id 
                then (Todo x { editing = not x.editing, new = x.text }) 
                else (Todo x { editing = false })) 
          st.todos 
        }

foldp (TodoInput id ev) (State st) = noEffects $
    case eventToKey ev of
        "Enter" -> State st 
            { todos = map (\(Todo x) -> 
                if x.id == id
                    then (Todo x { text = x.new, editing = false }) 
                    else Todo x) 
                st.todos
                
            }
        "Escape" -> State st
            { todos = map (\(Todo x) -> 
                if x.id == id 
                    then (Todo x { text = x.text, editing = false })
                    else Todo x)
                st.todos
            }
        _ -> State st
            { todos = map (\(Todo x) -> 
                if x.id == id
                    then (Todo x { new = targetValue ev })
                    else Todo x)
                    st.todos 
            }

foldp (ToggleCompleted id ev) (State st) = noEffects $
    State st
        { todos = map (\(Todo x) -> 
            if x.id == id 
                then (Todo x { completed = not x.completed })
                else Todo x)
            st.todos
        }

foldp MarkAll (State st) = noEffects $
    State st
        { todos = map (\(Todo x) ->
            (Todo x { completed = not x.completed }))
            st.todos
        }

foldp (DeleteTodo id ev) (State st) = noEffects $ State st
    { todos = filter (\(Todo t) -> t.id /= id) st.todos }

eventToKey :: DOMEvent -> String
eventToKey ev = either (const "") key $ runExcept $ eventToKeyboardEvent ev

