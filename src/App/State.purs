module App.State where

import App.Config (config)
import App.Routes (Route, match)
import Control.Applicative (pure)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Prelude (bind)

-- | our State type
newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , count :: Int
  , todos :: Todos
  , fetching :: Boolean
  , error :: String
  }

derive instance genericState :: Generic State _
derive instance newtypeState :: Newtype State _

instance showState :: Show State where show = genericShow

-- | Todos
data Todo = Todo 
  { id :: Int
  , title :: String
  , completed :: Boolean 
  , editing :: Boolean
  , text :: String
  , new :: String
  }
  
type Todos = Array Todo
type TodoId = Int

instance decodeJsonTodo :: DecodeJson Todo where
  decodeJson json = do
        obj <- decodeJson json
        id <- obj .? "id"
        title <- obj .? "title"
        completed <- obj .? "completed"
        pure $ Todo { id, title, completed, text: title, new: title, editing: false }

instance encodeJsonTodo :: EncodeJson Todo where
  encodeJson (Todo todo) = 
    "id" := todo.id
    ~> "title" := todo.title
    ~> "completed" := todo.completed
    ~> "text" := todo.text
    ~> "editing" := todo.editing
    ~> jsonEmptyObject
    

derive instance gTodo :: Generic Todo _
instance showTodo :: Show Todo where show = genericShow

-- decide whether to use argonaut and change the serialize/deserialize to localstorage etc or not
-- currently Data.Foreign is used for serde on app startup, and Argonaut is used for
-- ajax
instance decodeTodo :: Decode Todo where
  decode = genericDecode defaultOptions { unwrapSingleConstructors = true }

instance encodeTodo :: Encode Todo where
  encode = genericEncode defaultOptions { unwrapSingleConstructors = true }

-- | Initial State
init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , count: 0
  , todos: []
  , fetching: false
  , error: ""
  }
