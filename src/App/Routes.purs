module App.Routes where

import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Function (($))
import Data.Functor ((<$), (<$>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Pux.Router (end, int, lit, param, router)

data Route = 
  Home 
  | Todos String
  | Todo Int
  | NotFound String

derive instance genericRoute :: Generic Route _
instance showRoute :: Show Route where show = genericShow
instance decodeRoute :: Decode Route where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoute :: Encode Route where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  Home <$ end
  <|>
  Todos <$> (lit "todos" *> param "view") <* end
  <|>
  Todo <$> (lit "todos" *> int) <* end

toURL :: Route -> String
toURL (NotFound url) = url
toURL Home = "/"
toURL (Todo todoid) = "/todos/" <> show todoid
toURL (Todos view) = "todos?view=" <> (show view)
