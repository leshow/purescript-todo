module App.Routes where

import Control.Alt ((<|>))
import Control.Apply ((<*))
import Data.Eq (class Eq)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Function (($))
import Data.Functor ((<$))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromMaybe)
import Data.Show (class Show)
import Pux.Router (end, lit, router)

data Route 
  = All 
  | Active
  | Completed
  | NotFound String

derive instance genericRoute :: Generic Route _

instance eqRoute :: Eq Route where eq = genericEq
instance showRoute :: Show Route where show = genericShow
instance decodeRoute :: Decode Route where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoute :: Encode Route where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  All <$ end
  <|>
  Active <$ (lit "active") <* end
  <|>
  Completed <$ (lit "completed") <* end

toURL :: Route -> String
toURL (NotFound url) = url
toURL All = "/"
toURL Active = "/active" 
toURL Completed = "/completed"
