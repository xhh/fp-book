module Ch7b where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)

newtype CSV = CSV String

class ToCSV a where
  toCSV :: a -> CSV

newtype FullName = FullName String
newtype Age = Age Int
data Occupation = Doctor | Dentist | Lawyer | Unemployed
data Person = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  }

derive newtype instance eqCSV :: Eq CSV
derive newtype instance showCSV :: Show CSV

-- derive instance newtypeAge :: Newtype FullName _ 
-- derive newtype instance showFullName :: Show FullName
-- NOTE: The above code would get "Hello" (with double quotes) for `show (FullName "Hello")`.
instance showFullName :: Show FullName where
  show (FullName name) = name

-- Not needed?
-- derive instance newtypeAge :: Newtype Age _ 
derive newtype instance showAge :: Show Age
derive instance genericOccupation :: Generic Occupation _

instance showOccupation :: Show Occupation where
  show = genericShow

instance toCSVPerson :: ToCSV Person where
  toCSV (Person { name, age, occupation }) = CSV $
    show name <> "," <> show age <> "," <> show occupation

class FromCSV a where
  fromCSV :: CSV -> Maybe a

instance fromCSVPerson :: FromCSV Person where
  fromCSV (CSV str) = case split (Pattern ",") str of
    [ name, age, occupation ] -> do
      age' <- fromString age
      occupation' <- toOccupation occupation
      pure $ Person
        { name: FullName name
        , age: Age age'
        , occupation: occupation'
        }
    _ -> Nothing

toOccupation :: String -> Maybe Occupation
toOccupation = case _ of
  "Doctor" -> Just Doctor
  "Dentist" -> Just Dentist
  "Lawyer" -> Just Lawyer
  "Unemployed" -> Just Unemployed
  _ -> Nothing

test :: Effect Unit
test = do
  log str
  log $ show $ str == "Hello,10,Unemployed"
  log $ show $ str == case fromCSV (CSV "Hello,10,Unemployed") of
    Just (p :: Person) -> case toCSV p of
      CSV s -> s
    Nothing -> ""
  where
  person = Person { name, age, occupation }
  name = FullName "Hello"
  age = Age 10
  occupation = Unemployed
  CSV str = toCSV person
