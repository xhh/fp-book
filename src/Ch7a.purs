module Ch7a where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Ord (class Ord)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show, discard, (==), ($), (<), (>), (<=), (>=))

data Maybe a = Nothing | Just a

-- instance eqMaybe :: Eq a => Eq (Maybe a) where
--   eq Nothing Nothing = true
--   eq (Just x) (Just y) = x == y
--   eq _ _ = false

derive instance eqMaybe :: Eq a => Eq (Maybe a)

-- instance ordMaybe :: Ord a => Ord (Maybe a) where
--   compare Nothing Nothing = EQ
--   compare Nothing _ = LT
--   compare _ Nothing = GT
--   compare (Just x) (Just y) = compare x y

derive instance ordMaybe :: Ord a => Ord (Maybe a)

-- instance showMaybe :: Show a => Show (Maybe a) where
--   show Nothing = "Nothing"
--   show (Just x) = "(Just " <> show x <> ")"

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

test :: Effect Unit
test = do
  log $ show $ Just 5 == Just 5
  log $ show $ Just 5 == Just 2
  log $ show $ Just 5 == Nothing
  log $ show $ Nothing == Just 5
  log $ show $ Nothing == (Nothing :: Maybe Unit)
  log "------------------"
  log $ show $ Just 1 < Just 5
  log $ show $ Just 5 <= Just 5
  log $ show $ Just 5 > Just 10
  log $ show $ Just 10 >= Just 10
  log $ show $ Just 99 > Nothing
  log $ show $ Just 99 < Nothing
  log $ show $ Just "abc"
  log $ show $ (Nothing :: Maybe Unit)