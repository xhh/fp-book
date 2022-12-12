module Ch6 where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)

test :: Effect Unit
test = logShow $ a == b
  where
  a = Person { name: "a", age: 1, address: addr }
  b = Person { name: "a", age: 1, address: addr }
  addr = Address { street1: "", street2: "", city: "", state: "", zip: "" }

data Address = Address
  { street1 :: String
  , street2 :: String
  , city :: String
  , state :: String
  , zip :: String
  }

data Person = Person
  { name :: String
  , age :: Int
  , address :: Address
  }

instance eqPerson :: Eq Person where
  -- eq (Person p1) (Person p2) = p1.name == p2.name && p1.age == p2.age
  eq (Person p1) (Person p2) = p1 == p2

instance eqAddress :: Eq Address where
  eq (Address a1) (Address a2) = a1 == a2
