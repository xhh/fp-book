module Main where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "\n------- findLastIndex"
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  log "\n------- reverse"
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show $ reverse (10 : 20 : Nil)
  log $ show $ reverse (10 : Nil)
  log $ show $ reverse (Nil :: List Unit)
  log "\n------- concat"
  log $ show $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
  log "\n------- filter"
  log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log "\n------- catMaybes"
  log $ show $ catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
  log ""

catMaybes :: ∀ a. List (Maybe a) -> List a
catMaybes Nil = Nil
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs
-- catMaybes = reverse <<< go Nil
--   where
--   go acc Nil = acc
--   go acc (Nothing : xs) = go acc xs
--   go acc (Just x : xs) = go (x : acc) xs

filter :: ∀ a. (a -> Boolean) -> List a -> List a
-- filter _ Nil = Nil
-- filter pred (x : xs)
--   | pred x = x : filter pred xs
--   | otherwise = filter pred xs
filter pred l = reverse $ go Nil l
  where
  go acc Nil = acc
  go acc (x : xs) = if pred x then go (x : acc) xs else go acc xs

concat :: ∀ a. List (List a) -> List a
-- concat ll = go Nil ll
--   where
--   go acc Nil = acc
--   go acc (l : ls) = go (go2 (reverse acc) l) ls

--   go2 Nil l = l
--   go2 (x : xs) l = go2 xs (x : l)

concat ll = reverse $ go Nil ll
  where
  go acc Nil = acc
  go acc (l : ls) = go (go2 l acc) ls

  go2 Nil l = l
  go2 (x : xs) l = go2 xs (x : l)

-- concat Nil = Nil
-- concat (Nil : xs) = concat xs
-- concat ((x : xs) : xss) = x : concat (xs : xss)

reverse :: List ~> List
-- reverse Nil = Nil
reverse l = go Nil l
  where
  go acc Nil = acc
  go acc (x : xs) = go (x : acc) xs

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex pred l = go 0 l
  where
  go :: Int -> List a -> Maybe Int
  go _ Nil = Nothing
  go i (x : Nil)
    | pred x = Just i
    | otherwise = Nothing
  -- go i (x : Nil) =
  --   if pred x then
  --     Just i
  --   else
  --     Nothing
  go i (_ : xs) = go (i + 1) xs
