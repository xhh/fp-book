module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, negate, otherwise, show, (#), ($), (+), (-), (<), (<=), (==), (>), (>>>))

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
  log $ show $ singleton "xyz"
  log $ show $ null Nil
  log $ show $ null ("abc" : Nil)
  log $ show $ snoc (1 : 2 : Nil) 3
  log $ show $ length $ 1 : 2 : 3 : Nil
  log $ show (head Nil :: Maybe Unit)
  log $ show $ head ("abc" : "123" : Nil)
  log $ show $ tail (Nil :: List Unit)
  log $ show $ tail ("abc" : "123" : Nil)

  log "\n------- last"
  log $ show $ (last Nil :: Maybe Unit)
  log $ show $ last ("a" : "b" : "c" : Nil)

  log "\n------- init"
  log $ show $ init (Nil :: List Unit)
  log $ show $ init (1 : Nil)
  log $ show $ init (1 : 2 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)

  log "\n------- uncons"
  log $ show $ uncons (1 : 2 : 3 : Nil)

  log "\n------- index"
  log $ show $ index (1 : Nil) 4
  log $ show $ index (1 : 2 : 3 : Nil) 1
  log $ show $ index (Nil :: List Unit) 0
  log $ show $ (1 : 2 : 3 : Nil) !! 1
  log $ show $ flip index 0 (1 : 2 : 3 : Nil)

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

  log "\n------- range"
  log $ show $ range 1 10
  log $ show $ range 3 (-3)

  log "\n------- take"
  log $ show $ take 5 (12 : 13 : 14 : Nil)
  log $ show $ take 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil)

  log "\n------- drop"
  log $ show $ drop 2 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil)
  log $ show $ drop 10 (Nil :: List Unit)

  log "\n------- takeWhile"
  log $ show $ takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ takeWhile (_ == -17) (1 : 2 : 3 : Nil)

  log "\n------- dropWhile"
  log $ show $ dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ dropWhile (_ == -17) (1 : 2 : 3 : Nil)

  log "\n----- takeEnd"
  log $ show $ takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ takeEnd 10 (1 : Nil)

  log "\n------- dropEnd"
  log $ show $ dropEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ dropEnd 10 (1 : Nil)

  log "\n------- zip"
  log $ show $ zip (1 : 2 : 3 : Nil) ("a" : "b" : "c" : "d" : "e" : Nil)
  log $ show $ zip ("a" : "b" : "c" : "d" : "e" : Nil) (1 : 2 : 3 : Nil)
  log $ show $ zip (Nil :: List Unit) (1 : 2 : Nil)

  log "\n------- unzip"
  log $ show $ unzip (Tuple 1 "a" : Tuple 2 "b" : Tuple 3 "c" : Nil)
  log $ show $ unzip (Tuple "a" 1 : Tuple "b" 2 : Tuple "c" 3 : Nil)
  log $ show $ unzip (Nil :: List (Tuple Unit Unit))

  log ""

unzip :: ∀ a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip Nil = Tuple Nil Nil
unzip (Tuple x y : xys) = Tuple (x : xs) (y : ys)
  where
  Tuple xs ys = unzip xys

zip :: ∀ a b. List a -> List b -> List (Tuple a b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (x : xs) (y : ys) = Tuple x y : zip xs ys

dropEnd :: ∀ a. Int -> List a -> List a
dropEnd n = go >>> snd
  where
  go Nil = Tuple 0 Nil
  go (x : xs) = Tuple (n' + 1) l'
    where
    Tuple n' l = go xs
    l' = if n' < n then l else (x : l)

takeEnd :: ∀ a. Int -> List a -> List a
-- takeEnd _ Nil = Nil
-- takeEnd n l
--   | n <= 0 = Nil
--   -- | otherwise = l # reverse # take n # reverse
--   | otherwise = drop (length l - n) l
takeEnd n = go >>> snd
  where
  go Nil = Tuple 0 Nil
  go (x : xs) = Tuple (n' + 1) l'
    where
    Tuple n' l = go xs
    l' = if n' < n then (x : l) else l

dropWhile :: ∀ a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
-- dropWhile pred (x : xs) = if pred x then dropWhile pred xs else (x : xs)
dropWhile pred l@(x : xs) = if pred x then dropWhile pred xs else l

takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile pred (x : xs) = if pred x then x : takeWhile pred xs else Nil

drop :: ∀ a. Int -> List a -> List a
drop _ Nil = Nil
drop n l | n <= 0 = l
drop n (_ : xs) = drop (n - 1) xs

take :: ∀ a. Int -> List a -> List a
take _ Nil = Nil
take n _ | n <= 0 = Nil
take n (x : xs) = x : take (n - 1) xs

-- -- go Nil (max 0 n) l
-- take n l = reverse $ go Nil n l
--   where
--   go acc _ Nil = acc
--   go acc n' _ | n' <= 0 = acc
--   go acc n' (x : xs) = go (x : acc) (n' - 1) xs

range :: Int -> Int -> List Int
range a b = go Nil b
  where
  go acc n
    | n == a = (a : acc)
    | otherwise = go (n : acc) (n - step)
  step = if a <= b then 1 else -1

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

infixl 8 index as !!

index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
index (x : xs) i
  | i < 0 = Nothing
  | i == 0 = Just x
  | otherwise = index xs (i - 1)

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (x : xs) = Just { head: x, tail: xs }

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
init (_ : Nil) = Just Nil
-- init (x : xs) = Just (x : (unwrap? $ init xs))
init (x : xs) = case init xs of
  Nothing -> Nothing -- will never be executed
  Just xs2 -> Just (x : xs2)

last :: ∀ a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

head :: ∀ a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

length :: ∀ a. List a -> Int
length l = go 0 l
  where
  go :: Int -> List a -> Int
  go acc Nil = acc
  go acc (_ : xs) = go (acc + 1) xs

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

const :: ∀ a b. a -> b -> a
const x _ = x

singleton :: ∀ a. a -> List a
singleton x = x : Nil

null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

snoc :: ∀ a. List a -> a -> List a
snoc Nil x = singleton x
snoc (y : ys) x = y : snoc ys x
