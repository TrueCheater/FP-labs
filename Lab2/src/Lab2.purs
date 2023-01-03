module Lab2 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Data.List (List(..), (:), length, reverse)
import Data.Tuple (Tuple(..))

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex fn l = fIdx fn l 0
    where
    fIdx :: forall b. (b -> Boolean) -> List b -> Int -> Maybe Int
    fIdx _ Nil _ = Nothing
    fIdx f (x : xs) idx | f x = Just idx
                        | otherwise = fIdx f xs (idx + 1)

findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex fn xs = map ((length xs - 1) - _) (findIndex fn (reverse xs))

zip :: forall a b. List a -> List b -> List (Tuple a b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (x : xs) (y : ys) = (Tuple x y : zip xs ys)

unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip Nil = Tuple Nil Nil
unzip (Tuple x y : zs) = Tuple (x : xs) (y : ys)
    where
    Tuple xs ys = unzip zs

filter :: forall a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter fn (x : xs) | fn x = x : filter fn xs
                   | otherwise = filter fn xs

--tail recursion filter
filterT :: forall a. (a -> Boolean) -> List a -> List a
filterT fn l = fltr fn l Nil
    where
    fltr :: forall b. (b -> Boolean) -> List b -> List b -> List b
    fltr _ Nil list = reverse list
    fltr f (x : xs) list | f x = fltr f xs (x : list)
                         | otherwise = fltr f xs list

take :: forall a. Int -> List a -> List a
take _ Nil = Nil
take n (x : xs) | n > 0 = (x : take (n - 1) xs)
                | otherwise = Nil

--tail recursion take
takeT :: forall a. Int -> List a -> List a
takeT n l = takeT' n l Nil
    where
    takeT' :: forall b. Int -> List b -> List b -> List b
    takeT' _ Nil _ = Nil
    takeT' number (x : xs) list | number > 0 = takeT' (number - 1) xs (x : list)
                                | otherwise = reverse list

test :: Effect Unit
test = do
    log $ show $ findIndex (\a -> a `mod` 2 == 0) (1 : 2 : 3 : Nil)
    log $ show $ findIndex (_ < 1) (1 : 2 : 3 : Nil)
    log $ show $ findLastIndex (\a -> a `mod` 2 == 0) (2 : 1 : 3 : 4 : Nil)
    log $ show $ findLastIndex (_ > 5) (2 : 1 : 3 : 4 : Nil)
    log $ show $ zip (1 : 2 : 3 : Nil) (1 : 2 : 3 : Nil)
    log $ show $ unzip ((Tuple 1 1) : (Tuple 2 2) : (Tuple 3 3) : Nil)
    log $ show $ filter (_ > 1) (2 : 1 : 3 : 4 : Nil)
    log $ show $ filterT (_ > 1) (2 : 1 : 3 : 4 : Nil)
    log $ show $ take 3 (2 : 1 : 3 : 4 : Nil)
    log $ show $ takeT 3 (2 : 1 : 3 : 4 : Nil)

