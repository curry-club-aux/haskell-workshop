module NewPrelude where

import qualified Prelude as P
import qualified Data.List as L
import Prelude ((+), undefined, Integer, otherwise, (&&), Bool(..))

head [] = undefined
head (x:xs) = x

tail [] = undefined
tail (x:xs) = xs

init [] = undefined
init [x] = []
init (x:xs) = x : init xs

last [] = undefined
last (x:[]) = x
last (_:xs) = last xs

length [] = 0
length [x] = 1
length (x:xs) = 1 + length xs
length' :: [a] -> Integer
length' = L.foldl' (\x _ -> x+1) 0

flip f x y = f y x
reverse (x:xs) = reverse xs ++ [x]
reverse' :: [a] -> [a]
reverse' = P.foldl (flip (:)) []

(++) [] ys = ys
(++) (x:xs) y = x : (xs ++ y)

iterate f x = x : iterate f (f x)

map _ [] = []
map f (x:xs) = f x : map f xs

filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs
filter' p = P.foldr (\x l -> if p x then x:l else l) []

intersperse _ []  = []
intersperse _ [x] = [x]
intersperse a (x:xs) = x : a : intersperse a xs

-- TODO: foldr
concat :: [[a]] -> [a]
concat = P.foldr (++) []

zipWith _ _ [] = []
zipWith _ [] _ = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

repeat a = a : repeat a

and :: [Bool] -> Bool
and = P.foldr (&&) True

takeWhile _ [] = []
takeWhile p (x:xs)
  | p x = x : takeWhile p xs
  | otherwise = []

dropWhile _ [] = []
dropWhile p (x:xs)
  | p x = dropWhile p xs
  | otherwise = xs

maximum :: (P.Ord a) => [a] -> a
maximum [] = undefined
-- attention: foldl builds chunk
maximum xs = P.foldl1 P.max xs
