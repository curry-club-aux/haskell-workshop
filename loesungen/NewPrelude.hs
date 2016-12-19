module NewPrelude where

import qualified Prelude as P
import qualified Data.List as L
import Prelude ((+), undefined, Integer, otherwise, (&&), Bool(..))

-- Fleißaufgabe: Vergleiche die Implementierungen
-- mit denen in der richtigen Standardbibliothek in `Data.List`.
-- https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-List.html
-- Wie ist denn die Geschichte hinter `sort`?
-- Wofür werden die RULES und INLINE Compiler-Pragmas benutzt?

------
-- All diese Funktionen sind nicht für alle Eingaben
-- definiert und deshalb gefährlich zu benutzen!
-- Es gibt in dem Modul `Safe` sichere Alternativen.

head :: [a] -> a
head [] = undefined
head (x:_) = x

tail :: [a] -> [a]
tail [] = undefined
tail (_:xs) = xs

init :: [a] -> [a]
init [] = undefined
init [_] = []
init (x:xs) = x : init xs

last :: [a] -> a
last [] = undefined
last (x:[]) = x
last (_:xs) = last xs

--
-- !!
------

length :: [a] -> Integer
length [] = 0
length [_] = 1
length (_:xs) = 1 + length xs

length' :: [a] -> Integer
length' = L.foldl' (\x _ -> x+1) 0

-- Frage: Warum muss man die rechte Funktion
--        nicht in Klammern setzen?
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

reverse, reverse' :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

reverse' = P.foldl (flip (:)) []

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ y  = x : (xs ++ y)

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter, filter' :: (t -> Bool) -> [t] -> [t]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

filter' p = P.foldr (\x l -> if p x then x:l else l) []

intersperse :: a -> [a] -> [a]
intersperse _ []  = []
intersperse _ [x] = [x]
intersperse a (x:xs) = x : a : intersperse a xs

-- TODO: foldr
concat :: [[a]] -> [a]
concat = P.foldr (++) []

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ _ [] = []
zipWith _ [] _ = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

repeat :: a -> [a]
repeat a = a : repeat a

and :: [Bool] -> Bool
and = P.foldr (&&) True

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
  | p x = x : takeWhile p xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs)
  | p x = dropWhile p xs
  | otherwise = xs

maximum :: (P.Ord a) => [a] -> a
maximum [] = undefined
-- attention: foldl builds chunk
maximum xs = P.foldl1 P.max xs
