module Blatt1 where

import Control.Applicative ((<*>))
import Data.Char (isLower)
import Data.List (sort, foldl', group, groupBy, maximumBy)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Ratio (numerator)


-- Aufgabe 1: Tupel

tuple, tuple', tuple'', tuple''' :: (Int, (Char, String)) -> Char
tuple    i = fst ( snd   i)
tuple'   i = fst $ snd $ i
tuple''  i = fst . snd $ i
tuple'''   = fst . snd

a1 :: [Char]
a1 = [tuple, tuple', tuple'', tuple'''] <*> pure (1, ('a', "foo"))

-- Aufgabe 2: Medianbestimmung

median :: Ord a => [a] -> a
median l = sort l !! (length l `div` 2)

a2 :: Double
a2 = median [13, -345, 4.5, 17, 123e34]

-- Aufgabe 3: Der Smiley-Operator erster Ordnung

a3 :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
a3 = (.) . (.)
-- TODO: Erklärung


-- Listenfunktionen --

-- Aufgabe 4: Groß- und Kleinschreibung

lowerList :: String -> [Bool]
lowerList = map isLower

a4_a :: [Bool]
a4_a = lowerList "aBCde"

allLower :: String -> Bool
allLower = foldr1 (&&) . lowerList

a4_b, a4_b' :: Bool
a4_b  = allLower "abcde"
a4_b' = allLower "abCde"

numLower :: String -> Int
numLower = length . filter isLower

a4_c :: Int
a4_c = numLower "abCde"

-- Aufgabe 5: Typfehler
-- x würde in der Funktion zwei Typen haben, einmal a und einmal a -> b


-- Funktionsdefinitionen

-- Aufgabe 6: Fizz buzz

fizzbuzz, fizzbuzz', fizzbuzz'' :: [String]

-- naive Lösung
fizzbuzz = map go [1..]
  where
    go :: Int -> String
    go i = if i `mod` 15 == 0
          then "fizz buzz"
          else if i `mod` 3 == 0
              then "fizz"
              else if i `mod` 5 == 0
                    then "buzz"
                    else show i

-- Lösung mit Pattern Guards (boole’sche Vergleiche)
-- Die mod-Rechnung bekommt hier einen schöneren Namen,
-- um die Idee klarer zu machen.
fizzbuzz' = flip map [1..] go
  where
    go :: Int -> String
    go i | 15 `divides` i = "fizz buzz"
         |  3 `divides` i = "fizz"
         |  5 `divides` i = "buzz"
         | otherwise  = show i
    divides :: Int -> Int -> Bool
    divides x i = i `mod` x == 0

-- Elegante Lösung mit Guards
-- Hier wird das Ergebnis in ein Tupel gepackt, und die
-- Fälle können die Informationen beider Rechnungen betrachten.
-- Dementsprechend kann man „sowohl durch drei als auch durch
-- fünf teilbar“ genau so hinschreiben.
-- (flip map ist in dem Fall :: [Int] -> (Int -> String) -> [String]
--  und wir nutzen $ \x ->, um die ganze anonyme Funktion ohno
--  Klammern angeben zu können.)
fizzbuzz'' = flip map [1..] $ \x ->
  case (x `mod` 3, x `mod` 5) of
    (0, 0) -> "fizz buzz"
    (0, _) -> "fizz"
    (_, 0) -> "buzz"
    _      -> show x

a6 :: [[String]]
a6 = map (take 15) [fizzbuzz, fizzbuzz', fizzbuzz'']

-- Aufgabe 7: Origami

maximum' :: [Integer] -> Integer
maximum' = foldl max 0

a7_1, a7_2, a7_3 :: Integer
a7_1 = maximum' []
a7_2 = maximum' [1, 2, 0]
a7_3 = maximum' [-1, -2]

-- Aufgabe 8: Fibonacci-Zahlen

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

a8_a, a8_b :: Integer
a8_a  = fib 20
a8_b  = fib 35
-- Problem: Zwischenergebnisse werden nicht gespeichert!

fib', fib'' :: [Integer]
-- Rekursive Definitionen. Finde heraus, wie sie funktionieren!
fib' = 0 : 1 : zipWith (+) fib' (tail fib')
fib'' = 0 : scanl (+) 1 fib''

a8_c :: [[Integer]]
a8_c = map (take 100) [fib', fib'']

-- Aufgabe 9: Die Collatz-Vermutung

collNext :: Integer -> Integer
collNext n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3 * n + 1

a9_a :: Integer
a9_a = collNext 19

collSeq :: Integer -> [Integer]
collSeq = iterate collNext

a9_b :: [Integer]
a9_b = take 100 $ collSeq 19

collTest :: Integer -> Bool
collTest = any (==1) . collSeq

a9_c :: Bool
a9_c = collTest $ numerator 1e10000

-- foldr short-circuits with (&&)
c9_d :: Bool
c9_d = foldr1 (&&) $ map collTest [1..1000000]

-- Aufgabe 10: Die Prelude
-- siehe NewPrelude.hs

-- Aufgabe 11: Pointless/pointfree programming

multMany, multMany' :: Num b => b -> [b] -> [b]
multMany a = map (a*)
multMany' = map . (*)

filterMap, filterMap' :: (a -> Bool) -> (b -> a) -> [b] -> [a]
filterMap f g = filter f . map g
filterMap' = undefined --TODO

-- Aufgabe 12: Run-Length-Encoding 

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group

decode :: [(Int, a)] -> [a]
decode = concat . map (\(n,c) -> take n $ repeat c)

-- Aufgabe 13: Längste Teilfolge

longestSubsequence :: (a -> Bool) -> [a] -> [a]
longestSubsequence p l =
  map fst
  . maximumBy (comparing length)
  . filter ((==True).snd.head)
  . groupBy ((==) `on` snd)
  . zip l $ map p l

-- Aufgabe 14: Kettenbrüche

-- TODO

