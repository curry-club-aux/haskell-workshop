module Main where

import Control.Applicative ((<*>))
import Data.Char (isLower)
import Data.List (sort, foldl')
import Data.Ratio (numerator)


-- Aufgabe 1: Tupel

a1'    i = fst ( snd   i)
a1''   i = fst $ snd $ i
a1'''  i = fst . snd $ i
a1''''   = fst . snd

a1 = [a1', a1'', a1''', a1''''] <*> pure (1, ('a', "foo"))

-- Aufgabe 2: Medianbestimmung

median l = sort l !! (length l `div` 2)
a2 = median [13, -345, 4.5, 17, 123e34]

-- Aufgabe 3: Der Smiley-Operator erster Ordnung

a3 :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
a3 = (.) . (.)
-- TODO: Erklärung


-- Listenfunktionen --

-- Aufgabe 4: Groß- und Kleinschreibung

lowerList :: String -> [Bool]
lowerList = map isLower
a4_a = lowerList "aBCde"

allLower = foldr1 (&&) . lowerList
a4_b  = allLower "abcde"
a4_b' = allLower "abCde"

numLower = length . filter isLower
a4_c  = numLower "abCde"

-- Aufgabe 5: Typfehler
-- x würde in der Funktion zwei Typen haben, einmal a und einmal a -> b


-- Funktionsdefinitionen

-- Aufgabe 6: Fizz buzz

fizzbuzz = flip map [1..] $ \x ->
  case (x `mod` 3, x `mod` 5) of
    (0, 0) -> "fizz buzz"
    (0, _) -> "fizz"
    (_, 0) -> "buzz"
    otherwise -> show x
a6 = take 15 fizzbuzz
-- TODO more solutions

-- Aufgabe 7: Origami

maximum' = foldl max 0
a7_1 = maximum' []
a7_2 = maximum' [1, 2, 0]
a7_3 = maximum' [-1, -2]

-- Aufgabe 8: Fibonacci-Zahlen

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
a8_a  = fib 20
a8_b  = fib 35
-- Problem: Zwischenergebnisse werden nicht gespeichert!

fib' = 0 : 1 : zipWith (+) fib' (tail fib')
fib'' = 0 : scanl (+) 1 fib''
a8_c = take 100 fib'

-- Aufgabe 9: Die Collatz-Vermutung

collNext n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3 * n + 1
a9_a = collNext 19

collSeq = iterate collNext
a9_b = take 100 $ collSeq 19

collTest = any (==1) . collSeq
a9_c = collTest $ numerator 1e10000

-- foldr short-circuits with (&&)
c9_d = foldr1 (&&) $ map collTest [1..1000000]

-- Aufgabe 10: Die Prelude
-- siehe NewPrelude.hs
