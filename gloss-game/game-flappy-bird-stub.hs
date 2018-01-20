module Main where

import Graphics.Gloss.Interface.Pure.Game
import Data.Monoid

main :: IO ()
main = play displayMode white fps initialState render handleInput step
  where width = 800
        height = 600
        displayMode = InWindow "Game" windowSize windowPos
        windowSize = (width, height)
        windowPos = (10, 10)
        fps = 100

balken = [0, 100, 200, 100, 150]

data GameState = GameState
  { y :: Float
  , vy :: Float
  , time :: Float
  } deriving (Show)

initialState :: GameState
initialState = GameState
  { y = 100
  , vy = 0
  , time = 0
  }

render :: GameState -> Picture
render s =
     (translate (-100) (y s) $ color niceColor (rectangleSolid 100 40))
  <> (mconcat $ map renderBalken (zip balken [0..]))
  where
    niceColor = makeColor 0.5 1.0 0.1 1.0
    balkenColor = makeColor 0 0 0 1.0
    renderBalken (h, n) = color balkenColor $
         (translate (fromIntegral n*500 - time s*200) (h-500) $
           rectangleSolid 50 1000)
      <> (translate (fromIntegral n*500 - time s*200) (h+700) $
           rectangleSolid 50 1000)

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey k) _ _ _) s
  = case k of
      KeyUp    -> s {vy = 500}
      _        -> s
handleInput _ gs = gs

step :: Float -> GameState -> GameState
step t s = s
  { y = y s + t * vy s
  , vy = vy s - 500*t
  , time = time s + t
  }
