module Main where

import           Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play displayMode white fps initialState render handleInput step
  where width = 800
        height = 600
        displayMode = InWindow "Game" windowSize windowPos
        windowSize = (width, height)
        windowPos = (10, 10)
        fps = 100

data GameState = GameState
  { squarePosition :: Point -- ^ Point ist ein Alias für (Float, Float)
  , picture        :: Picture
      -- ^ Unter https://hackage.haskell.org/package/gloss-1.9.4.1/docs/Graphics-Gloss-Data-Picture.html findest
      --   du eine Dokumentation aller Funktionen, mit denen du ein Picture produzieren kannst.
      --   makeColor nimmt den Rot-, Grün-, Blau- und Transparenz-Anteil einer Farbe als Floats
      --   zwischen 0 und 1.0.
  } deriving (Show)

initialState :: GameState
initialState = GameState
  { squarePosition = (0, 0)
  , picture = color (makeColor 0.5 1.0 0.1 1.0) (rectangleSolid 100 40)
  }

-- Gloss rendert Pictures, wenn man nichts tut, bei (0, 0) also in der
-- Mitte des Bildschirms, darum müssen wir mit translate x y bild unser
-- Bild an die richtige Stelle verschieben
render :: GameState -> Picture
render (GameState (x, y) pic)  = translate x y pic

-- Unter https://hackage.haskell.org/package/gloss-1.9.4.1/docs/Graphics-Gloss-Interface-Pure-Game.html#t:Event
-- findest du die Dokumentation zu Event.
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey k) _ _ _) (GameState (x, y) pic)
  = case k of
      KeyUp -> GameState (x, y + 5) pic
      KeyDown -> GameState (x, y - 5) pic
      KeyLeft -> GameState (x - 5, y) pic
      KeyRight -> GameState (x + 5, y) pic
      _ -> GameState (x,y) pic
handleInput _ gs = gs

-- Step bekommt im ersten Argument die Änderung der Zeit,
-- seit dem das letzte Mal step aufgerufen wurde in Sekunden.
step :: Float -> GameState -> GameState
step _ gs = gs
