module Main where

import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play displayMode white fps initialState render handleInput step
  where width = 800
        height = 600
        displayMode = InWindow "Game" windowSize windowPos
        windowSize = (width, height)
        windowPos = (10, 10)
        fps = 100

-- Speichere in dieser Datenstruktur den Zustand deines Spiels, also Positionen
-- und Geschwindigkeit von deiner Spielfigur, den Gegnern, von sich bewegenden
-- Plattformen, die Anzahl an verbleibenden <3 usw.
data GameState = GameState
  { squarePosition :: Point -- ^ Point ist ein Alias für (Float, Float)
  } deriving (Show)

-- Der Zustand zu Beginn des Spiels.
initialState :: GameState
initialState = GameState
  { squarePosition = (0, 0)
  }

-- Gloss rendert Pictures, wenn man nichts tut, bei (0, 0) also in der Mitte des
-- Bildschirms, darum müssen wir mit translate x y bild unser das Bild an die
-- richtige Stelle verschieben. Unter
-- https://hackage.haskell.org/package/gloss-1.9.4.1/docs/
-- Graphics-Gloss-Data-Picture.html findest du eine Dokumentation aller
-- Funktionen, mit denen du ein Picture produzieren kannst. makeColor nimmt den
-- Rot-, Grün-, Blau- und Transparenz-Anteil einer Farbe als Floats zwischen 0
-- und 1.0.
render :: GameState -> Picture
render (GameState (x, y)) =
  translate x y $ color (makeColor 0.5 1.0 0.1 1.0) (rectangleSolid 100 40)

-- In dieser Funktion beschreibst du, wie sich der Zustand ändern soll, wenn
-- der Benutzer die Maus bewegt, klickt oder eine Taste drückt.
-- Unter https://hackage.haskell.org/package/gloss-1.9.4.1/docs/
-- Graphics-Gloss-Interface-Pure-Game.html#t:Event
-- findest du die Dokumentation zu Event.
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey k) _ _ _) (GameState (x, y))
  = case k of
      KeyUp    -> GameState (x, y + 5)
      KeyDown  -> GameState (x, y - 5)
      KeyLeft  -> GameState (x - 5, y)
      KeyRight -> GameState (x + 5, y)
      _        -> GameState (x,y)
handleInput _ gs = gs

-- Beschreibe, wie sich der Zustand ändert, wenn Zeit verstreicht.
-- `step` bekommt im ersten Argument die Änderung der Zeit,
-- seit dem das letzte Mal step aufgerufen wurde in Sekunden.
step :: Float -> GameState -> GameState
step _ gs = gs -- keine Zustandsänderung
