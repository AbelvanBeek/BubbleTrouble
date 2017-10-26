-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss

import Model
import DrawLogic

view :: GameState -> IO Picture
view (GameState _ lvl _) = do x <- sequence $ drawLevel lvl                -- return . viewPure
                              return (pictures x)

viewPure :: GameState -> Picture
viewPure gstate = case gstate of
    GameState Menu lvl _       ->  color green (text (show "Menu"))
    GameState Play lvl _       ->  color green (text (show "Play"))
    GameState Pause lvl _      ->  color blue (text (show "Pause"))
    GameState GameOver lvl _   ->  color white (text (show "GameOver"))