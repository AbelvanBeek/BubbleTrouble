-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case gstate of
    GameState Menu lvl _       ->  pictures [showSprite x | x <- lvl] -- For every gameobject x in level, show x -> MAAK SHOW VOOR GAMEOBJECTS
    GameState Play lvl _       ->  color green (text (show "Play"))
    GameState Pause lvl _      ->  color blue (text (show "Pause"))
    GameState GameOver lvl _   ->  color white (text (show "GameOver"))