-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case gstate of
    GameState Menu _        ->  color red (text (show "Menu"))
    GameState Play _        ->  color green (text (show "Play"))
    GameState Pause _       ->  color blue (text (show "Pause"))
    GameState GameOver _    ->  color white (text (show "GameOver"))