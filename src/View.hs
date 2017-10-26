-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss.Juicy
import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view (GameState _ (Level _ _ _ _ enemies _) _) = do x <- sequence $ map draw enemies                 -- return . viewPure
                                                    return (pictures x)

viewPure :: GameState -> Picture
viewPure gstate = case gstate of
    GameState Menu lvl _       ->  color green (text (show "Menu"))
    GameState Play lvl _       ->  color green (text (show "Play"))
    GameState Pause lvl _      ->  color blue (text (show "Pause"))
    GameState GameOver lvl _   ->  color white (text (show "GameOver"))