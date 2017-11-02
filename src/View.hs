-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss

import Model
import HighScores
import DrawLogic

view :: GameState -> IO Picture
view gstate = viewPure gstate

viewPure :: GameState -> IO Picture
viewPure gstate = case gstate of
    GameState Menu lvl _       ->  return $ translate (-560) 0 $ color green (text (show ".:Bubble Trouble:."))
    GameState Play lvl _       ->  do x <- sequence $ drawLevel lvl
                                      return (pictures x)
    GameState Pause lvl _      ->  do x <- sequence $ drawLevel lvl
                                      return (pictures ((translate (-325) 0 $ color blue (text (show ".:Paused:."))) : x))
    GameState GameOver lvl _   ->  do updateHighScores lvl
                                      return $ translate (-350) 0 $ color black (text (show ".:GameOver:."))
