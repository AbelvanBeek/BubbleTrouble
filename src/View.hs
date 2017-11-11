-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss

import Model
import HighScores
import DrawLogic

view :: IO GameState -> IO Picture
view gstat = do gstate <- gstat
                viewPure gstate

viewPure :: GameState -> IO Picture
viewPure gstate = case gstate of
    GameState Menu lvl _       ->  return $ translate (-560) 0 $ color green (text ".:Bubble Trouble:.")
    GameState Play lvl s       ->  do x <- sequence $ drawLevel lvl
                                      return (pictures ((text(show s)) : x))
    GameState Pause lvl _      ->  do x <- sequence $ drawLevel lvl
                                      return (pictures ((translate (-325) 0 $ color blue (text ".:Paused:.")) : x))
    GameState GameOver lvl _   ->  do updateHighScores lvl
                                      return $ translate (-350) 0 $ color black (text ".:GameOver:.")
