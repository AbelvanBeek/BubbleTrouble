-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Juicy

import Model
import HighScores
import DrawLogic

view :: IO GameState -> IO Picture
view gstat = do gstate <- gstat
                viewPure gstate

viewPure :: GameState -> IO Picture
viewPure gstate = case gstate of
    GameState Menu (Level _ _ _ _ _ _ _ pics) _ -> do x <- pics !! 8
                                                      return $ pictures ((scale 4.5 4.5 x) : [(translate (-560) 0 $ color black (text ".:Bubble Trouble:."))])
    GameState Play lvl _       ->  do x <- sequence $ drawLevel lvl
                                      return (pictures x)
    GameState Pause lvl _      ->  do x <- sequence $ drawLevel lvl
                                      return $ pictures (x ++ [translate (-325) 0 $ color blue (text ".:Paused:.")])
    GameState GameOver lvl@(Level (Player (P1 (PlayerInfo _ p1score _ _))) _ (Player (P2 (PlayerInfo _ p2score _ _))) _ _ _ _ pics) _   ->  
                                                          do x <- pics !! 8
                                                             updateHighScores lvl
                                                             return $ pictures ((scale 4.5 4.5 x) : 
                                                                                (translate (-350) (100) $ color black (text ".:GameOver:.")) :
                                                                                (scale 0.5 0.5 $ translate (-600) (-100) $ color black (text "Player 1")) :
                                                                                (scale 0.5 0.5 $ translate (-410) (-350) $ color black (text (show p1score))) :                                                                                
                                                                                (scale 0.5 0.5 $ translate (200) (-100) $ color black (text "Player 2")) :
                                                                                (scale 0.5 0.5 $ translate (330) (-350) $ color black (text (show p2score))) :                                                                                
                                                                                []
                                                                               )
