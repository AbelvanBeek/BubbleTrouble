-- | This module defines how the state changes
--   in response to time and user input
module Controller where

-- import Graphics.Gloss
-- import Graphics.Gloss.Interface.IO.Game
-- import System.Random

import Model
import UpdateLogic
import HighScores

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState Play lvl _) | checkGameOver lvl = return gstate { gameStatus = GameOver }
                                        | otherwise = return $ gstate { level = updateLevel $ filterLevel lvl }
step secs gstate@(GameState _ lvl _)   =   return $ gstate
