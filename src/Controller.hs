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
step :: Float -> IO GameState -> IO (IO GameState)
step secs gstat = do gstate@(GameState status lvl _) <- gstat
                     case status of
                        Play -> if checkGameOver lvl 
                                   then return $ return $ gstate { gameStatus = GameOver }
                                   else return $ return $ gstate { level = updateLevel secs $ filterLevel lvl }
                        _    -> return gstat
