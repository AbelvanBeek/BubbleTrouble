-- | This module defines how the state changes
--   in response to time and user input
module Controller where

-- import Graphics.Gloss
-- import Graphics.Gloss.Interface.IO.Game
-- import System.Random

import Model
import Collision
import UpdateLogic
import HighScores
import InitialStates

import Data.Maybe

-- | Handle one iteration of the game
step :: Float -> IO GameState -> IO (IO GameState)
step secs gstat = do gstate@(GameState status lvl@(Level p1 p1o p2 p2o enemies lvls ani pics) _) <- gstat
                     case status of
                        Play    | isNothing (checkPlayerCollided lvl) -> if checkGameOver lvl
                                                                      then return $ return $ gstate { gameStatus = GameOver }
                                                                      else return $ return $ gstate { elapsedTime = elapsedTime gstate + secs, level = updateLevel (elapsedTime gstate) $ filterLevel lvl }
                                | checkPlayerCollided lvl == Just (Player (P1 undefined)) -> if checkGameOver lvl 
                                                                                                     then return $ return $ gstate { gameStatus = GameOver }
                                                                                                     else return $ initialPlayWPlayer (fromJust (checkPlayerCollided lvl)) p2
                                | otherwise -> if checkGameOver lvl
                                                  then return $ return $ gstate { gameStatus = GameOver }
                                                  else return $ initialPlayWPlayer p1 (fromJust (checkPlayerCollided lvl))

                        _    -> return $ return gstate { elapsedTime = elapsedTime gstate + secs }
