-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState _ (Level p1 p1o p2 p2o enemies lvl) _) = return $ gstate {level = (Level p1 p1o p2 p2o (updateObjects enemies) lvl)} -- here something like "return $ update $ getNewVelocity $ gstate"

-- | Handle input

