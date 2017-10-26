-- | This module defines how the state changes
--   in response to time and user input
module Controller where

-- import Graphics.Gloss
-- import Graphics.Gloss.Interface.IO.Game
-- import System.Random

import Model
import UpdateLogic

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState _ lvl _)    =   return $ gstate { level = updateLevel lvl }

-- | Handle input

