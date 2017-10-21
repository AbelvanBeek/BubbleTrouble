-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return $ gstate -- here something like "return $ update $ getNewVelocity $ gstate"

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
-- If the user presses a character key, handle that one
inputKey (EventKey k _ _ _) gstate@(GameState status _ _) = 
  case status of

    Menu    ->
      case k of
          -- Player movements
          Char 'p'            -> gstate { gameStatus = Pause }
          Char 'm'            -> gstate { gameStatus = Menu }
          Char 'g'            -> gstate { gameStatus = GameOver }
          SpecialKey KeySpace -> gstate { gameStatus = Play }
        
          -- Game handling
          SpecialKey KeyEsc   -> error "Game closed"
        
          -- Not recognized
          _                   -> gstate
        
    Play    ->
      case k of
          -- Player movements
          Char 'p'            -> gstate { gameStatus = Pause }
          Char 'm'            -> gstate { gameStatus = Menu }
          Char 'g'            -> gstate { gameStatus = GameOver }
          SpecialKey KeySpace -> gstate { gameStatus = Play }
              
          -- Game handling
          SpecialKey KeyEsc   -> error "Game closed"
              
          -- Not recognized
          _                   -> gstate

    Pause   ->
      case k of
          -- Player movements
          Char 'p'            -> gstate { gameStatus = Pause }
          Char 'm'            -> gstate { gameStatus = Menu }
          Char 'g'            -> gstate { gameStatus = GameOver }
          SpecialKey KeySpace -> gstate { gameStatus = Play }
              
          -- Game handling
          SpecialKey KeyEsc   -> error "Game closed"
              
          -- Not recognized
          _                   -> gstate

    GameOver->
      case k of
          -- Game handling
          SpecialKey KeyEsc   -> error "Game closed"

          -- Not recognized
          _                   -> gstate

-- Handle other patterns than an EventKey
inputKey _ gstate     = gstate