module HandleInput where

import Model
import Graphics.Gloss.Interface.IO.Game
import HelperFunctions
import InitialStates

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
-- If the user presses a character key, handle that one
inputKey (EventKey k Down _ _) gstate@(GameState status (Level p1 p1o p2 p2o enemies lvl) _) = 
  case status of

    Menu    ->
      case k of
          -- Player movements
          SpecialKey KeySpace -> initialPlay
        
          -- Game handling
          SpecialKey KeyEsc   -> error "Game closed"
        
          -- Not recognized
          _                   -> gstate
        
    Play    ->
      case k of
          -- Player movements
          Char 'p'            -> gstate { gameStatus = Pause }
          Char 'g'            -> gstate { gameStatus = GameOver }

          -- P1 movement left and right
          SpecialKey KeyLeft  -> gstate { level = (Level (newVelocity (-playerSpeed) 0 p1) p1o p2 p2o enemies lvl) }
          SpecialKey KeyRight -> gstate { level = (Level (newVelocity   playerSpeed  0 p1) p1o p2 p2o enemies lvl) }
          -- Max arrows = 1
          SpecialKey KeyUp    -> if length p1o < arrowAmount then gstate { level = (Level p1 (createArrow p1 p1o) p2 p2o enemies lvl) } 
                                                             else gstate

          -- P2 movement left and right
          Char 'a'            -> gstate { level = (Level p1 p1o (newVelocity (-playerSpeed) 0 p2) p2o enemies lvl) }
          Char 'd'            -> gstate { level = (Level p1 p1o (newVelocity   playerSpeed  0 p2) p2o enemies lvl) }
          Char 'w'            -> if length p1o < arrowAmount then gstate { level = (Level p1 p1o p2 (createArrow p2 p2o) enemies lvl) }
                                                             else gstate

          -- Game handling
          SpecialKey KeyEsc   -> error "Game closed"
              
          -- Not recognized
          _                   -> gstate

    Pause   ->
      case k of
          -- Player movements
          SpecialKey KeySpace -> gstate { gameStatus = Play }
              
          -- Game handling
          SpecialKey KeyEsc   -> error "Game closed"
              
          -- Not recognized
          _                   -> gstate

    GameOver->
      case k of
          -- Game handling
          SpecialKey KeySpace -> initialMenu
          SpecialKey KeyEsc   -> error "Game closed"

          -- Not recognized
          _                   -> gstate

-- Reverse Player Movements (Other things can be removed, but lets keep em here in case we need them further on)
inputKey (EventKey k Up _ _) gstate@(GameState Play (Level p1 p1o p2 p2o enemies lvl) _) = 
  case k of
    -- P1 movement left and right reverse
    SpecialKey KeyLeft  -> gstate { level = (Level (newVelocity   playerSpeed  0 p1) p1o p2 p2o enemies lvl) }
    SpecialKey KeyRight -> gstate { level = (Level (newVelocity (-playerSpeed) 0 p1) p1o p2 p2o enemies lvl) }
          
    -- P2 movement left and right reverse
    Char 'a'            -> gstate { level = (Level p1 p1o (newVelocity   playerSpeed  0 p2) p2o enemies lvl) }
    Char 'd'            -> gstate { level = (Level p1 p1o (newVelocity (-playerSpeed) 0 p2) p2o enemies lvl) }
              
    -- Not recognized
    _                   -> gstate

-- Handle other patterns than an EventKey
inputKey _ gstate     = gstate


newVelocity :: Float -> Float -> GameObjects -> GameObjects
newVelocity x y (Player (P1 (PlayerInfo (ObjectInfo d (vx,vy) o n) t c a)))  = (Player (P1 (PlayerInfo (ObjectInfo d ((vx+x),(vy+y)) o n) t c a)))
newVelocity x y (Player (P2 (PlayerInfo (ObjectInfo d (vx,vy) o n) t c a)))  = (Player (P2 (PlayerInfo (ObjectInfo d ((vx+x),(vy+y)) o n) t c a)))

createArrow :: GameObjects -> [GameObjects] -> [GameObjects]
createArrow player xs
          = (PlayerObjects (Arrow (ObjectInfo red (0,5) ((getX (getPosition player)),-1388) (Size 1 1)))) : xs


