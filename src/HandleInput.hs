module HandleInput where

import Model
import Graphics.Gloss.Interface.IO.Game

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
-- If the user presses a character key, handle that one
inputKey (EventKey k _ _ _) gstate@(GameState status (Level p1 p1o p2 p2o enemies lvl) _) = 
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
          SpecialKey KeyLeft  -> gstate { level = (Level (newVelocity (-1) 0 p1) p1o p2 p2o enemies lvl) }
              
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

newVelocity :: Float -> Float -> GameObjects -> GameObjects
newVelocity x y (Player (P1 (PlayerInfo (ObjectInfo d (Vec vx vy) o n) t c a)))  = (Player (P1 (PlayerInfo (ObjectInfo d (Vec (vx+x) (vy+y)) o n) t c a)))

createArrow :: Player -> Level -> Level
createArrow (P1 _) (Level p1 p1o p2 p2o enemies lvl)
          = Level p1 (PlayerObjects (Arrow (ObjectInfo red (Vec 0 1) (getPosition p1) (Size 1 1))) : p1o) p2 p2o enemies lvl
createArrow (P2 _) (Level p1 p1o p2 p2o enemies lvl)
          = Level p2 p1o p2 (PlayerObjects (Arrow (ObjectInfo red (Vec 0 1) (getPosition p2) (Size 1 1))) : p2o) enemies lvl

getPosition :: GameObjects -> Pt
getPosition (Player (P1 (PlayerInfo (ObjectInfo _ _ point _) _ _ _))) = point