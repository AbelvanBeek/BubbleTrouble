module HandleInput where

import Model
import Graphics.Gloss.Interface.IO.Game
import HelperFunctions
import InitialStates

-- | Handle user input
input :: Event -> IO GameState -> IO (IO GameState)
input e gstate = do gstat <- gstate
                    return (inputKey e gstat)

inputKey :: Event -> GameState -> IO GameState
-- If the user presses a character key, handle that one
inputKey (EventKey k Down _ _) gstate@(GameState status (Level p1 p1o p2 p2o enemies lvl ani pics) _) = 
  case status of

    Menu    ->
      case k of
          -- Player movements
          SpecialKey KeySpace -> initialPlay
        
          -- Game handling
          SpecialKey KeyEsc   -> error "Game closed"
        
          -- Not recognized
          _                   -> return $ gstate
        
    Play    ->
      case k of
          -- Player movements
          Char 'p'            -> return $ gstate { gameStatus = Pause }
          Char 'g'            -> return $ gstate { gameStatus = GameOver }

          -- P1 movement left and right
          SpecialKey KeyLeft  -> return $ gstate { level = (Level (newVelocity (-playerSpeed) 0 p1) p1o p2 p2o enemies lvl ani pics) }
          SpecialKey KeyRight -> return $ gstate { level = (Level (newVelocity   playerSpeed  0 p1) p1o p2 p2o enemies lvl ani pics) }
          -- Max arrows = 1
          SpecialKey KeyUp    -> if length p1o < arrowAmount then return $ gstate { level = (Level p1 (createArrow p1 p1o) p2 p2o enemies lvl ani pics) } 
                                                             else return $ gstate

          -- P2 movement left and right
          Char 'a'            -> return $ gstate { level = (Level p1 p1o (newVelocity (-playerSpeed) 0 p2) p2o enemies lvl ani pics) }
          Char 'd'            -> return $ gstate { level = (Level p1 p1o (newVelocity   playerSpeed  0 p2) p2o enemies lvl ani pics) }
          Char 'w'            -> if length p2o < arrowAmount then return $ gstate { level = (Level p1 p1o p2 (createArrow p2 p2o) enemies lvl ani pics) }
                                                             else return $ gstate

          -- Game handling
          SpecialKey KeyEsc   -> error "Game closed"
              
          -- Not recognized
          _                   -> return $ gstate

    Pause   ->
      case k of
          -- Player movements
          SpecialKey KeySpace -> return $ gstate { gameStatus = Play }
              
          -- Game handling
          SpecialKey KeyEsc   -> error "Game closed"
              
          -- Not recognized
          _                   -> return $ gstate

    GameOver->
      case k of
          -- Game handling
          SpecialKey KeySpace -> initialMenu
          SpecialKey KeyEsc   -> error "Game closed"

          -- Not recognized
          _                   -> return $ gstate

-- Reverse Player Movements (Other things can be removed, but lets keep em here in case we need them further on)
inputKey (EventKey k Up _ _) gstate@(GameState Play (Level p1 p1o p2 p2o enemies lvl ani pics) _) = 
  case k of
    -- P1 movement left and right reverse
    SpecialKey KeyLeft  -> return $ gstate { level = (Level (plusxIfNecessary  0  0 p1) p1o p2 p2o enemies lvl ani pics) }
    SpecialKey KeyRight -> return $ gstate { level = (Level (minxIfNecessary (0) 0 p1) p1o p2 p2o enemies lvl ani pics) }
          
    -- P2 movement left and right reverse
    Char 'a'            -> return $ gstate { level = (Level p1 p1o (plusxIfNecessary 0 0 p2) p2o enemies lvl ani pics) }
    Char 'd'            -> return $ gstate { level = (Level p1 p1o (minxIfNecessary 0 0 p2) p2o enemies lvl ani pics) }
               
    -- Not recognized
    _                   -> return $ gstate

-- Handle other patterns than an EventKey
inputKey _ gstate     = return $ gstate

--Since there is no while keydown function we need to reset the speed of the players once the key goes back up,
--if however the player already pressed the right key when the left key was still down, we don't want the player
--to stand still after the left key goes up when the right key is still down. This functions is used to realise that.
plusxIfNecessary, minxIfNecessary :: Float -> Float -> GameObjects -> GameObjects
plusxIfNecessary x y p@(Player (P1 (PlayerInfo (ObjectInfo d (vx,vy) o n) t c a))) | vx > 0 = p --if moving right, we dont do anything when the left key goes up
                                                                                   | otherwise = (newVelocity x y p) --adjust the velocity
plusxIfNecessary x y p@(Player (P2 (PlayerInfo (ObjectInfo d (vx,vy) o n) t c a))) | vx > 0 = p
                                                                                   | otherwise = (newVelocity x y p)
minxIfNecessary x y p@(Player (P1 (PlayerInfo (ObjectInfo d (vx,vy) o n) t c a)))  | vx < 0 = p --if moving left, we dont do anything when the right key goes up
                                                                                   | otherwise = (newVelocity x y p) --adjust the velocity
minxIfNecessary x y p@(Player (P2 (PlayerInfo (ObjectInfo d (vx,vy) o n) t c a)))  | vx < 0 = p
                                                                                   | otherwise = (newVelocity x y p) 



