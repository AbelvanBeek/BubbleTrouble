module InitialStates where

import Graphics.Gloss
import Model
import Random
import LoadPictures

{- Initialize States -}

--All the objects in the level are undefined while we are in the menu state
initialMenu :: IO GameState
initialMenu = return $ GameState Menu (Level undefined undefined undefined undefined undefined undefined undefined loadPictures) undefined

--generates a random level (Meaning only random ball positions)
initialPlay :: IO GameState
initialPlay = do rndlvl <- randomPlayLevel (Player(P1 (PlayerInfo (ObjectInfo red (0,0) (0,-320) (Size 1 1)) 0 No 5))) (Player(P2 (PlayerInfo (ObjectInfo red (0,0) (0,-320) (Size 1 1)) 0 No 5)))
                 return $ GameState Play rndlvl 0

initialPlayWPlayer :: GameObjects -> GameObjects -> IO GameState
initialPlayWPlayer p1 p2 = let x = (randomPlayLevel p1 p2)
                           in do rndlvl <- x
                                 return $ GameState Play rndlvl 0