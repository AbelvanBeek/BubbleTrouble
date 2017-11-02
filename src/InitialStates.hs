module InitialStates where
    
import Graphics.Gloss

import Model

{- Initialize States -}

initialMenu :: GameState
initialMenu = GameState Menu (Level 
                                (Player(P1 (PlayerInfo (ObjectInfo red (0,0) (0,-320) (Size 1 1)) 0 No 5)))
                                [] 
                                (Player(P2 (PlayerInfo (ObjectInfo red (0,0) (0,-320) (Size 1 1)) 0 No 5)))
                                [] 
                                [EnemyObjects(Ball (ObjectInfo red (3,0) (0,0) (Size 1 1)))] 
                                []) 0


initialPlay :: GameState
initialPlay = GameState Play (Level 
                                (Player(P1 (PlayerInfo (ObjectInfo red (0,0) (0,-320) (Size 1 1)) 0 No 5)))
                                [] 
                                (Player(P2 (PlayerInfo (ObjectInfo red (0,0) (0,-320) (Size 1 1)) 0 No 5)))
                                [] 
                                [EnemyObjects(Ball (ObjectInfo red (3,0) (0,0) (Size 1 1)))] 
                                []) 0

initialGameOver :: GameState
initialGameOver = GameState GameOver (Level 
                                (Player(P1 (PlayerInfo (ObjectInfo red (0,0) (0,-320) (Size 1 1)) 0 No 5)))
                                [] 
                                (Player(P2 (PlayerInfo (ObjectInfo red (0,0) (0,-320) (Size 1 1)) 0 No 5)))
                                [] 
                                [EnemyObjects(Ball (ObjectInfo red (3,0) (0,0) (Size 1 1)))] 
                                []) 0


-- initial pause not neccesary? Just freeze the current game -> No update velocity etc anymore