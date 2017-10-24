-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Juicy

{- Data Types -}

-- Main game status data types
data GameStatus = Menu 
                | Play
                | Pause
                | GameOver

data GameState = GameState {
                   gameStatus  :: GameStatus
                 , level :: Level
                 , elapsedTime :: Float
                 }

-- Level/Score related data types
data Level = Level { 
                  p1 :: Player
                , p1Objects :: [PlayerObjects]
                , p2 :: Player
                , p2Objects :: [PlayerObjects]
                , enemies :: [EnemyObjects]
                , lvl :: [LevelObjects]
                }

type Score = Int
type Lives = Int

-- Display data types
data Pt    = Pt  { xpt  :: Float, ypt  :: Float }
data Vec   = Vec { xvec :: Float, yvex :: Float }

type Position = Pt
type Velocity = Vec

data Size = Size { w :: Float, h :: Float }

type Sprite = Picture
newtype Animation = Animation { unAnimate :: [Sprite] }

-- Game object related data types
-- Union all gameobjects in one data type
--data GameObjects = Player | PlayerObjects | LevelObjects | EnemyObjects

data ObjectInfo = ObjectInfo { c :: Color, vel :: Velocity, pos :: Position, size :: Size }
data PlayerObjects = Arrow ObjectInfo
data LevelObjects = Wall ObjectInfo 
data EnemyObjects = Ball ObjectInfo 

-- Player related data types
data Player = P1 PlayerInfo | P2 PlayerInfo
data IsShooting = Yes | No
data PlayerInfo = PlayerInfo {objectinfo :: ObjectInfo, score :: Score, shooting :: IsShooting, lives :: Lives}

{- Initialize States -}

initialMenu :: GameState
initialMenu = GameState Menu (Level 
                                (P1 (PlayerInfo (ObjectInfo red (Vec 0 0) (Pt 0 0) (Size 1 1)) 0 No 5))
                                [] 
                                (P1 (PlayerInfo (ObjectInfo red (Vec 0 0) (Pt 0 0) (Size 1 1)) 0 No 5))
                                [] 
                                [Ball (ObjectInfo red (Vec 0 0) (Pt 0 0) (Size 1 1))] 
                                []) 0


initialPlay :: GameState
initialPlay = GameState Play (Level 
                                (P1 (PlayerInfo (ObjectInfo red (Vec 0 0) (Pt 0 0) (Size 1 1)) 0 No 5))
                                [] 
                                (P1 (PlayerInfo (ObjectInfo red (Vec 0 0) (Pt 0 0) (Size 1 1)) 0 No 5))
                                [] 
                                [] 
                                []) 0

initialGameOver :: GameState
initialGameOver = GameState GameOver (Level 
                                        (P1 (PlayerInfo (ObjectInfo red (Vec 0 0) (Pt 0 0) (Size 1 1)) 0 No 5))
                                        [] 
                                        (P1 (PlayerInfo (ObjectInfo red (Vec 0 0) (Pt 0 0) (Size 1 1)) 0 No 5))
                                        [] 
                                        [] 
                                        []) 0


-- initial pause not neccesary? Just freeze the current game -> No update velocity etc anymore

{- Type Classes -}

-- update locations
class Update a where 
  update :: a -> a

instance Update Player where
  update o@(P1 (PlayerInfo objectinfo _ _ _)) = o
  update o@(P2 (PlayerInfo objectinfo _ _ _)) = o

instance Update PlayerObjects where
  update o@(Arrow objectinfo)      = o

instance Update EnemyObjects where
  update o@(Ball objectinfo)       = o

instance Update LevelObjects where
  update o@(Wall objectinfo)       = o


-- draw with correct sprite
class Draw a where 
  draw :: a -> IO Picture

instance Draw Player where
  draw o@(P1 (PlayerInfo objectinfo _ _ _)) = retSprite (getFilePathP o) objectinfo
  draw o@(P2 (PlayerInfo objectinfo _ _ _)) = retSprite (getFilePathP o) objectinfo

instance Draw PlayerObjects where
  draw o@(Arrow objectinfo)      = retSprite (getFilePathU o) objectinfo

instance Draw EnemyObjects where
  draw o@(Ball objectinfo)       = retSprite (getFilePathE o) objectinfo

instance Draw LevelObjects where
  draw o@(Wall objectinfo)       = retSprite (getFilePathL o) objectinfo

getFilePathP :: Player -> FilePath
getFilePathP (P1 _) = "assets/ball.png"
getFilePathP (P2 _) = "assets/ball.png"

getFilePathU :: PlayerObjects -> FilePath
getFilePathU (Arrow _) = "assets/ball.png"

getFilePathE :: EnemyObjects -> FilePath
getFilePathE (Ball _) = "assets/ball.png"

getFilePathL :: LevelObjects -> FilePath
getFilePathL (Wall _) = "assets/ball.png"

retSprite :: FilePath -> ObjectInfo -> IO Picture
retSprite path (ObjectInfo c (Vec vx vy) (Pt px py) (Size w h)) = do mbpic <- loadJuicyPNG path
                                                                     return $ color c $ translate vx vy $ scale w h (maybePicToIO mbpic)

{- Helper Functions-}

-- Convert a maybe picture to a picture -> Blank picture in case of Nothing
maybePicToIO :: Maybe Picture -> Picture
maybePicToIO Nothing  = blank
maybePicToIO (Just x) = x
