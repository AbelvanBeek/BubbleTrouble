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

data GameState = GameState { gameStatus  :: GameStatus
                           , level       :: Level
                           , elapsedTime :: Float
                           }

-- Level/Score related data types
data Level = Level { p1         :: GameObjects
                   , p1Objects  :: [GameObjects]
                   , p2         :: GameObjects
                   , p2Objects  :: [GameObjects]
                   , enemies    :: [GameObjects]
                   , lvl        :: [GameObjects]
                   , animations :: [GameObjects]
                   , pics       :: [IO Picture]
                   }

type Score = Int
type Lives = Int
type HighScore = (String, Int)

-- Display data types

type Position = Point
type Velocity = Vector

data Size = Size { w :: Float
                 , h :: Float } deriving (Eq)

type Sprite = Picture

-- Animation data types
type Img = Int
type LifeTime = Int

-- Game object related data types
-- Union all gameobjects in one data type
data GameObjects = Player Player 
                 | PlayerObjects PlayerObjects
                 | LevelObjects LevelObjects 
                 | EnemyObjects EnemyObjects
                 | AnimationObjects AnimationObjects deriving (Eq)

data ObjectInfo     = ObjectInfo { c    :: Color
                                 , vel  :: Velocity
                                 , pos  :: Position
                                 , size :: Size 
                                 } deriving (Eq)
data PlayerObjects    = Arrow ObjectInfo deriving (Eq)
data LevelObjects     = Wall ObjectInfo deriving (Eq)
data EnemyObjects     = Ball ObjectInfo deriving (Eq)
data AnimationObjects = Animation ObjectInfo Img LifeTime deriving (Eq)

-- Player related data types
data Player     = P1 PlayerInfo 
                | P2 PlayerInfo
data IsShooting = Yes 
                | No deriving (Eq)
data PlayerInfo = PlayerInfo { objectinfo :: ObjectInfo
                             , score      :: Score
                             , shooting   :: IsShooting
                             , lives      :: Lives 
                             } deriving (Eq)

instance Eq Player where
    (P1 _) == (P1 _) = True
    (P2 _) == (P2 _) = True
    _ == _ = False