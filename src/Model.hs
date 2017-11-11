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
                   }

type Score = Int
type Lives = Int
type HighScore = (String, Int)

-- Display data types

type Position = Point
type Velocity = Vector

data Size = Size { w :: Float
                 , h :: Float }

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
                 | AnimationObjects AnimationObjects

data ObjectInfo     = ObjectInfo { c    :: Color
                                 , vel  :: Velocity
                                 , pos  :: Position
                                 , size :: Size 
                                 }
data PlayerObjects    = Arrow ObjectInfo
data LevelObjects     = Wall ObjectInfo 
data EnemyObjects     = Ball ObjectInfo 
data AnimationObjects = Animation ObjectInfo Img LifeTime

-- Player related data types
data Player     = P1 PlayerInfo 
                | P2 PlayerInfo
data IsShooting = Yes 
                | No deriving (Eq)
data PlayerInfo = PlayerInfo { objectinfo :: ObjectInfo
                             , score      :: Score
                             , shooting   :: IsShooting
                             , lives      :: Lives 
                             }