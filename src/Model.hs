-- | This module contains the data types
--   which represent the state of the game
module Model where

-- Main game status data types
data GameStatus = Menu 
                | Play
                | Pause 
                | GameOver

data GameState = GameState {
                   gameStatus  :: GameStatus
                 , elapsedTime :: Float
                 }

-- Level/Score related data types
type Level = [Gameobject]

data Score = Score Int
data Lives = Lives Int

-- Display data types
data Velocity = Velocity { xvel :: Float, yvel :: Float }
data Position = Position { xpos :: Int, ypos :: Int }

data Size = Radius { r :: Float } | Rectangle { w :: Float, h :: Float }
data Sprite = Picture
type Animation = [Sprite]

-- Game object related data types
data Objectinfo = Color Velocity Position Sprite Size
data Gameobject = Wall Objectinfo | Ball Objectinfo | Player PlayerInfo| Arrow Objectinfo

-- Player related data types
data IsShooting = True | False
data PlayerInfo = PlayerInfo Objectinfo Score IsShooting Lives
data Player = P1 PlayerInfo | P2 PlayerInfo

initialState :: GameState
initialState = GameState Menu 0

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5
