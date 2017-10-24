-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

--import PngToPic
--import Data.ByteString as B

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
type Level = [Gameobject]

data Score = Score Int
data Lives = Lives Int

-- Display data types
data Velocity = Velocity { xvel :: Float, yvel :: Float }
data Position = Position { xpos :: Float, ypos :: Float }

data Size = Size { w :: Float, h :: Float }
-- data Sprite = Sprite Picture
newtype Animation = Animation { unAnimate :: [Picture] }

-- Game object related data types
data ObjectInfo = ObjectInfo Color Velocity Position Picture Size
data Gameobject = Wall ObjectInfo | Ball ObjectInfo | Player PlayerInfo| Arrow ObjectInfo

-- Player related data types
data IsShooting = True | False
data PlayerInfo = PlayerInfo ObjectInfo Score IsShooting Lives
data Player = P1 PlayerInfo | P2 PlayerInfo

{- Initialize States -}

initialMenu :: GameState
initialMenu = GameState Menu [Wall (ObjectInfo blue (Velocity 0 0) (Position 0 0) (Circle 50) (Size 1 1))] 0

initialPlay :: GameState
initialPlay = GameState Play [] 0

initialGameOver :: GameState
initialGameOver = GameState GameOver [] 0

-- initial pause not neccesary? Just freeze the current game -> No update velocity etc anymore

{- Type Classes -}

class Drawable a where 
  showSprite :: a -> Picture

instance Drawable Gameobject where
  showSprite (Player (PlayerInfo objectinfo _ _ _ ))  = getPic objectinfo
  showSprite (Wall objectinfo)                        = getPic objectinfo
  showSprite (Ball objectinfo)                        = getPic objectinfo
  showSprite (Arrow objectinfo)                       = getPic objectinfo

getPic :: ObjectInfo -> Picture
getPic (ObjectInfo c _ (Position x y) spr (Size w h)) = color c $ translate x y $ scale w h spr
