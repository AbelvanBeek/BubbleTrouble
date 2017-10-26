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
                  p1 :: GameObjects
                , p1Objects :: [GameObjects]
                , p2 :: GameObjects
                , p2Objects :: [GameObjects]
                , enemies :: [GameObjects]
                , lvl :: [GameObjects]
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
data GameObjects = Player Player| PlayerObjects PlayerObjects| LevelObjects LevelObjects | EnemyObjects EnemyObjects -- Wel kut met extra constuctor...

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
                                (Player(P1 (PlayerInfo (ObjectInfo red (Vec 0 0) (Pt 0 0) (Size 1 1)) 0 No 5)))
                                [] 
                                (Player(P2 (PlayerInfo (ObjectInfo red (Vec 0 0) (Pt 0 0) (Size 1 1)) 0 No 5)))
                                [] 
                                [EnemyObjects(Ball (ObjectInfo red (Vec 0 0) (Pt 0 0) (Size 1 1)))] 
                                []) 0


initialPlay :: GameState
initialPlay = GameState Play (Level 
                                (Player(P1 (PlayerInfo (ObjectInfo red (Vec 0 0) (Pt 0 0) (Size 1 1)) 0 No 5)))
                                [] 
                                (Player(P2 (PlayerInfo (ObjectInfo red (Vec 0 0) (Pt 0 0) (Size 1 1)) 0 No 5)))
                                [] 
                                [] 
                                []) 0

initialGameOver :: GameState
initialGameOver = GameState GameOver (Level 
                                        (Player(P1 (PlayerInfo (ObjectInfo red (Vec 0 0) (Pt 0 0) (Size 1 1)) 0 No 5)))
                                        [] 
                                        (Player(P2 (PlayerInfo (ObjectInfo red (Vec 0 0) (Pt 0 0) (Size 1 1)) 0 No 5)))
                                        [] 
                                        [] 
                                        []) 0


-- initial pause not neccesary? Just freeze the current game -> No update velocity etc anymore

{- Type Classes -}

-- update locations
class Update a where 
  update :: a -> a

instance Update GameObjects where
  update o@(Player(P1 (PlayerInfo objectinfo _ _ _))) = o
  update o@(Player(P2 (PlayerInfo objectinfo _ _ _))) = o
  update o@(PlayerObjects(Arrow objectinfo))      = o
  update o@(EnemyObjects(Ball (ObjectInfo clr vec (Pt x y) size))) = EnemyObjects(Ball (ObjectInfo clr vec (Pt (x+1) y) size))
  update o@(LevelObjects(Wall objectinfo))       = o

updateObjects :: [GameObjects] -> [GameObjects]
updateObjects x = map update x

-- Update position somewhere and reset velocity?

-- Given a level, all objects in that level will be drawn
drawLevel :: Level -> [IO Picture]
drawLevel (Level p1 p1o p2 p2o enemies lvl) = [draw p1] ++ (map draw p1o) ++ [draw p2] ++ (map draw p2o) ++ (map draw enemies) ++ (map draw lvl)

-- Given an object to draw, will return the correct IO picture for that object
class Draw a where 
  draw :: a -> IO Picture

instance Draw GameObjects where
  draw o@(Player(P1 (PlayerInfo objectinfo _ _ _))) = setSprite (getFilePath o) objectinfo
  draw o@(Player(P2 (PlayerInfo objectinfo _ _ _))) = setSprite (getFilePath o) objectinfo
  draw o@(PlayerObjects(Arrow objectinfo))      = setSprite (getFilePath o) objectinfo
  draw o@(EnemyObjects(Ball objectinfo))       = setSprite (getFilePath o) objectinfo
  draw o@(LevelObjects(Wall objectinfo))       = setSprite (getFilePath o) objectinfo

getFilePath :: GameObjects -> FilePath
getFilePath (Player _) = "assets/ball.png"
getFilePath (PlayerObjects(Arrow _)) = "assets/ball.png"
getFilePath (EnemyObjects(Ball _)) = "assets/ball.png"
getFilePath (LevelObjects(Wall _)) = "assets/ball.png"

setSprite :: FilePath -> ObjectInfo -> IO Picture
setSprite path (ObjectInfo c (Vec vx vy) (Pt px py) (Size w h)) = do mbpic <- loadJuicyPNG path
                                                                     return $ translate px py $ maybePicToIO mbpic

{- Helper Functions-}

-- Convert a maybe picture to a picture -> Blank picture in case of Nothing
maybePicToIO :: Maybe Picture -> Picture
maybePicToIO Nothing  = blank
maybePicToIO (Just x) = x

{-
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
-}
