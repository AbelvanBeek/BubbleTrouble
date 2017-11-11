module DrawLogic where

import Graphics.Gloss
import Graphics.Gloss.Juicy

import Model
import HelperFunctions
import LoadPictures

-- Given a level, all objects in that level will be drawn
drawLevel :: Level -> [IO Picture]
drawLevel (Level p1 p1o p2 p2o enemies lvl) 
    = (map draw p1o) ++ (map draw p2o) ++ [draw p1] ++ [draw p2] ++ (map draw enemies) ++ (map draw lvl)

-- Given an object to draw, will return the correct IO picture for that object
class Draw a where 
  draw :: a -> IO Picture

instance Draw GameObjects where
  draw o@(Player        (P1 (PlayerInfo objectinfo score _ _))) = let x = translate (-600) 300 $ scale 0.2 0.2 $ color red $ text (y ++ show score) 
                                                                      y = "Player 1: " in 
                                                                  do sprite <- setSprite (getFilePath o) objectinfo
                                                                     return $ pictures [sprite, x]
  draw o@(Player        (P2 (PlayerInfo objectinfo score _ _))) = let x = translate 400 300 $ scale 0.2 0.2 $ color blue $ text (y ++ show score)
                                                                      y = "Player 2: " in 
                                                                  do sprite <- setSprite (getFilePath o) objectinfo
                                                                     return $ pictures [sprite, x]
  draw o@(PlayerObjects (Arrow          objectinfo))        = setSprite (getFilePath o) objectinfo
  draw o@(EnemyObjects  (Ball           objectinfo))        = setSprite (getFilePath o) objectinfo
  draw o@(LevelObjects  (Wall           objectinfo))        = setSprite (getFilePath o) objectinfo

getFilePath :: GameObjects -> IO Picture
getFilePath o@(Player _) 
  | (getX $ getVelocity o) < 0 = loadPictures !! 0
  | (getX $ getVelocity o) > 0 = loadPictures !! 1
  | otherwise                  = loadPictures !! 2
  {-
getFilePath o@(Player (P1 (PlayerInfo _ _ isShooting _)))
  | isShooting == Yes          = loadPictures !! 4
  | (getX $ getVelocity o) < 0 = loadPictures !! 0
  | (getX $ getVelocity o) > 0 = loadPictures !! 1
  | otherwise                  = loadPictures !! 2
getFilePath o@(Player (P2 (PlayerInfo _ _ isShooting _)))
  | isShooting == Yes          = loadPictures !! 4
  | (getX $ getVelocity o) < 0 = loadPictures !! 0
  | (getX $ getVelocity o) > 0 = loadPictures !! 1
  | otherwise                  = loadPictures !! 2
  -}
getFilePath (PlayerObjects(Arrow _)) = loadPictures !! 3
getFilePath (EnemyObjects(Ball _)) = loadPictures !! 4
getFilePath (LevelObjects(Wall _)) = loadPictures !! 4

setSprite :: IO Picture -> ObjectInfo -> IO Picture
setSprite picture (ObjectInfo c (vx,vy) (px,py) (Size w h)) = do pic <- picture
                                                                 return $ color c $ translate px py $ scale w h pic