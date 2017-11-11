module DrawLogic where

import Graphics.Gloss
import Graphics.Gloss.Juicy

import Model
import HelperFunctions
import LoadPictures

-- Given a level, all objects in that level will be drawn
drawLevel :: Level -> [IO Picture]
drawLevel (Level p1 p1o p2 p2o enemies lvl ani pics) 
    = (map (draw pics) lvl) ++ (map (draw pics) ani) ++ (map (draw pics) p1o) ++ (map (draw pics) p2o) ++ [draw pics p1] ++ [draw pics p2] ++ (map (draw pics) enemies)

-- Given an object to draw, will return the correct IO picture for that object
class Draw a where 
  draw :: [IO Picture] -> a -> IO Picture

instance Draw GameObjects where
  draw pic o@(Player        (P1 (PlayerInfo objectinfo score _ _))) = let x = translate (-600) 300 $ scale 0.2 0.2 $ color red $ text (y ++ show score) 
                                                                          y = "Player 1: " in 
                                                                      do sprite <- setSprite (getFilePath o pic) objectinfo
                                                                         return $ pictures [sprite, x]
  draw pic o@(Player        (P2 (PlayerInfo objectinfo score _ _))) = let x = translate 400 300 $ scale 0.2 0.2 $ color blue $ text (y ++ show score)
                                                                          y = "Player 2: " in 
                                                                      do sprite <- setSprite (getFilePath o pic) objectinfo
                                                                         return $ pictures [sprite, x]
  draw pic o@(PlayerObjects (Arrow          objectinfo))        = setSprite (getFilePath o pic) objectinfo
  draw pic o@(EnemyObjects  (Ball           objectinfo))        = setSprite (getFilePath o pic) objectinfo
  draw pic o@(LevelObjects  (Wall           objectinfo))        = setSprite (getFilePath o pic) objectinfo
  draw pic o@(AnimationObjects (Animation   objectinfo img _))  = setSprite (getFilePath o pic) objectinfo

getFilePath :: GameObjects -> [IO Picture] -> IO Picture
getFilePath o@(Player (P1 _)) pics
  | (getX $ getVelocity o) < 0 = pics !! 0
  | (getX $ getVelocity o) > 0 = pics !! 1
  | otherwise                  = pics !! 2
getFilePath o@(Player (P2 _)) pics
  | (getX $ getVelocity o) < 0 = pics !! 0
  | (getX $ getVelocity o) > 0 = pics !! 1
  | otherwise                  = pics !! 2
getFilePath (PlayerObjects(Arrow _)) pics = pics !! 6
getFilePath (EnemyObjects(Ball _)) pics = pics !! 7
getFilePath (LevelObjects(Wall _)) pics = pics !! 8
getFilePath (AnimationObjects(Animation _ img _)) pics = pics !! img

setSprite :: IO Picture -> ObjectInfo -> IO Picture
setSprite picture (ObjectInfo c (vx,vy) (px,py) (Size w h)) = do pic <- picture
                                                                 return $ color c $ translate px py $ scale w h pic