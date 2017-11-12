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
  draw pic o@(Player        (P1 (PlayerInfo objectinfo score _ l))) = let x = translate (-600) 300 $ scale 0.2 0.2 $ color red $ text (y ++ show score ++ "   " ++ z ++ show l) 
                                                                          y = "Player 1: " 
                                                                          z = "Lifes: " in 
                                                                      if (l /= 0) then do sprite <- setSprite (getSprite o pic) objectinfo
                                                                                          return $ pictures [sprite, x]
                                                                                  else return x
  draw pic o@(Player        (P2 (PlayerInfo objectinfo score _ l))) = let x = translate 200 300 $ scale 0.2 0.2 $ color blue $ text (y ++ show score ++ "   " ++ z ++ show l)
                                                                          y = "Player 2: " 
                                                                          z = "Lifes: " in 
                                                                      if (l /= 0) then do sprite <- setSprite (getSprite o pic) objectinfo
                                                                                          return $ pictures [sprite, x]
                                                                                  else return x
  draw pic o@(PlayerObjects (Arrow          objectinfo))        = setSprite (getSprite o pic) objectinfo
  draw pic o@(EnemyObjects  (Ball           objectinfo))        = setSprite (getSprite o pic) objectinfo
  draw pic o@(LevelObjects  (Wall           objectinfo))        = setSprite (getSprite o pic) objectinfo
  draw pic o@(AnimationObjects (Animation   objectinfo img _))  = setSprite (getSprite o pic) objectinfo

  --gets the correct sprite from the list of IO Pictures stored in level
getSprite :: GameObjects -> [IO Picture] -> IO Picture
getSprite o@(Player (P1 _)) pics
  | (getX $ getVelocity o) < 0 = pics !! 0
  | (getX $ getVelocity o) > 0 = pics !! 2
  | otherwise                  = pics !! 1
getSprite o@(Player (P2 _)) pics
  | (getX $ getVelocity o) < 0 = pics !! 3
  | (getX $ getVelocity o) > 0 = pics !! 5
  | otherwise                  = pics !! 4
getSprite (PlayerObjects(Arrow _)) pics = pics !! 6
getSprite (EnemyObjects(Ball _)) pics = pics !! 7
getSprite (LevelObjects(Wall _)) pics = pics !! 8
getSprite (AnimationObjects(Animation _ img _)) pics = pics !! img

setSprite :: IO Picture -> ObjectInfo -> IO Picture
setSprite picture (ObjectInfo _ _ (px,py) (Size w h)) = do pic <- picture
                                                           return $ translate px py $ scale w h pic