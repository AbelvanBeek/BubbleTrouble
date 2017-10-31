module DrawLogic where

import Graphics.Gloss
import Graphics.Gloss.Juicy

import Model
import HelperFunctions

-- Given a level, all objects in that level will be drawn
drawLevel :: Level -> [IO Picture]
drawLevel (Level p1 p1o p2 p2o enemies lvl) 
    = (map draw p1o) ++ (map draw p2o) ++ [draw p1] ++ [draw p2] ++ (map draw enemies) ++ (map draw lvl)

-- Given an object to draw, will return the correct IO picture for that object
class Draw a where 
  draw :: a -> IO Picture

instance Draw GameObjects where
  draw o@(Player        (P1 (PlayerInfo objectinfo _ _ _))) = setSprite (getFilePath o) objectinfo
  draw o@(Player        (P2 (PlayerInfo objectinfo _ _ _))) = setSprite (getFilePath o) objectinfo
  draw o@(PlayerObjects (Arrow          objectinfo))        = setSprite (getFilePath o) objectinfo
  draw o@(EnemyObjects  (Ball           objectinfo))        = setSprite (getFilePath o) objectinfo
  draw o@(LevelObjects  (Wall           objectinfo))        = setSprite (getFilePath o) objectinfo

getFilePath :: GameObjects -> FilePath
getFilePath (Player _) = "assets/ball.png"
getFilePath (PlayerObjects(Arrow _)) = "assets/arrow.png"
getFilePath (EnemyObjects(Ball _)) = "assets/ball.png"
getFilePath (LevelObjects(Wall _)) = "assets/ball.png"

setSprite :: FilePath -> ObjectInfo -> IO Picture
setSprite path (ObjectInfo c (vx,vy) (px,py) (Size w h)) = do mbpic <- loadJuicyPNG path
                                                              return $ color c $ translate px py $ scale w h $ maybePicToIO mbpic