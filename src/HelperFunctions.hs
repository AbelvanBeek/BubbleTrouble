module HelperFunctions where

import Graphics.Gloss
import Model

playerSpeed :: Float
playerSpeed = 5

arrowAmount :: Int
arrowAmount = 1
ballSpeed :: Float
ballSpeed = 3

ballHitY :: Float
ballHitY = 5

halfBallSprite :: Float
halfBallSprite = 50

halfPlayerSprite :: Float
halfPlayerSprite = 30

-- Convert a maybe picture to a picture -> Blank picture in case of Nothing
maybePicToPic :: Maybe Picture -> Picture
maybePicToPic Nothing  = blank
maybePicToPic (Just x) = x

getPosition, getVelocity :: GameObjects -> Point
getPosition (Player (P1 (PlayerInfo objectinfo _ _ _))) = getPos objectinfo
getPosition (Player (P2 (PlayerInfo objectinfo _ _ _))) = getPos objectinfo
getPosition (PlayerObjects (Arrow objectinfo)) = getPos objectinfo
getPosition (EnemyObjects (Ball objectinfo)) = getPos objectinfo
getVelocity (Player (P1 (PlayerInfo objectinfo _ _ _))) = getVel objectinfo
getVelocity (Player (P2 (PlayerInfo objectinfo _ _ _))) = getVel objectinfo


getPos, getVel :: ObjectInfo -> Point
getPos (ObjectInfo _ _ point _) = point
getVel (ObjectInfo _ velocity _ _) = velocity

getY, getX :: Point -> Float
getY (x,y) = y
getX (x,y) = x

newVelocity :: Float -> Float -> GameObjects -> GameObjects
newVelocity x y (Player (P1 (PlayerInfo (ObjectInfo d (vx,vy) o n) t c a)))  | checkSideCollision (ObjectInfo d ((x),(y)) o n) = (Player (P1 (PlayerInfo (ObjectInfo d ((x),(y)) o n) t c a)))
                                                                                   | otherwise = (Player (P1 (PlayerInfo (ObjectInfo d ((x),(y)) o n) t c a)))
newVelocity x y (Player (P2 (PlayerInfo (ObjectInfo d (vx,vy) o n) t c a)))  = (Player (P2 (PlayerInfo (ObjectInfo d ((x),(y)) o n) t c a)))

checkSideCollision, checkFloorCollision :: ObjectInfo -> Bool
checkSideCollision (ObjectInfo _ (vx,vy) (px, py) (Size w h))     | (px + vx) < ((-640) + adjustsize) = True 
                                                                  | (px + vx) > (640 - adjustsize) = True
                                                                  | otherwise = False
                                                                        where adjustsize = halfBallSprite * w * w
checkFloorCollision (ObjectInfo _ (vx,vy) (px, py) (Size w h))    | (py + vy) < (-360 + adjustsize) = True
                                                                  | otherwise = False
                                                                        where adjustsize = halfBallSprite * w * w