module HelperFunctions where

import Graphics.Gloss
import Model

--constants 
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

screenHeight :: Float
screenHeight = 720

halfScreenHeight :: Float
halfScreenHeight = 360

halfScreenWidth :: Float
halfScreenWidth = 640

scoreModifier :: Int
scoreModifier = 500

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

--take a list of indices and a second list, and return that list without the given indices.
filterindices :: [Int] -> [a] -> [a]
filterindices [] a = a
filterindices _ [] = []
filterindices [x] list = (fst split) ++ (tail (snd split))
      where split = splitAt x list 
filterindices (x:xs) list = (fst split) ++ (tail (snd split)) ++ (filterindices xs list)
      where split = splitAt x list

--object creation      
createExplosions :: [GameObjects] -> [GameObjects] --List of enemies to list of Animations
createExplosions balls = map explosion ballpositions
                            where ballpositions = map getPosition balls
                                  explosion (x,y) = AnimationObjects(Animation (ObjectInfo red (0,0) (x,y) (Size 1 1)) 9 9)

createArrow :: GameObjects -> [GameObjects] -> [GameObjects]
createArrow player xs
        = (PlayerObjects (Arrow (ObjectInfo red (0,7) ((getX (getPosition player)),-1388) (Size 1 1)))) : xs

--Function used to set the velocity of the players.
newVelocity :: Float -> Float -> GameObjects -> GameObjects
newVelocity x y (Player (P1 (PlayerInfo (ObjectInfo d (vx,vy) o n) t c a)))  = (Player (P1 (PlayerInfo (ObjectInfo d ((x),(y)) o n) t c a)))
newVelocity x y (Player (P2 (PlayerInfo (ObjectInfo d (vx,vy) o n) t c a)))  = (Player (P2 (PlayerInfo (ObjectInfo d ((x),(y)) o n) t c a)))
