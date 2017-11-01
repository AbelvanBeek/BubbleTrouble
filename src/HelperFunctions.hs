module HelperFunctions where

import Graphics.Gloss
import Model

playerSpeed :: Float
playerSpeed = 5

-- Convert a maybe picture to a picture -> Blank picture in case of Nothing
maybePicToPic :: Maybe Picture -> Picture
maybePicToPic Nothing  = blank
maybePicToPic (Just x) = x

getPosition, getVelocity :: GameObjects -> Point
getPosition (Player (P1 (PlayerInfo objectinfo _ _ _))) = getPos objectinfo
getPosition (Player (P2 (PlayerInfo objectinfo _ _ _))) = getPos objectinfo
getPosition (PlayerObjects (Arrow objectinfo)) = getPos objectinfo
getVelocity (Player (P1 (PlayerInfo objectinfo _ _ _))) = getVel objectinfo
getVelocity (Player (P2 (PlayerInfo objectinfo _ _ _))) = getVel objectinfo

getPos, getVel :: ObjectInfo -> Point
getPos (ObjectInfo _ _ point _) = point
getVel (ObjectInfo _ velocity _ _) = velocity

getY, getX :: Point -> Float
getY (x,y) = y
getX (x,y) = x