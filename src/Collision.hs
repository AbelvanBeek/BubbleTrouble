module Collision where

import HelperFunctions
import Model
import Data.List
import Data.Maybe

checkNoRoofCollision :: GameObjects -> Bool
checkNoRoofCollision o@(PlayerObjects _) = (getY (getPosition o) < -720)
checkNoRoofCollision (EnemyObjects (Ball (ObjectInfo _ (vx,vy) (x,y) (Size w h) ))) | ((vy + y) > (350 - adjustsize)) = False
                                                                                    | otherwise = True
                                                                                    where adjustsize = halfBallSprite * w

splitb :: GameObjects -> [GameObjects] -- split into two half balls if their size is bigger than 0.2
splitb (EnemyObjects (Ball (ObjectInfo a (x,y) c (Size w h) ))) | w > 0.2 = ((EnemyObjects (Ball (ObjectInfo a ((-ballSpeed), ballHitY - 2 * w) c halfsize))) 
                                                                            : (EnemyObjects (Ball (ObjectInfo a (ballSpeed, ballHitY - 2 * w) c halfsize))) : [])
                                                                | otherwise = []
                                                                where halfsize = Size (w / 2) (h / 2)
--Return a list with tuples containing the index of the arrow that hit a ball and the ball that was hit
collisionindices :: Int -> [GameObjects] -> [GameObjects] -> [(Int,Int)]
collisionindices _ [] _ = []                
                                            --if there is no collision, check the next arrow.
collisionindices x (arrow:arrows) enemies | (collided arrow enemies) == Nothing = collisionindices (x+1) arrows enemies
                                            --if it collided return its own index + the index of the ball hit
                                          | otherwise = (x, (fromJust (collided arrow enemies))) : collisionindices (x+1) arrows enemies
                                          
-- Arrow checks collision with every ball, and returns the index of the ball it hit.
collided :: GameObjects -> [GameObjects] -> Maybe Int
collided arrow [] = Nothing
collided arrow balls = findIndex (collision arrow) balls

-- Actual collision check between an arrow and a ball.
collision :: GameObjects -> GameObjects -> Bool 
collision (PlayerObjects (Arrow (ObjectInfo _ (avx,avy) (ax, ay) _))) (EnemyObjects (Ball (ObjectInfo _ (bvx,bvy) (bx, by) (Size w h))))
            --check if the top of the arrow is above the ball and the ball is withing 8 pixels of the arrow
            | (((ay + avy + 1078) > (by + bvy - adjustsize)) && (abs (ax - (bx + bvx))) < 8 + adjustsize) = True
            --if not there is no collision
            | otherwise = False
                  where adjustsize = halfBallSprite * w
collision (Player (P1 (PlayerInfo (ObjectInfo _ (pvx,pvy) (px,py) _) _ _ _))) (EnemyObjects (Ball (ObjectInfo _ (bvx,bvy) (bx, by) (Size w h))))
            --pythagoras over the absolute distance between the player and the ball
            | (sqrt ((xdistance*xdistance) + (ydistance * ydistance) ) < halfPlayerSprite + adjustsize) = True
            | otherwise = False
                  where adjustsize = halfBallSprite * w
                        xdistance = abs ((px + pvx) - (bx + bvx))
                        ydistance = abs ((py + pvy) - (by + bvy))
            --the same for player 2
collision (Player (P2 (PlayerInfo (ObjectInfo _ (pvx,pvy) (px,py) _) _ _ _))) (EnemyObjects (Ball (ObjectInfo _ (bvx,bvy) (bx, by) (Size w h))))
            | (sqrt ((xdistance*xdistance) + (ydistance * ydistance) ) < halfPlayerSprite + adjustsize) = True
            | otherwise = False
                  where adjustsize = halfBallSprite * w
                        xdistance = abs ((px + pvx) - (bx + bvx))
                        ydistance = abs ((py + pvy) - (by + bvy))
--Checks if a player was hit by a ball.
checkPlayerCollided :: Level -> Maybe GameObjects
checkPlayerCollided (Level p1@(Player (P1 (PlayerInfo d1 o1 n1 p1life))) p1o p2@(Player (P2 (PlayerInfo d2 o2 n2 p2life))) p2o enemies lvl ani pics)
      | p1hit = Just (Player (P1 (PlayerInfo d1 o1 n1 (max (p1life -1) 0 ))))
      | p2hit = Just (Player (P2 (PlayerInfo d2 o2 n2 (max (p2life -1) 0 ))))
      | otherwise = Nothing
      where p1hit = checkPlayerCollision enemies p1
            p2hit = checkPlayerCollision enemies p2

--returns true if the player hit one of the balls
checkPlayerCollision :: [GameObjects] -> GameObjects -> Bool
checkPlayerCollision balls player = elem True (map (collision player) balls)

checkSideCollision, checkFloorCollision :: ObjectInfo -> Bool
--check collision with the side of the map
checkSideCollision (ObjectInfo _ (vx,vy) (px, py) (Size w h))     | (px + vx) < ((-halfScreenWidth) + adjustsize) = True 
                                                                  | (px + vx) > (halfScreenWidth - adjustsize) = True
                                                                  | otherwise = False
                                                                        where adjustsize = halfBallSprite * w
--check collision with the floor of the map
checkFloorCollision (ObjectInfo _ (vx,vy) (px, py) (Size w h))    | (py + vy) < (-halfScreenHeight + adjustsize) = True
                                                                  | otherwise = False
                                                                        where adjustsize = halfBallSprite * w