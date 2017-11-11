module UpdateLogic where
    
import Model
import HelperFunctions
import Data.List
import Data.Maybe
import Data.Fixed
import Graphics.Gloss

class Update a where 
    update :: a -> a
  
instance Update GameObjects where
    update (Player           (P1 (PlayerInfo                 objectinfo  d o n)))
          = Player           (P1 (PlayerInfo (updatePosition objectinfo) d o n))
    update (Player           (P2 (PlayerInfo                 objectinfo  d o n)))
          = Player           (P2 (PlayerInfo (updatePosition objectinfo) d o n))
    update (PlayerObjects    (Arrow                          objectinfo))
          = PlayerObjects    (Arrow          (updatePosition objectinfo))
    update (EnemyObjects     (Ball objectinfo)) 
          = EnemyObjects     (Ball           (updatePosition (adjustVelocity (0) (-0.05) objectinfo)))
    update (LevelObjects     (Wall                           objectinfo))
          = LevelObjects     (Wall           (updatePosition objectinfo))

updateAnimation :: Float -> GameObjects -> GameObjects
updateAnimation secs o@(AnimationObjects (Animation objectinfo img lifetime))
          | secs `mod'` 0.08 < 0.015 && lifetime /= 0 = AnimationObjects (Animation objectinfo (img + 1) (lifetime - 1))
          | otherwise = o

filterAnimation :: GameObjects -> Bool
filterAnimation (AnimationObjects (Animation _ _ lifetime)) | lifetime == 0 = False
                                                            | otherwise = True

updateLevel :: Float -> Level -> Level
updateLevel secs (Level p1 p1o p2 p2o enemies lvl ani) 
            = handleBallCollisions (Level (update p1) (map update p1o) (update p2) (map update p2o) (map update enemies) (map update lvl) (map (updateAnimation secs) ani))

filterLevel :: Level -> Level
filterLevel (Level p1 p1o p2 p2o enemies lvl ani) 
            = Level p1 (removeOutOfBounds p1o) p2 (removeOutOfBounds p2o) (filter checkNoRoofCollision enemies) lvl (filter filterAnimation ani)

removeOutOfBounds :: [GameObjects] -> [GameObjects]
removeOutOfBounds xs = filter checkInBounds xs

checkInBounds :: GameObjects -> Bool
checkInBounds o@(PlayerObjects _) = (getY (getPosition o) < -720)
checkInBounds obj = (getY (getPosition obj) < -720)

checkNoRoofCollision :: GameObjects -> Bool
checkNoRoofCollision (EnemyObjects (Ball (ObjectInfo _ (vx,vy) (x,y) (Size w h) ))) | ((vy + y) > (350 - adjustsize)) = False
                                                                                    | otherwise = True
                                                                                           where adjustsize = halfBallSprite * w * w
                                          
handleBallCollisions ::  Level -> Level
handleBallCollisions (Level p1 p1o p2 p2o enemies lvl ani) = (Level (updateScore p1score p1) (filterindices p1arrowindices p1o) (updateScore p2score p2) (filterindices p2arrowindices p2o) (splitballs balls enemies) lvl (ani ++ createExplosions hitballs))
      where p1indices = collisionindices 0 p1o enemies 
            p1arrowindices =  (sort (map fst p1indices)) --Indices of all p1Arrows that hit a ball
            p2indices = collisionindices 0 p2o enemies 
            p2arrowindices =  (sort (map fst p2indices)) --Indices of all p1Arrows that hit a ball
            balls = nub (sort ((map snd p1indices) ++ (map snd p2indices)))
            p1score = (length p1indices) * 500
            p2score = (length p2indices) * 500
            hitballs = map (enemies !!) balls

createExplosions :: [GameObjects] -> [GameObjects] --List of enemies to list of Animations
createExplosions balls = map explosion ballpositions
                        where ballpositions = map getPosition balls
                              explosion (x,y) = AnimationObjects(Animation (ObjectInfo red (0,0) (x,y) (Size 1 1)) 1 9)
updateScore :: Int -> GameObjects -> GameObjects
updateScore n (Player (P1 (PlayerInfo a score b c))) = (Player (P1 (PlayerInfo a (score + n) b c)))
updateScore n (Player (P2 (PlayerInfo a score b c))) = (Player (P2 (PlayerInfo a (score + n) b c)))

filterindices :: [Int] -> [a] -> [a]
filterindices [] a = a
filterindices _ [] = []
filterindices [x] list = (fst split) ++ (tail (snd split))
      where split = splitAt x list 
filterindices (x:xs) list = (fst split) ++ (tail (snd split)) ++ (filterindices xs list)
      where split = splitAt x list

splitballs :: [Int] -> [GameObjects] -> [GameObjects]
splitballs [] a = a
splitballs _ [] = []
splitballs [x] list = (fst split) ++ (tail (snd split)) ++ (splitb (list!!x))
      where split = splitAt x list 
splitballs (x:xs) list = (fst split) ++ (tail (snd split)) ++ (filterindices xs list)
      where split = splitAt x list

splitb :: GameObjects -> [GameObjects] -- split into two half balls unless their size is smaller than 0.2
splitb (EnemyObjects (Ball (ObjectInfo a (x,y) c (Size w h) ))) | w > 0.2 = ((EnemyObjects (Ball (ObjectInfo a ((-ballSpeed), ballHitY - 2 * w) c halfsize))) 
                                                                              : (EnemyObjects (Ball (ObjectInfo a (ballSpeed, ballHitY - 2 * w) c halfsize))) : [])
                                                                | otherwise = []
      where halfsize = Size (w / 2) (h / 2)
--Return a list with tuples containing the index of the arrow that hit a ball and the ball that was hit
collisionindices :: Int -> [GameObjects] -> [GameObjects] -> [(Int,Int)]
collisionindices _ [] _ = []
collisionindices x (arrow:arrows) enemies | (collided arrow enemies) == Nothing = collisionindices (x+1) arrows enemies
                                          | otherwise = (x, (fromJust (collided arrow enemies))) : collisionindices (x+1) arrows enemies
                                          --if it collided return its own index + the index of the ball hit
-- Arrow checks collision with every ball, and returns the index of the ball it hit.
collided :: GameObjects -> [GameObjects] -> Maybe Int
collided arrow [] = Nothing
collided arrow balls = findIndex (collision arrow) balls

-- Actual collision check between an arrow and a ball.
collision :: GameObjects -> GameObjects -> Bool 
collision (PlayerObjects (Arrow (ObjectInfo _ (avx,avy) (ax, ay) _))) (EnemyObjects (Ball (ObjectInfo _ (bvx,bvy) (bx, by) (Size w h))))
            | (((ay + avy + 1078) > (by + bvy - adjustsize)) && (abs (ax - (bx + bvx))) < 8 + adjustsize) = True
            --                                                      absolute x dist between arrow and ball smaller than width of the arrow
            | otherwise = False
                  where adjustsize = halfBallSprite * w
collision (Player (P1 (PlayerInfo (ObjectInfo _ (pvx,pvy) (px,py) _) _ _ _))) (EnemyObjects (Ball (ObjectInfo _ (bvx,bvy) (bx, by) (Size w h))))
            | (sqrt ((xdistance*xdistance) + (ydistance * ydistance) ) < halfPlayerSprite + adjustsize) = True
            | otherwise = False
            -- needs to be changed with player sprite
                  where adjustsize = halfBallSprite * w
                        xdistance = abs ((px + pvx) - (bx + bvx))
                        ydistance = abs ((py + pvy) - (by + bvy))
collision (Player (P2 (PlayerInfo (ObjectInfo _ (pvx,pvy) (px,py) _) _ _ _))) (EnemyObjects (Ball (ObjectInfo _ (bvx,bvy) (bx, by) (Size w h))))
            | (sqrt ((xdistance*xdistance) + (ydistance * ydistance) ) < halfPlayerSprite + adjustsize) = False
            | otherwise = False
            --50 needs to be changed with player sprite
                  where adjustsize = halfBallSprite * w
                        xdistance = abs ((px + pvx) - (bx + bvx))
                        ydistance = abs ((py + pvy) - (by + bvy))

checkGameOver :: Level -> Bool
checkGameOver (Level p1 p1o p2 p2o enemies lvl ani) = (checkPlayerCollision enemies p1) || (checkPlayerCollision enemies p2)

checkPlayerCollision :: [GameObjects] -> GameObjects -> Bool
checkPlayerCollision balls player = elem True (map (collision player) balls)

updatePosition :: ObjectInfo -> ObjectInfo
updatePosition x@(ObjectInfo clr (vx,vy) (px,py) size) | checkSideCollision x = (ObjectInfo clr (0,vy) ((px),(py)) size)
                                                       | otherwise = (ObjectInfo clr (vx,vy) ((px+vx),(py+vy)) size)



adjustVelocity :: Float -> Float -> ObjectInfo -> ObjectInfo
adjustVelocity x y obj@(ObjectInfo clr (vx,vy) pos size@(Size w h)) | (checkSideCollision obj) && (checkFloorCollision obj) = (ObjectInfo clr ((-(vx + x)),(ballHitY * (sqrt (3 * w)))) pos size)
                                                                    | (checkSideCollision obj) = (ObjectInfo clr ((-(vx + x)),(vy + y)) pos size)
                                                                    | (checkFloorCollision obj) = (ObjectInfo clr ((vx + x),(ballHitY *(sqrt (sqrt (3 * w))))) pos size)
                                                                    | otherwise = (ObjectInfo clr (((vx + x)),(vy + y)) pos size)





