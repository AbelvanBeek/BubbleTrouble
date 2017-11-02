module UpdateLogic where
    
import Model
import HelperFunctions
import Data.List
import Data.Maybe

class Update a where 
    update :: a -> a
  
instance Update GameObjects where
    update (Player         (P1 (PlayerInfo                 objectinfo  d o n)))
          = Player         (P1 (PlayerInfo (updatePosition objectinfo) d o n))
    update (Player         (P2 (PlayerInfo                 objectinfo  d o n)))
          = Player         (P2 (PlayerInfo (updatePosition objectinfo) d o n))
    update (PlayerObjects  (Arrow                          objectinfo))
          = PlayerObjects  (Arrow          (updatePosition objectinfo))
    update (EnemyObjects   (Ball objectinfo)) 
          = EnemyObjects   (Ball           (updatePosition (adjustVelocity (0) (-0.05) objectinfo)))
    update o@(LevelObjects (Wall                           objectinfo))
          = LevelObjects   (Wall           (updatePosition objectinfo))

updateLevel :: Level -> Level
updateLevel (Level p1 p1o p2 p2o enemies lvl) 
            = handleBallCollisions (Level (update p1) (map update p1o) (update p2) (map update p2o) (map update enemies) (map update lvl))

filterLevel :: Level -> Level
filterLevel (Level p1 p1o p2 p2o enemies lvl) 
            = Level p1 (removeOutOfBounds p1o) p2 (removeOutOfBounds p2o) (filter checkNoRoofCollision enemies) lvl

removeOutOfBounds :: [GameObjects] -> [GameObjects]
removeOutOfBounds xs = filter checkInBounds xs

checkInBounds :: GameObjects -> Bool
checkInBounds o@(PlayerObjects _) = (getY (getPosition o) < -720)
checkInBounds obj = (getY (getPosition obj) < -720)

checkSideCollision, checkFloorCollision :: ObjectInfo -> Bool
checkSideCollision (ObjectInfo _ (vx,vy) (px, py) (Size w h))     | (px + vx) < ((-640) + adjustsize) = True --50 is half the width of the sprite 
                                                                  | (px + vx) > (640 - adjustsize) = True
                                                                  | otherwise = False
                                                                        where adjustsize = halfBallSprite * w * w
checkFloorCollision (ObjectInfo _ (vx,vy) (px, py) (Size w h))    | (py + vy) < (-360 + adjustsize) = True
                                                                  | otherwise = False
                                                                        where adjustsize = halfBallSprite * w * w

checkNoRoofCollision :: GameObjects -> Bool
checkNoRoofCollision (EnemyObjects (Ball (ObjectInfo _ (vx,vy) (x,y) (Size w h) ))) | ((vy + y) > (360 - adjustsize)) = False
                                                                                    | otherwise = True
                                                                                           where adjustsize = halfBallSprite * w * w
                                          
handleBallCollisions ::  Level -> Level
handleBallCollisions (Level p1 p1o p2 p2o enemies lvl) = (Level p1 (filterindices p1arrowindices p1o) p2 (filterindices p2arrowindices p2o) (splitballs balls enemies) lvl)
      where p1indices = collisionindices 0 p1o enemies 
            p1arrowindices = reverse (sort (map fst p1indices)) --Indices of all p1Arrows that hit a ball
            p2indices = collisionindices 0 p2o enemies 
            p2arrowindices = reverse (sort (map fst p2indices)) --Indices of all p1Arrows that hit a ball
            balls = nub (reverse (sort ((map snd p1indices) ++ (map snd p2indices))))

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

updatePosition :: ObjectInfo -> ObjectInfo
updatePosition (ObjectInfo clr (vx,vy) (px,py) size) =  (ObjectInfo clr (vx,vy) ((px+vx),(py+vy)) size)

newVelocity :: Float -> Float -> GameObjects -> GameObjects
newVelocity x y (Player (P1 (PlayerInfo (ObjectInfo d (vx,vy) o n) t c a)))  = (Player (P1 (PlayerInfo (ObjectInfo d ((vx+x),(vy+y)) o n) t c a)))
newVelocity x y (Player (P2 (PlayerInfo (ObjectInfo d (vx,vy) o n) t c a)))  = (Player (P2 (PlayerInfo (ObjectInfo d ((vx+x),(vy+y)) o n) t c a)))

adjustVelocity :: Float -> Float -> ObjectInfo -> ObjectInfo
adjustVelocity x y obj@(ObjectInfo clr (vx,vy) pos size@(Size w h)) | (checkSideCollision obj) && (checkFloorCollision obj) = (ObjectInfo clr ((-(vx + x)),(ballHitY * (sqrt (3 * w)))) pos size)
                                                                    | (checkSideCollision obj) = (ObjectInfo clr ((-(vx + x)),(vy + y)) pos size)
                                                                    | (checkFloorCollision obj) = (ObjectInfo clr ((vx + x),(ballHitY *(sqrt (sqrt (3 * w))))) pos size)
                                                                    | otherwise = (ObjectInfo clr (((vx + x)),(vy + y)) pos size)





