module UpdateLogic where
    
import Model
import HelperFunctions
import Data.List
import Data.Maybe
import Data.Fixed
import Graphics.Gloss
import Collision

class Update a where 
    update :: a -> a
  
instance Update GameObjects where
    update (Player           (P1 (PlayerInfo                 objectinfo  d o lives)))
          | lives <= 0 = Player          (P1 (PlayerInfo                 objectinfo {pos = (-1000,0)}  d o lives))
          | otherwise = Player           (P1 (PlayerInfo (updatePosition objectinfo) d o lives))
    update (Player           (P2 (PlayerInfo                 objectinfo  d o lives)))
          | lives <= 0 = Player          (P2 (PlayerInfo                 objectinfo {pos = (-1000,0)}  d o lives))
          | otherwise = Player           (P2 (PlayerInfo (updatePosition objectinfo) d o lives))
    update (PlayerObjects    (Arrow                          objectinfo))
          = PlayerObjects    (Arrow          (updatePosition objectinfo))
    update (EnemyObjects     (Ball objectinfo)) 
          = EnemyObjects     (Ball           (updatePosition (adjustVelocity (0) (-0.05) objectinfo)))
    update (LevelObjects     (Wall                           objectinfo))
          = LevelObjects     (Wall           (updatePosition objectinfo))

handleBallCollisions ::  Level -> Level
handleBallCollisions (Level p1 p1o p2 p2o enemies lvl ani pics) = (Level (updateScore p1score p1) (filterindices p1arrowindices p1o) (updateScore p2score p2) (filterindices p2arrowindices p2o) ((concat (map splitb hitballs)) ++ (filterindices balls enemies)) lvl (ani ++ createExplosions hitballs) pics)
      where p1indices = collisionindices 0 p1o enemies 
            p1arrowindices =  (sort (map fst p1indices)) --Indices of all p1Arrows that hit a ball
            p2indices = collisionindices 0 p2o enemies 
            p2arrowindices =  (sort (map fst p2indices)) --Indices of all p1Arrows that hit a ball
            balls = nub (sort ((map snd p1indices) ++ (map snd p2indices)))
            p1score = (length p1indices) * scoreModifier
            p2score = (length p2indices) * scoreModifier
            hitballs = map (enemies !!) balls -- list of ball gameobjects that were hit

--go to the next picture in the animation depending on the passed gametime.
updateAnimation :: Float -> GameObjects -> GameObjects
updateAnimation secs o@(AnimationObjects (Animation objectinfo img lifetime))
          | secs `mod'` 0.08 < 0.015 && lifetime /= 0 = AnimationObjects (Animation objectinfo (img + 1) (lifetime - 1))
          | otherwise = o

filterAnimation :: GameObjects -> Bool
filterAnimation (AnimationObjects (Animation _ _ lifetime)) | lifetime == 0 = False
                                                            | otherwise = True

updateLevel :: Float -> Level -> Level
updateLevel secs (Level p1 p1o p2 p2o enemies lvl ani pics) 
            = handleBallCollisions (Level (update p1) (map update p1o) (update p2) (map update p2o) (map update enemies) (map update lvl) (map (updateAnimation secs) ani) pics)

--remove anything that is out of bounds and update the score if balls hit the roof
filterLevel :: Level -> Level
filterLevel (Level p1 p1o p2 p2o enemies lvl ani pics) 
            = Level (updateScore score p1) (removeOutOfBounds p1o) (updateScore score p2) (removeOutOfBounds p2o) (filteredenemies) lvl (filter filterAnimation ani) pics
            where initiallength = length enemies
                  lengthafter = length filteredenemies
                  filteredenemies = removeOutOfBounds enemies
                  score = (initiallength - lengthafter) * 1000 --both players get bonus score if a ball hits the roof

--remove every gamobject from a list if the gameobject is out of bounds (which is only possible in the roof)
removeOutOfBounds :: [GameObjects] -> [GameObjects]
removeOutOfBounds xs = filter checkNoRoofCollision xs

--add a givin value to the score of the given player
updateScore :: Int -> GameObjects -> GameObjects
updateScore n (Player (P1 (PlayerInfo a score b c))) = (Player (P1 (PlayerInfo a (score + n) b c)))
updateScore n (Player (P2 (PlayerInfo a score b c))) = (Player (P2 (PlayerInfo a (score + n) b c)))

--check if the players still have health left
checkGameOver :: Level -> Bool
checkGameOver (Level (Player (P1 (PlayerInfo _ _ _ p1life))) p1o (Player (P2 (PlayerInfo _ _ _ p2life))) p2o enemies lvl ani pics) 
      | (p1life <= 0) && (p2life <= 0) = True
      | otherwise = False

updatePosition :: ObjectInfo -> ObjectInfo
updatePosition x@(ObjectInfo clr (vx,vy) (px,py) size) | checkSideCollision x = (ObjectInfo clr (0,vy) ((px),(py)) size)
                                                       | otherwise = (ObjectInfo clr (vx,vy) ((px+vx),(py+vy)) size)

--Regulates the velocity of the ball (depending on the size) if the ball collides with some wall.
adjustVelocity :: Float -> Float -> ObjectInfo -> ObjectInfo
adjustVelocity x y obj@(ObjectInfo clr (vx,vy) pos size@(Size w h)) | (checkSideCollision obj) && (checkFloorCollision obj) = (ObjectInfo clr ((-(vx + x)),(ballHitY * (sqrt (3 * w)))) pos size)
                                                                    | (checkSideCollision obj) = (ObjectInfo clr ((-(vx + x)),(vy + y)) pos size)
                                                                    | (checkFloorCollision obj) = (ObjectInfo clr ((vx + x),(ballHitY *(sqrt (sqrt (sqrt (3 * w)))))) pos size)
                                                                    | otherwise = (ObjectInfo clr (((vx + x)),(vy + y)) pos size)





