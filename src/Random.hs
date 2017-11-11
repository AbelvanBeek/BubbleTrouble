module Random where

import System.Random
import Model

getRandomFloat :: Float -> Float -> IO Float
getRandomFloat min max = getStdRandom (randomR (min,max))

randomize :: Level -> Level
randomize (Level p1 p1o p2 p2o enemies lvl) 
    = (Level (randomPos p1) p1o (randomPos p2) p2o (map randomPos enemies) (map randomPos lvl))

randomPos :: GameObjects -> GameObjects
randomPos (Player         (P1 (PlayerInfo                 objectinfo d o n)))
         = Player         (P1 (PlayerInfo                 objectinfo {pos = (0,100)} d o n))
randomPos (Player         (P2 (PlayerInfo                 objectinfo d o n)))
         = Player         (P2 (PlayerInfo                 objectinfo d o n))
randomPos (PlayerObjects  (Arrow                          objectinfo))
         = PlayerObjects  (Arrow                          objectinfo)
randomPos (EnemyObjects   (Ball                           objectinfo)) 
         = EnemyObjects   (Ball                           objectinfo)
randomPos o@(LevelObjects (Wall                           objectinfo))
           = LevelObjects (Wall                           objectinfo)