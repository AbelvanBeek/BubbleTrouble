module Random where

import Graphics.Gloss
import System.Random
import HelperFunctions
import LoadPictures
import Model

randomRFloat :: Float -> Float -> IO Float
randomRFloat min max = getStdRandom (randomR (min,max))

randomRInt :: Int -> Int -> IO Int
randomRInt min max = getStdRandom (randomR (min,max))

randomBool :: IO Bool
randomBool = getStdRandom random

randomPlayLevel :: GameObjects -> GameObjects -> IO Level
randomPlayLevel (Player(P1 (PlayerInfo (ObjectInfo w1 o1 (n1,t1) c1) a1 r1 e1))) (Player(P2 (PlayerInfo (ObjectInfo w2 o2 (n2,t2) c2) a2 r2 e2))) = 
                        let x1IO = randomRFloat (-550) 550
                            x2IO = randomRFloat (-550) 550
                            x3IO = randomRFloat (-550) 550
                            x4IO = randomRFloat (-550) 550
                            y1IO = randomRFloat (-150) 300
                            y2IO = randomRFloat (-150) 300
                            d1IO = randomBool 
                            d2IO = randomBool
                        in  do  x1 <- x1IO
                                x2 <- x2IO
                                x3 <- x3IO
                                x4 <- x4IO
                                y1 <- y1IO
                                y2 <- y2IO
                                d1 <- d1IO
                                d2 <- d2IO
                                return $ Level 
                                    (Player(P1 (PlayerInfo (ObjectInfo w1 o1 (x1,t1) c1) a1 r1 e1)))
                                    [] 
                                    (Player(P2 (PlayerInfo (ObjectInfo w2 o2 (x2,t2) c2) a2 r2 e2)))
                                    [] 
                                    [EnemyObjects(Ball (ObjectInfo red (if d1 then ballSpeed else -ballSpeed,0) (x3,y1) (Size 1 1))),
                                    EnemyObjects(Ball (ObjectInfo red (if d2 then ballSpeed else -ballSpeed,0) (x4,y2) (Size 1 1)))] 
                                    [LevelObjects (Wall (ObjectInfo red (0,0) (0,0) (Size 4.5 4.5)))]
                                    []
                                    loadPictures
randomPlayLevel _ _ = return $ Level 
                                    (Player(P1 (PlayerInfo (ObjectInfo red (0,0) (0,-320) (Size 1 1)) 0 No 5)))
                                    [] 
                                    (Player(P2 (PlayerInfo (ObjectInfo blue (0,0) (0,-320) (Size 1 1)) 0 No 5)))
                                    [] 
                                    [EnemyObjects(Ball (ObjectInfo red (-ballSpeed,0) (200,150) (Size 1 1))),
                                    EnemyObjects(Ball (ObjectInfo red (ballSpeed,0) (-200,200) (Size 1 1)))] 
                                    [LevelObjects (Wall (ObjectInfo red (0,0) (0,0) (Size 4.5 4.5)))]
                                    []
                                    loadPictures