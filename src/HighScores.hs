module HighScores where

import Model
import System.IO.Strict as S

readHighScores :: IO [HighScore]
readHighScores = do x <- highscores
                    return $ zip (listOfNames x) (map read (listOfScores x))                 
    where highscores     = S.readFile "assets/highscores.txt"
          listOfNames x  = words $ (lines x) !! 0
          listOfScores x = words $ (lines x) !! 1

writeHighScores :: IO [HighScore] -> IO()
writeHighScores xs = do x <- xs
                        writeFile "assets/highscores.txt" (unlines ([unwords [ name | (name,score) <- x ]] ++ [unwords [ show score | (name,score) <- x ]]))
                        return ()

updateHighScores :: Level -> IO()
updateHighScores (Level p1 _ p2 _ _ _ _ _) = writeHighScores $ topHighScore readHighScores $ getScore p1 p2

getScore :: GameObjects -> GameObjects -> [HighScore]
getScore (Player (P1 (PlayerInfo _ p1score _ _))) (Player (P2 (PlayerInfo _ p2score _ _))) = [("Player_1", p1score), ("Player_2", p2score)]

topHighScore :: IO [HighScore] -> [HighScore] -> IO [HighScore]
topHighScore current new = do cur <- current
                              return [compareScore (cur !! 0) (new !! 0),
                                      compareScore (cur !! 1) (new !! 1)]

compareScore :: HighScore -> HighScore -> HighScore
compareScore s1 s2 | snd s1 >= snd s2 = s1
                   | otherwise        = s2