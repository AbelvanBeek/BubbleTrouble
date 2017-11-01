module HighScores where

import Model

readHighScores :: IO [HighScore]
readHighScores = do x <- highscores
                    return $ zip (listOfNames x) (map read (listOfScores x))                 
    where highscores = readFile "assets/highscores.txt"
          listOfNames x = words $ (lines x) !! 0
          listOfScores x = words $ (lines x) !! 1

writeHighScores :: [HighScore] -> IO()
writeHighScores xs = writeFile "assets/highscores1.txt" highscores
    where names  = unwords [ name | (name,score) <- xs ]
          scores = unwords [ show score | (name,score) <- xs ]
          highscores = unlines ([names] ++ [scores])

-- test function
outputScores :: IO()
outputScores = do x <- readHighScores
                  writeHighScores x
                  return ()