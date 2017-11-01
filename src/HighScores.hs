module HighScores where

import Model

readHighScores :: IO [HighScore]
readHighScores = do x <- highscores
                    return [ ((name),(read score)) | score <- listOfScores x, name <- listOfNames x]
                    
    where highscores = readFile "highscores.txt"
          listOfNames x = words $ (lines x) !! 0
          listOfScores x = words $ (lines x) !! 1

writeHighScores :: [HighScore] -> IO()
writeHighScores xs = writeFile "/assets/highscores.txt" highscores
    where names  = unwords [ name | (name,score) <- xs ]
          scores = unwords [ show score | (name,score) <- xs ]
          highscores = unlines ([names] ++ [scores])

outputScores :: IO()
outputScores = undefined