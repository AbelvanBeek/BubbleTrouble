module LoadPictures where

import Model
import HelperFunctions
import Graphics.Gloss.Juicy
import Graphics.Gloss

loadPictures :: [IO Picture]
loadPictures = let x = [ handlePic (loadJuicyPNG path) | path <- filePaths]
                   handlePic xy = do pic <- xy
                                     return $ maybePicToPic pic
                in x

filePaths :: [FilePath]
filePaths = [
              "assets/P1Left.png" -- 0
            , "assets/P1Idle.png"
            , "assets/P1Right.png"
            , "assets/P2Left.png" -- 3
            , "assets/P2Idle.png"
            , "assets/P2Right.png"
            , "assets/Arrow.png" -- 6
            , "assets/Ball.png" -- 7
            , "assets/bg.png" -- 8
            , "assets/explosion1.png" -- 9
            , "assets/explosion2.png"
            , "assets/explosion3.png"
            , "assets/explosion4.png"
            , "assets/explosion5.png"
            , "assets/explosion6.png"
            , "assets/explosion7.png"
            , "assets/explosion8.png"
            , "assets/explosion9.png"
            , "assets/explosion10.png" -- 18
            ]
