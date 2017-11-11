module LoadPictures where

import Model
import HelperFunctions
import Graphics.Gloss.Juicy
import Graphics.Gloss

loadPictures :: [IO Picture]
loadPictures = let x = [ handlePic (loadJuicyPNG path) | path <- filePaths]
                   handlePic xy= do pic <- xy
                                    return $ maybePicToPic pic
                in x

filePaths :: [FilePath]
filePaths = [
              "assets/P1Left.png" -- 0
            , "assets/P1Idle.png"
            , "assets/P1Right.png"
            , "assets/P1Left.png" -- 3
            , "assets/P1Idle.png"
            , "assets/P1Right.png"
            , "assets/Arrow.png" -- 6
            , "assets/Ball.png" -- 7
            , "assets/bg.png" -- 8
            , "assets/fire/explosion1.png" -- 9
            , "assets/fire/explosion2.png"
            , "assets/fire/explosion3.png"
            , "assets/fire/explosion4.png"
            , "assets/fire/explosion5.png"
            , "assets/fire/explosion6.png"
            , "assets/fire/explosion7.png"
            , "assets/fire/explosion8.png"
            , "assets/fire/explosion9.png"
            , "assets/fire/explosion10.png" -- 18
            ]
