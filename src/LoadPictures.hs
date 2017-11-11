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
              "assets/P1Left.png"
            , "assets/P1Idle.png"
            , "assets/P1Right.png"
            , "assets/Arrow.png"
            , "assets/Ball.png"
            , "assets/bg.png"
            , "assets/explosion1.png"
            , "assets/explosion2.png"
            , "assets/explosion3.png"
            , "assets/explosion4.png"
            , "assets/explosion5.png"
            , "assets/explosion6.png"
            , "assets/explosion7.png"
            , "assets/explosion8.png"
            , "assets/explosion9.png"
            , "assets/explosion10.png"
            ]
