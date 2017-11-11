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
            ]
