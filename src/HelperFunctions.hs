module HelperFunctions where

import Graphics.Gloss

-- Convert a maybe picture to a picture -> Blank picture in case of Nothing
maybePicToIO :: Maybe Picture -> Picture
maybePicToIO Nothing  = blank
maybePicToIO (Just x) = x