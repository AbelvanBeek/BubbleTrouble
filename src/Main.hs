module Main where

import Controller
import HandleInput
import Model
import View
import InitialStates

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "1280x720" (1280,720) (0,0) )      -- Or FullScreen
              white            -- Background color
              60               -- Frames per second
              initialPlay      -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function