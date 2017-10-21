module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO FullScreen       -- Or FullScreen
              black            -- Background color
              60               -- Frames per second
              initialMenu      -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function