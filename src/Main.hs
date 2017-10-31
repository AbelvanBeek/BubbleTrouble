module Main where

import Controller
import HandleInput
import Model
import View
import InitialStates

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO FullScreen       -- Or FullScreen
              white            -- Background color
              60               -- Frames per second
              initialPlay      -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function