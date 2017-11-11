module InitialStates where
    
import Model
import Random
import LoadPictures

{- Initialize States -}

initialMenu :: IO GameState
initialMenu = return $ GameState Menu (Level undefined undefined undefined undefined undefined undefined undefined loadPictures) undefined

initialPlay :: IO GameState
initialPlay = do rndlvl <- randomPlayLevel
                 return $ GameState Play rndlvl 0