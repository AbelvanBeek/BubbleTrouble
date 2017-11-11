module InitialStates where
    
import Model
import Random

{- Initialize States -}

initialMenu :: IO GameState
initialMenu = return $ GameState Menu (Level undefined undefined undefined undefined undefined undefined undefined) undefined

initialPlay :: IO GameState
initialPlay = do rndlvl <- randomPlayLevel
                 return $ GameState Play rndlvl 0