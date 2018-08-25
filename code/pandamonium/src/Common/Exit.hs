module Common.Exit where

import System.Exit
import Graphics.Gloss.Interface.IO.Game
import Common.Redux

exitable :: Redux w e -> Event -> w -> IO w
exitable redux event world = do checkForExit event
                                reduxListen redux event world

checkForExit :: Event -> IO ()
checkForExit (EventKey (Char 'q') _ _ _) = exitSuccess
checkForExit _ = return ()