module Graphics.UI.GLFW.Netwire.Window.Core( WindowHandle, 
                                             Window(),
                                             getFocusedWindow,
                                             giveFocusTo,
                                             swapBuffers
                                           ) where

import qualified Graphics.UI.GLFW as GLFW
import System.IO.Unsafe
import Data.IORef

{-# NOINLINE focusedWindowRef #-}
focusedWindowRef :: IORef (Maybe Window)
focusedWindowRef = unsafePerformIO $ newIORef Nothing

type WindowHandle = GLFW.Window

data Window = Window { currentTime :: IORef Double, windowHandle :: WindowHandle }

giveFocusTo :: Window -> IO ()
giveFocusTo window = do
    currentWindow <- readIORef focusedWindowRef
    case currentWindow of
        Nothing -> return ()
        Just cw -> do
            t <- fmap (maybe 0 id) GLFW.getTime
            writeIORef (currentTime cw) t
    tnew <- readIORef (currentTime window)
    GLFW.setTime tnew
    writeIORef focusedWindowRef $ Just window

getFocusedWindow :: IO (Maybe Window)
getFocusedWindow = readIORef focusedWindowRef

swapBuffers :: Window -> IO ()
swapBuffers window = GLFW.swapBuffers $ windowHandle window
