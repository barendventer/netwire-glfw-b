module Graphics.UI.GLFW.Netwire.Window.Core( WindowHandle, 
                                             Window(..),
                                             GLContext(..),
                                             --VideoMode(..),
                                             --getFocusedWindow,
                                             --getWindowSize,
                                             --setWindowSize,
                                             --swapBuffers,
                                             --createWindow--,
                                             --getVideoMode ??
                                           ) where

--Mostly re-exports from GLFW intended for use with annotated Window type for the netwire session
import qualified Graphics.UI.GLFW as GLFW
import Prelude hiding ((.))
--import qualified Graphics.Rendering.GL as GL
import System.IO.Unsafe
import Data.IORef
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Wire

--Avoid repeatedly setting GL Mode
--{-# NOINLINE lastFocusedWindowRef #-}
--lastFocusedWindowRef :: IORef (Maybe Window)
--lastFocusedWindowRef = unsafePerformIO $ newIORef Nothing

type WindowHandle = GLFW.Window

newtype GLContext = GLContext { glContextNaughtyBits :: Window }

--type Window = ()

data Window = Window { extensionsDesired :: IORef (Set String),
                       windowHandle :: WindowHandle }

inWindow :: Window -> (GLContext -> IO ()) -> IO ()
inWindow window drawAction = drawAction (GLContext window)

--mkWindow :: (GLParams -> IO ()) -> (GLParams -> scene -> IO ()) -> [String]
--mkWindow drawSetup drawScene = undefined

--Even though it is possible given the data declaration, the implementation of netwire-glfw-b
--should guarantee that any Window with the same WindowHandle is the same Window
--instance Eq (Window t) where
--   w1 == w2 = windowHandle w1 == windowHandle w2

type GLDrawParams = GLFW.VideoMode

-- | Returns the size of a netwire-glfw-b window as a width-height pair
getWindowSize :: Window -> IO (Int,Int)
getWindowSize = GLFW.getWindowSize . windowHandle

-- | Sets the size of a netwire-glfw-b window
setWindowSize :: Window -- ^ The window to resize
              -> Int    -- ^ The new width of the window
              -> Int    -- ^ The new height of the window
              -> IO ()
setWindowSize window = GLFW.setWindowSize (windowHandle window) 
{--
 
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
    GLFW.makeContextCurrent (Just (windowHandle window))
    --set up OpenGL to draw on the window 
    --

loseFocus :: IO ()
loseFocus = do
   currentWindow <- readIORef focusedWindowRef
   case currentWindow of
       Nothing -> return ()
       Just cw -> do
            t <- fmap (maybe 0 id) GLFW.getTime
            writeIORef (currentTime cw) t
   writeIORef focusedWindowRef Nothing
   GLFW.makeContextCurrent Nothing

getWindowFocusCallback :: Window -> GLFW.WindowFocusCallback
getWindowFocusCallback w _ GLFW.FocusState'Focused = giveFocusTo w
getWindowFocusCallback _ _ _                       = loseFocus

--TODO: setGLMode to allow users to set up custom projections for windows

getFocusedWindow :: IO (Maybe Window)
getFocusedWindow = readIORef focusedWindowRef

--

swapBuffers :: Window -> IO ()
swapBuffers window = GLFW.swapBuffers $ windowHandle window

createWindow :: Int -> Int -> String -> IO Window
createWindow width height name = do
    Just wh <- GLFW.createWindow width height name Nothing Nothing
    t <- newIORef 0
    let window = Window t wh
    GLFW.setWindowFocusCallback wh $ Just(GLFW.getWindowFocusCallback window)
    return window

--initializeWindowSubsystem :: IO ()
--initializeWindowSubsystem
--}
