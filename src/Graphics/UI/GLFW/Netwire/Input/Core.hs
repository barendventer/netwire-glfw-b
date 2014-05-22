module Graphics.UI.GLFW.Netwire.Input.Core( 
  ButtonEvent,
  KeyEventData,
  MouseButtonEventData,
  MouseCursorPosition,
  JoystickState,
  WindowEventState,
  clipboard
) where

import Prelude hiding ((.))
import Control.Wire
import qualified Graphics.UI.GLFW as GLFW
import Control.Wire.Core(mkGen_)
import Data.IORef
import Graphics.UI.GLFW.Netwire.Exception
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

allJoysticks :: [GLFW.Joystick]
allJoysticks = [GLFW.Joystick'1 .. GLFW.Joystick'16]

--Redefine the alias to prevent a circular dependency
type WindowHandle = GLFW.Window

newtype InputBuffer = InputBuffer { inputBufferNaughtyBits :: IORef (Maybe WindowEventState) }

mkEmptyInputBuffer :: IO InputBuffer
mkEmptyInputBuffer = InputBuffer <$> newIORef (error "mkEmptyInputBuffer: Finish Me") 

data ButtonEvent = KeyEvent KeyEventData | MouseButtonEvent MouseButtonEventData | JoystickButtonEvent GLFW.Joystick [GLFW.JoystickButtonState]

data KeyEventData = KeyEventData GLFW.Key GLFW.KeyState GLFW.ModifierKeys

data MouseButtonEventData = MouseButtonEventData GLFW.MouseButtonState GLFW.ModifierKeys

type MouseCursorPosition = (Double,Double)

data JoystickState = JoystickUnplugged | JoystickState [Bool] [Double]

data WindowEventState = WindowEventState {
    focused :: Bool,
    clipboard :: Maybe String,
    mouseCursorPosition :: MouseCursorPosition,
    closeRequest :: Bool,
    buttonPresses :: [ButtonEvent],
    joysticks :: [JoystickState],
    cursorInWindow :: Bool,
    stickyKeys :: Bool,
    cursorEnabled :: Bool,
    cursorVisible :: Bool,
    scrollWheelX :: Double,
    scrollWheelY :: Double
}
{-
initialEventState = runReaderT $ do
    c <- r GLFW.getClipboardString
    m <- r GLFW.getCursorPos
    
  where 
   init f c m x i v k = WindowEventState {
    focused = f,
    clipboard = c,
    mouseCursorPosition = m,
    closeRequest = x,
    buttonPresses = [],
    joysticks = replicate 16 JoystickUnplugged,
    cursorInWindow = i,
    cursorVisible = cv,
    stickyKeys = k,
    scrollWheelX = 0,
    scrollWheelY = 0
   }
   r = ReaderT
-}
getInputBufferContents :: (MonadIO m) => InputBuffer -> ExceptT GLFWSessionError m WindowEventState
getInputBufferContents ib = do
   unsafe_wes <- liftIO . readIORef . inputBufferNaughtyBits $ ib
   case unsafe_wes of
       Just wes -> return wes
       Nothing  -> throwE GLFWSessionErrorInputBufferClosed

--May make an invalid input buffer appear valid, use only if you will fail on invalid input buffers anyway
unsafeSetInputBufferContents :: (MonadIO m) => WindowEventState -> InputBuffer -> ExceptT GLFWSessionError m ()
unsafeSetInputBufferContents ds ib = liftIO $ writeIORef (inputBufferNaughtyBits ib) (Just ds)

setInputBufferContents :: (MonadIO m) => WindowEventState -> InputBuffer -> ExceptT GLFWSessionError m () 
setInputBufferContents ds = modifyInputBuffer (const ds)

modifyInputBuffer :: (MonadIO m) => (WindowEventState -> WindowEventState) -> InputBuffer -> ExceptT GLFWSessionError m ()
modifyInputBuffer f ib = do
   wes <- getInputBufferContents ib
   unsafeSetInputBufferContents (f wes) ib

modifyFocus :: (MonadIO m) => Bool -> InputBuffer -> ExceptT GLFWSessionError m ()
modifyFocus ds = modifyInputBuffer $ \wes -> wes { focused = ds }


