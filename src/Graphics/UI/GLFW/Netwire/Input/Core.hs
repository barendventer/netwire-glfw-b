module Graphics.UI.GLFW.Netwire.Input.Core( 
  ButtonEvent,
  KeyEventData,
  MouseButtonEventData,
  MouseCursorPosition,
  JoystickState,
  WindowEventState,
  clipboard,
  InputBuffer(..),
  mkEmptyInputBuffer,
  pullJoysticks
) where

import Prelude hiding ((.))
import Control.Wire
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef
import Graphics.UI.GLFW.Netwire.Exception
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Maybe(fromMaybe)
import Control.Monad.Trans.Class
import qualified Control.Monad as Monad

allJoysticks :: [GLFW.Joystick]
allJoysticks = [GLFW.Joystick'1 .. GLFW.Joystick'16]

--Redefine the alias to prevent a circular dependency
type WindowHandle = GLFW.Window

newtype InputBuffer = InputBuffer { inputBufferNaughtyBits :: IORef (Maybe WindowEventState) }

mkEmptyInputBuffer :: WindowHandle -> IO InputBuffer
mkEmptyInputBuffer wh = do
   wes <- getInitialEventState wh
   ib <- newIORef (Just wes)
   return (InputBuffer ib) 

data ButtonEvent = KeyEvent KeyEventData | MouseButtonEvent MouseButtonEventData | JoystickButtonEvent GLFW.Joystick [GLFW.JoystickButtonState]

data KeyEventData = KeyEventData GLFW.Key Int GLFW.KeyState GLFW.ModifierKeys

data MouseButtonEventData = MouseButtonEventData GLFW.MouseButtonState GLFW.ModifierKeys

type MouseCursorPosition = (Double,Double)

data JoystickState = JoystickUnplugged | JoystickState String [Bool] [Double]

type Joysticks = [JoystickState]

readJoystick :: Int -> Joysticks -> JoystickState
readJoystick = flip (!!)

data WindowEventState = WindowEventState {
    focused :: Bool,
    clipboard :: Maybe String,
    mouseCursorPosition :: MouseCursorPosition,
    closeRequest :: Bool,
    buttonPresses :: [ButtonEvent],
    joysticks :: [JoystickState],
    cursorInWindow :: Bool,
    stickyKeys :: Bool,
    stickyMouse :: Bool,
    cursorEnabled :: Bool,
    cursorVisible :: Bool,
    scrollWheelX :: Double,
    scrollWheelY :: Double,
    textInputBuffer :: String
}

getInitialEventState :: WindowHandle -> IO WindowEventState
getInitialEventState = runReaderT $ do
    f <- (GLFW.FocusState'Focused==) <$> r GLFW.getWindowFocused
    c <- r GLFW.getClipboardString
    m <- r GLFW.getCursorPos
    x <- r GLFW.windowShouldClose
    cmode <- r GLFW.getCursorInputMode
    let v = cmode /= GLFW.CursorInputMode'Disabled
    let e = cmode /= GLFW.CursorInputMode'Hidden
    k <- (GLFW.StickyKeysInputMode'Enabled==) <$> r GLFW.getStickyKeysInputMode
    km <- (GLFW.StickyMouseButtonsInputMode'Enabled==) <$> r GLFW.getStickyMouseButtonsInputMode
    return $ initializeEventState f c m x v e k km
  where 
   initializeEventState f c m x v e k km = WindowEventState {
    focused = f,
    clipboard = c,
    mouseCursorPosition = m,
    closeRequest = x,
    buttonPresses = [],
    joysticks = replicate 16 JoystickUnplugged,
    cursorInWindow = False,
    cursorVisible = v,
    cursorEnabled = e,
    stickyKeys = k,
    stickyMouse = km,
    scrollWheelX = 0,
    scrollWheelY = 0,
    textInputBuffer = ""
   }
   r = ReaderT

readInputBufferContents :: (MonadIO m) => InputBuffer -> ExceptT GLFWSessionError m WindowEventState
readInputBufferContents ib = do
   contents <- getInputBufferContents ib
   return contents { textInputBuffer = reverse (textInputBuffer contents), buttonPresses = reverse (buttonPresses contents) }

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

--Reset the accumulating fields
resetInputBuffer :: (MonadIO m) => InputBuffer -> ExceptT GLFWSessionError m ()
resetInputBuffer = modifyInputBuffer $ \wes -> wes {
   buttonPresses = [],
   scrollWheelX = 0,
   scrollWheelY = 0 
   }

--Pull the state from the joysticks
pullJoysticks :: IO Joysticks
pullJoysticks = mapM pullJoystick allJoysticks where
   pullJoystick joystick = do
      connected <- GLFW.joystickPresent joystick
      if not connected
        then return JoystickUnplugged 
        else do
           axes <- fromMaybe [] <$> GLFW.getJoystickAxes joystick
           buttons <- map (GLFW.JoystickButtonState'Pressed==) <$> fromMaybe [] <$> GLFW.getJoystickButtons joystick
           name <- fromMaybe "UntitledJoystick" <$> GLFW.getJoystickName joystick
           return $ JoystickState name buttons axes

--Pull state for which there are no pushing callbacks
updateInputBuffer :: (MonadIO m, Functor m) => Joysticks -> InputBuffer -> WindowHandle -> ExceptT GLFWSessionError m ()
updateInputBuffer js ib = runReaderT $ do
   k <-  (GLFW.StickyKeysInputMode'Enabled==) <$> r GLFW.getStickyKeysInputMode
   km <- (GLFW.StickyMouseButtonsInputMode'Enabled==) <$> r GLFW.getStickyMouseButtonsInputMode
   cmode <- r GLFW.getCursorInputMode
   let v = cmode /= GLFW.CursorInputMode'Hidden
   let e = cmode /= GLFW.CursorInputMode'Disabled
   c <- r GLFW.getClipboardString
   lift $ modifyInputBuffer (\wes -> wes { joysticks = js, stickyKeys = k, stickyMouse = km, clipboard = c, cursorEnabled = e, cursorVisible = v }) ib 
 where r = ReaderT . (liftIO .)

pollState :: (MonadIO m, Functor m) => Joysticks -> InputBuffer -> WindowHandle -> ExceptT GLFWSessionError m WindowEventState
pollState js ib wh = do
    updateInputBuffer js ib wh
    result <- readInputBufferContents ib
    resetInputBuffer ib
    return result

useCallbacks :: WindowHandle
             -> Maybe GLFW.KeyCallback 
             -> Maybe GLFW.CharCallback
             -> Maybe GLFW.CursorEnterCallback
             -> Maybe GLFW.MouseButtonCallback
             -> Maybe GLFW.CursorPosCallback
             -> Maybe GLFW.ScrollCallback
             -> Maybe GLFW.WindowFocusCallback
             -> IO ()
useCallbacks wh key char mouseEnter mouseButton mousePosition scrollWheel onFocus = do
   GLFW.setKeyCallback wh key
   GLFW.setCharCallback wh char
   GLFW.setCursorEnterCallback wh mouseEnter
   GLFW.setMouseButtonCallback wh mouseButton
   GLFW.setCursorPosCallback wh mousePosition
   GLFW.setScrollCallback wh scrollWheel
   GLFW.setWindowFocusCallback wh onFocus
 
resetCallbacks :: WindowHandle -> IO ()
resetCallbacks wh = useCallbacks wh Nothing Nothing Nothing Nothing Nothing Nothing Nothing

--TODO: Code to return from IO to ExceptT GLFWSessionError IO
keyCallback :: (WindowHandle,InputBuffer) -> GLFW.KeyCallback
keyCallback(wh,ib) dwh k scancode ks modkeys = 
   Monad.unless (wh /= dwh) $ return (error "Not Implemented")--modifyInputBuffer (\wes -> wes { buttonPresses = KeyEvent (KeyEventData k scancode ks modkeys) : buttonPresses wes }) ib
    
setFocus :: (MonadIO m) => Bool -> InputBuffer -> ExceptT GLFWSessionError m ()
setFocus ds = modifyInputBuffer $ \wes -> wes { focused = ds }
