{-# LANGUAGE FlexibleContexts #-}

module Graphics.UI.GLFW.Netwire.Input.Core( 
  MouseCursorPosition,
  JoystickState,
  InputHandler(..),
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
import Control.Monad((>=>))
import Control.Exception(throw)
import Graphics.UI.GLFW.Netwire.Input.Types

allJoysticks :: [GLFW.Joystick]
allJoysticks = [GLFW.Joystick'1 .. GLFW.Joystick'16]

--Redefine the alias to prevent a circular dependency

data InputHandler = InputHandler { inputHandler :: GLFWEventData -> IO (), --Callback to the window
                                   pollJoysticks :: IO Joysticks, --Poll actions
                                   pollClipboard :: IO ClipboardString,
                                   pollInputMode :: IO InputMode,
                                   attachedWindow :: WindowHandle }

--Shadow fromJust 
fromJust :: IO (Maybe a) -> IO a
fromJust = fmap $ fromMaybe (error $ "Read of a Maybe failed inside netwire-glfw-b IO handler. Either you were working with GLFW directly or this is a bug "
                                  ++ "in netwire-glfw-b. Likely cause is GLFW not having been initialized while IO was being handled")
{- 
mkEmptyInputHandler :: WindowHandle -> IO InputHandler
mkEmptyInputHandler wh = do
   wes <- getInitialEventState wh
   ib <- newIORef (Just wes)
   return (InputHandler wh ib) 
-}

type GLFWEvent = Event GLFWEventData

type Joysticks = [JoystickState]

readJoystick :: Int -> Joysticks -> JoystickState
readJoystick = flip (!!)

type ClipboardString = Maybe String

data InputMode = InputMode Bool Bool Bool Bool
 deriving (Eq,Ord,Bounded)

stickyMouse, stickyKeys, cursorEnabled, cursorVisible :: InputMode -> Bool
stickyKeys (InputMode x _ _ _) = x
stickyMouse (InputMode _ x _ _) = x
cursorEnabled (InputMode _ _ x _) = x
cursorVisible (InputMode _ _ _ x) = x

{-
data WindowEventState = WindowEventState {
    focused :: Bool,
    clipboard :: Maybe String,
    mouseCursorPosition :: MouseCursorPosition,
    closeRequest :: Bool,
    joysticks :: [JoystickState],
    cursorInWindow :: Bool,
    stickyKeys :: Bool,
    stickyMouse :: Bool,
    cursorEnabled :: Bool,
    cursorVisible :: Bool,
    scrollWheelX :: Double,
    scrollWheelY :: Double
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
    joysticks = replicate 16 JoystickUnplugged,
    cursorInWindow = False,
    cursorVisible = v,
    cursorEnabled = e,
    stickyKeys = k,
    stickyMouse = km,
    scrollWheelX = 0,
    scrollWheelY = 0,
    eventBuffer = []
   }
   r = ReaderT

readInputHandlerContents :: (MonadIO m) => InputHandler -> ExceptT GLFWSessionError m WindowEventState
readInputHandlerContents ib = do
   contents <- getInputHandlerContents ib
   return contents { textInputHandler = reverse (textInputHandler contents), 
                     keyPresses = reverse (keyPresses contents), 
                     mouseButtonPresses = reverse (mouseButtonPresses contents) }

getInputHandlerContents :: (MonadIO m) => InputHandler -> ExceptT GLFWSessionError m WindowEventState
getInputHandlerContents ib = do
   unsafe_wes <- liftIO . readIORef . inputBufferNaughtyBits $ ib
   case unsafe_wes of
       Just wes -> return wes
       Nothing  -> throwE GLFWSessionErrorInputHandlerClosed

--May make an invalid input buffer appear valid, use only if you will fail on invalid input buffers anyway
unsafeSetInputHandlerContents :: (MonadIO m) => WindowEventState -> InputHandler -> ExceptT GLFWSessionError m ()
unsafeSetInputHandlerContents ds ib = liftIO $ writeIORef (inputBufferNaughtyBits ib) (Just ds)

setInputHandlerContents :: (MonadIO m) => WindowEventState -> InputHandler -> ExceptT GLFWSessionError m () 
setInputHandlerContents ds = modifyInputHandler (const ds)

modifyInputHandler :: (MonadIO m) => (WindowEventState -> WindowEventState) -> InputHandler -> ExceptT GLFWSessionError m ()
modifyInputHandler f ib = do
   wes <- getInputHandlerContents ib
   unsafeSetInputHandlerContents (f wes) ib

--Reset the accumulating fields
resetInputHandler :: (MonadIO m) => InputHandler -> ExceptT GLFWSessionError m ()
resetInputHandler = modifyInputHandler $ \wes -> wes {
   keyPresses = [],
   mouseButtonPresses = [],
   scrollWheelX = 0,
   scrollWheelY = 0 
   }
-}

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

{-
--Pull state for which there are no pushing callbacks
updateInputHandler :: (MonadIO m, Functor m) => Joysticks -> InputHandler -> WindowHandle -> ExceptT GLFWSessionError m ()
updateInputHandler js ib = runReaderT $ do
   k <-  (GLFW.StickyKeysInputMode'Enabled==) <$> r GLFW.getStickyKeysInputMode
   km <- (GLFW.StickyMouseButtonsInputMode'Enabled==) <$> r GLFW.getStickyMouseButtonsInputMode
   cmode <- r GLFW.getCursorInputMode
   let v = cmode /= GLFW.CursorInputMode'Hidden
   let e = cmode /= GLFW.CursorInputMode'Disabled
   c <- r GLFW.getClipboardString
   lift $ modifyInputHandler (\wes -> wes { joysticks = js, stickyKeys = k, stickyMouse = km, clipboard = c, cursorEnabled = e, cursorVisible = v }) ib 
 where r = ReaderT . (liftIO .)

pollState :: (MonadIO m, Functor m) => InputHandler -> WindowHandle -> ExceptT GLFWSessionError m WindowEventState
pollState = pullJoysticks >=> pollState'

pollState' :: (MonadIO m, Functor m) => Joysticks -> InputHandler -> WindowHandle -> ExceptT GLFWSessionError m WindowEventState
pollState' js ib wh = do
    updateInputHandler js ib wh
    result <- readInputHandlerContents ib
    resetInputHandler ib
    return result
-}

closeInputHandler :: InputHandler -> IO ()
closeInputHandler ib = resetCallbacks (attachedWindow ib) 

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

toIO :: ExceptT GLFWSessionError IO a -> IO a
toIO action = runExceptT action >>= \result -> case result of
   Left e -> return $ throw e
   Right v -> return v

attachCallbacks :: WindowHandle -> InputHandler -> IO ()
attachCallbacks = curry . runReader $ 
   reader (useCallbacks . fst)
                   <*> keyCallback
                   <*> charCallback
                   <*> cursorEnterCallback
                   <*> mouseButtonCallback
                   <*> mousePositionCallback
                   <*> scrollWheelCallback
                   <*> windowFocusCallback

callingBackTo :: (WindowHandle,InputHandler) -> WindowHandle -> GLFWEventData -> IO ()
callingBackTo (wh,ib) dwh handled = Monad.unless (wh /= dwh) $ inputHandler ib handled

mkCallback :: (a -> b) -> Reader a (Maybe b)
mkCallback f = reader $ \p -> Just (f p)

keyCallback :: Reader (WindowHandle,InputHandler) (Maybe GLFW.KeyCallback)
keyCallback = mkCallback $ \(wh,ib) dwh k scancode ks modkeys ->
   callingBackTo(wh,ib) dwh $ 
          KeyEvent k scancode ks modkeys
 
mouseButtonCallback :: Reader (WindowHandle,InputHandler) (Maybe GLFW.MouseButtonCallback)
mouseButtonCallback = mkCallback $ \(wh,ib) dwh mb mbs modkeys -> 
   callingBackTo(wh,ib) dwh $ 
         MouseButtonEvent (fromEnum mb) (mbs==GLFW.MouseButtonState'Pressed) modkeys

charCallback :: Reader (WindowHandle,InputHandler) (Maybe GLFW.CharCallback)
charCallback = mkCallback $ \(wh,ib) dwh char -> 
   callingBackTo(wh,ib) dwh $ 
         CharEvent char

cursorEnterCallback :: Reader (WindowHandle,InputHandler) (Maybe GLFW.CursorEnterCallback)
cursorEnterCallback = mkCallback $ \(wh,ib) dwh x -> 
   callingBackTo(wh,ib) dwh $
         MouseEnterEvent(x == GLFW.CursorState'InWindow)

mousePositionCallback :: Reader (WindowHandle,InputHandler) (Maybe GLFW.CursorPosCallback)
mousePositionCallback = mkCallback $ \(wh,ib) dwh x y -> 
   callingBackTo(wh,ib) dwh $
         MouseMoveEvent(x,y)

windowFocusCallback :: Reader (WindowHandle,InputHandler) (Maybe GLFW.WindowFocusCallback)
windowFocusCallback = mkCallback $ \(wh,ib) dwh x -> 
   callingBackTo(wh,ib) dwh $
         FocusChangeEvent(x == GLFW.FocusState'Focused)

scrollWheelCallback :: Reader (WindowHandle,InputHandler) (Maybe GLFW.ScrollCallback)
scrollWheelCallback = mkCallback $ \(wh,ib) dwh x y -> 
   callingBackTo(wh,ib) dwh $
         ScrollWheelEvent(x,y)

