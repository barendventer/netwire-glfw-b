module Graphics.UI.GLFW.Netwire.Input.Core( 
  ButtonEvent,
  KeyEventData,
  MouseButtonEventData,
  MouseCursorPosition,
  JoystickState,
  WindowEventState,
  clipboard
) where

import Control.Wire
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW.Netwire.Window.Core
import Control.Wire.Core(mkGen_)
import Control.Monad.IO.Class

allJoysticks :: [GLFW.Joystick]
allJoysticks = [GLFW.Joystick'1 .. GLFW.Joystick'16]

data ButtonEvent = KeyEvent KeyEventData | MouseButtonEvent MouseButtonEventData | JoystickButtonEvent GLFW.Joystick [GLFW.JoystickButtonState]

data KeyEventData = KeyEventData GLFW.Key GLFW.KeyState GLFW.ModifierKeys

data MouseButtonEventData = MouseButtonEventData GLFW.MouseButtonState GLFW.ModifierKeys

type MouseCursorPosition = (Double,Double)

data JoystickState = JoystickUnplugged | JoystickState [Bool] [Double]

data WindowEventState = WindowEventState {
    focused :: Bool,
    iconified :: Bool,
    shouldClose :: Bool,
    visible :: Bool,
    width :: Int,
    height :: Int,
    position :: (Int,Int),
    clipboard :: String,
    mouseCursorPosition :: MouseCursorPosition,
    closeButtonPressed :: Bool,
    buttonPresses :: [ButtonEvent],
    joysticks :: [JoystickState]
}

windowActive :: (MonadIO m, Monoid e) => GLFW.Window -> Wire s e m a ()
windowActive window = mkGen_ (\_ -> checkWindow) --> inhibit mempty
   where
     checkWindow = liftIO (GLFW.windowShouldClose window) >>= \x -> if x then return (Right ()) else return (Left mempty)

--whileWindowActive :: (MonadIO m, Monoid e) => GLFW.Window -> Wire s e m a b -> Wire s e m a b
--whileWindowActive window wire = windowActive window
