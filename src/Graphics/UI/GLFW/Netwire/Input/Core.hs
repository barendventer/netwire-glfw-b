module Graphics.UI.GLFW.Netwire.Input.Core( 
  ButtonEvent,
  KeyEventData,
  MouseButtonEventData,
  MouseCursorPosition,
  JoystickState,
  WindowEventState
) where

import Control.Wire
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW.Netwire.Window.Core

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


