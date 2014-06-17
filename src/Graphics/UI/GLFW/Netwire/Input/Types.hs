module Graphics.UI.GLFW.Netwire.Input.Types(WindowHandle,
                                            ModifierKeys,
                                            KeyState,
                                            Scancode,
                                            Key,
                                            MouseCursorPosition,
                                            GLFWEventData(..),
                                            JoystickState(..)
                                           ) where

import qualified Graphics.UI.GLFW as GLFW

type WindowHandle = GLFW.Window

type ModifierKeys = GLFW.ModifierKeys

type KeyState = GLFW.KeyState

type Scancode = Int

type Key = GLFW.Key

type MouseCursorPosition = (Double,Double)

data GLFWEventData = MouseButtonEvent Int Bool ModifierKeys 
                   | KeyEvent Key Scancode KeyState ModifierKeys
                   | MouseMoveEvent (Double,Double)
                   | FocusChangeEvent Bool
                   | MouseEnterEvent Bool
                   | ScrollWheelEvent (Double,Double)
                   | CharEvent Char 
                   | CloseButtonEvent Bool
   deriving(Eq,Ord)

data JoystickState = JoystickUnplugged | JoystickState String [Bool] [Double]
  deriving(Eq,Ord)
