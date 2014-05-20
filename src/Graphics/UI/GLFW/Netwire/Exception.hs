{-# LANGUAGE DeriveDataTypeable #-}

module Graphics.UI.GLFW.Netwire.Exception (
    GLFWSessionError(..),
    GLFWError(..)) where

import qualified Graphics.UI.GLFW as GLFW
import Control.Exception
import Data.Data(Data)
import Data.Typeable(Typeable)

data GLFWError = GLFWError GLFW.Error String deriving (Eq,Ord,Data,Read,Show,Typeable)

instance Exception GLFWError

--Un-enumerated errors in the GLFW library should ideally be prevented by the library
data GLFWSessionError = GLFWSessionErrorAlreadyInitialized
                      | GLFWSessionErrorAlreadyFinalized
                      | GLFWSessionErrorInitializationFailed
                      | GLFWSessionErrorInvalidToken
                      | GLFWSessionErrorWindowClosed
                      | GLFWSessionErrorGLFWErrorOccured GLFWError
                      | GLFWSessionErrorOther String --Should never be used from netwire-glfw-b itself
   deriving(Eq,Show,Ord,Data,Read,Typeable)

instance Exception GLFWSessionError
