module Graphics.UI.Netwire.Window.Context(
   GLContext(),
   isOpenGLDebugContext,
   isOpenGLForwardCompatible,
   isOpenGLES,
   getContextVersionMajor,
   getContextVersionMinor,
   getContextVersionRevision,
   
) where

import Graphics.UI.GLFW.Netwire.Window.Core(GLContext(..), WindowHandle, Window(..))
import qualified Graphics.UI.GLFW as GLFW
import Control.Applicative((<$>))
import Data.Set(Set)

withContext :: (WindowHandle -> b) -> GLContext -> b
withContext glfwFunc = glfwFunc . windowHandle . glContextNaughtyBits

type GLContextAccessor t = GLContext -> IO t
-- | Get the extensions supported by the window
getSupportedExtensions :: GLContextAccessor (Set String)
-- | Check if the OpenGL context is running in debug mode

isOpenGLDebugContext :: GLContextAccessor Bool
isOpenGLDebugContext = withContext GLFW.getWindowOpenGLDebugContext

-- | Check if context has forwards-compatibility enabled
isOpenGLForwardCompatible = withContext GLFW.getWindowOpenGLForwardCompat

-- | Check if context is an OpenGL ES context
isOpenGLES :: GLContextAccessor Bool
isOpenGLES = clientAPItoBool <$> withContext GLFW.getWindowClientAPI where
    clientAPItoBool GLFW.ClientAPI'OpenGLES = True
    clientAPItoBool _                       = False

-- | Gets the major version of OpenGL used in the OpenGL context
getContextVersionMajor :: GLContextAccessor Int
getContextVersionMajor = withContext GLFW.getWindowContextVersionMajor

-- | Gets the minor version of OpenGL used in the OpenGL context
getContextVersionMinor :: GLContextAccessor Int
getContextVersionMinor = withContext GLFW.getWindowContextVersionMinor

-- | Gets the revision number of the version of OpenGL used in the OpenGL context
getContextVersionRevision :: GLContextAccessor Int
getContextVersionRevision = withContext GLFW.getWindowContextVersionRevision

getContextRobustness = withContext GLFW.getWindowContextRobustness

-- | Checks if this OpenGL context supports only the Core mode, and no compatability modes. Returns True if so.
getContextCoreProfileOnly :: GLContextAccessor
getContextCoreProfileOnly = profileToBool <$> withContext GLFW.getWindowOpenGLProfile where
     profileToBool GLFW.OpenGLProfile'Core = True
     profileToBool _                       = False

-- | Returns the framebuffer size for the given OpenGL context
getContextFramebufferSize = withContext GLFW.getFramebufferSize

