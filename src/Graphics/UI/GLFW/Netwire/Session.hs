module Graphics.UI.GLFW.Netwire.Session ( glfwClockSession ) where

import Control.Wire.Session
import Graphics.UI.GLFW(pollEvents,getTime)
import Control.Monad.IO.Class(MonadIO(..))
import Graphics.UI.GLFW.Netwire.Window.Core
import Control.Applicative
import Data.Maybe(fromMaybe)

glfwClockSession :: (MonadIO m) => Session m (s -> Timed Double s)
glfwClockSession = 
    Session $ do
        t0 <- liftIO $ fmap  (fromMaybe 0) getTime
        return (Timed 0, loop t0) 
  where
    loop tprev = Session $ do
--        w <- liftIO getFocusedWindow
--        case w of
--             Nothing -> return ()
--             Just cw -> liftIO $ swapBuffers cw
        liftIO pollEvents
        t <- liftIO $ fmap (fromMaybe 0) getTime
        let dt = t-tprev
        dt `seq` return (Timed dt, loop t)

glfwClockSession_ :: (MonadIO m, Applicative m) => Session m (Timed Double ())
glfwClockSession_ = glfwClockSession <*> pure ()

--The count session will slow down time in the game if GLFW starts to drop frames
-- by fixing the length of time progressed by each tick of the GLFW game loop
glfwCountSession :: (MonadIO m) => t -> Session m (s -> Timed t s)
glfwCountSession dt = Session $ do
    liftIO pollEvents
    return (Timed dt, glfwCountSession dt)

glfwCountSession_ :: (MonadIO m, Applicative m) => t -> Session m (Timed t ())
glfwCountSession_ dt = countSession dt <*> pure ()
