module Graphics.UI.GLFW.Netwire.Session ( glfwClockSession ) where

import Control.Wire.Session
import Graphics.UI.GLFW(pollEvents,getTime)
import Control.Monad.IO.Class(MonadIO(..))
import Graphics.UI.GLFW.Netwire.Window.Core

glfwClockSession :: (MonadIO m) => Session m (s -> Timed Double s)
glfwClockSession = 
    Session $ do
        t0 <- liftIO $ fmap (maybe 0 id) getTime
        return (Timed 0, loop t0) 
  where
    loop tprev = Session $ do
        w <- liftIO getFocusedWindow
        case w of
             Nothing -> return ()
             Just cw -> liftIO $ swapBuffers cw
        liftIO $ pollEvents
        t <- liftIO $ fmap (maybe 0 id) getTime
        let dt = t-tprev
        dt `seq` return (Timed dt, loop t) 
