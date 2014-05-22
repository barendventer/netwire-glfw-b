module Graphics.UI.GLFW.Netwire.Token.Core( 
   checkout,
   checkin,
   TokenGlobalState(..), --Naughty
   GLFWSessionError(..),
   GLFWError,
   voidToken, --Naughty
   emptyToken, --Naughty
   getVoidedToken, --OK, but for testing
   GLFWVersion(..),
   getGLFWVersion,
   Token(..)) where

import qualified Data.Set as Set
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef
import Control.Monad.Trans.Except
import System.IO.Unsafe
import Control.Monad.IO.Class
import Graphics.UI.GLFW.Netwire.Window.Core
import Control.Applicative((<$>))
import Graphics.UI.GLFW.Netwire.Exception
import Control.Monad.Trans.Reader
import Control.Exception

type GLFWb a = ReaderT Token (ExceptT GLFWSessionError IO) a

toIOException :: (Exception e) => ExceptT e IO a -> IO a
toIOException action = do
   unsafe <- runExceptT action
   case unsafe of
       Left e -> throw e
       Right r -> return r

safeRunGLFWb :: GLFWb a -> ExceptT GLFWSessionError IO a
safeRunGLFWb action = do
   tk <- checkout
   result <- runReaderT action tk
   checkin tk
   return result
   
runGLFWb :: GLFWb a -> IO a
runGLFWb action = toIOException . safeRunGLFWb $ action

type GLFWVersion = GLFW.Version

getGLFWVersion = GLFW.getVersion

--Singleton to ensure only one GLFW token ever is valid
{-# NOINLINE glfwIsInitialized #-}
glfwIsInitialized :: IORef Bool
glfwIsInitialized = unsafePerformIO $ newIORef False

getErrorHandler :: IORef [GLFWError] -> GLFW.Error -> String -> IO ()
getErrorHandler eb err desc = do
     lastBufferState <- readIORef eb
     writeIORef eb $ GLFWError err desc : lastBufferState

--Placeholder
-- | Initialize GLFW and get the global state record.
-- | Throws GLFWSessionErrorAlreadyInitialized if GLFW is initialized.
checkout :: (MonadIO m) => ExceptT GLFWSessionError m Token
checkout = do
   alreadyInit <- liftIO $ readIORef glfwIsInitialized
   if alreadyInit then throwE GLFWSessionErrorAlreadyInitialized
                  else do initializeSuccess <- liftIO GLFW.init
                          if initializeSuccess then liftIO $ do writeIORef glfwIsInitialized True
                                                                liftIO emptyToken
                                               else throwE GLFWSessionErrorInitializationFailed

finalizeGLFW :: IO ()
finalizeGLFW = do
   GLFW.setErrorCallback Nothing
   GLFW.terminate

-- | Finalize GLFW and set the input token to the useless token
-- | Throws GLFWSessionErrorInvalidToken if argument token is already finalized
-- | Throws GLFWSessionErrorAlreadyFinalized if GLFW is not running
checkin :: (MonadIO m) => Token -> ExceptT GLFWSessionError m ()
checkin token = do
   tokenContents <- liftIO $ readIORef (tokenNaughtyBits token)
   case tokenContents of
       Left _  -> throwE GLFWSessionErrorInvalidToken
       Right x -> do
            glfwInitialized <- liftIO $ readIORef glfwIsInitialized
            if not glfwInitialized then throwE GLFWSessionErrorAlreadyFinalized
                                   else liftIO $ do finalizeGLFW
                                                    writeIORef glfwIsInitialized False
                                                    voidToken token

-- | Sets a token to the useless token. Causes token-consuming GL functions to return errors
voidToken :: Token -> IO ()
voidToken tk = writeIORef (tokenNaughtyBits tk) (Left ())

getVoidedToken :: IO Token
getVoidedToken = Token <$> newIORef (Left ())

data TokenGlobalState = TokenGlobalState {
    errorBuffer  :: IORef [GLFWError],
    windowBuffer :: IORef [Window]
    }

emptyWindowBuffer :: [Window]
emptyWindowBuffer = []

emptyErrorBuffer :: [GLFWError]
emptyErrorBuffer = []

getTokenState :: (MonadIO m) => Token -> ExceptT GLFWSessionError m TokenGlobalState
getTokenState tk = do
       tkSt <- liftIO $ readIORef (tokenNaughtyBits tk)
       case tkSt of
          Left  _ -> throwE GLFWSessionErrorInvalidToken
          Right g -> return g

appendWindow :: (MonadIO m) => Token -> Window -> ExceptT GLFWSessionError m ()
appendWindow tk window = do
     tkSt <- getTokenState tk
     liftIO $ do
        currentWindows <- readIORef (windowBuffer tkSt)
        writeIORef (windowBuffer tkSt) (window : currentWindows)
{-
newWindow :: (MonadIO m) => Token -> [String] -> Int -> Int -> String -> ExceptT m GLFWSessionError Window
newWindow tk exts x y name = 
   liftIO $ do
       wh <- GLFW.createWindow x y name 
       return Window { extensionsDesired = Set.fromList exts,
                       windowHandle = wh,
                       stateWire = inhibit  
                     }
-} 

clearTokenErrorBuffer :: (MonadIO m) => Token -> ExceptT GLFWSessionError m ()
clearTokenErrorBuffer tk = do 
    tkState <- getTokenState tk
    liftIO $ writeIORef (errorBuffer tkState) []

--Initialize the token
emptyToken :: IO Token
emptyToken = do
   windows <- newIORef emptyWindowBuffer
   errors  <- newIORef emptyErrorBuffer
   tk <- newIORef (Right (TokenGlobalState errors windows))
   GLFW.setErrorCallback $ Just(getErrorHandler errors)
   return $ Token tk

newtype Token = Token { tokenNaughtyBits :: IORef (Either () TokenGlobalState) }
