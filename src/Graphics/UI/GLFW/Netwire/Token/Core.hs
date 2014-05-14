{-# LANGUAGE DeriveDataTypeable #-}

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
   Token(..)) where

import qualified Data.Set as Set
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef
import Data.Typeable(Typeable)
import Data.Data(Data)
import Control.Monad.Trans.Except
import System.IO.Unsafe
import Control.Monad.IO.Class
import Control.Exception

data GLFWError = GLFWError GLFW.Error String deriving (Eq,Ord,Data,Read,Show,Typeable)

type GLFWVersion = GLFW.Version

instance Exception GLFWError

--Un-enumerated errors in the GLFW library should ideally be prevented by the library
data GLFWSessionError = GLFWSessionErrorAlreadyInitialized
                      | GLFWSessionErrorAlreadyFinalized
                      | GLFWSessionErrorInitializationFailed
                      | GLFWSessionErrorInvalidToken
                      | GLFWSessionGLFWErrorOccured GLFWError
                      | GLFWSessionErrorOther String --Should never be used from netwire-glfw-b itself
   deriving(Eq,Show,Ord,Data,Read,Typeable)

instance Exception GLFWSessionError

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
                  else do initializeSuccess <- liftIO $ GLFW.init
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
getVoidedToken = fmap Token $ newIORef (Left ())

--Placeholder
type Window = ()

data TokenGlobalState = TokenGlobalState {
    errorBuffer  :: IORef [GLFWError],
    windowBuffer :: IORef [Window],
    glfwVersion  :: GLFWVersion
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

clearTokenErrorBuffer :: (MonadIO m) => Token -> ExceptT GLFWSessionError m ()
clearTokenErrorBuffer tk = do 
    tkState <- getTokenState tk
    liftIO $ writeIORef (errorBuffer tkState) []

--Initialize the token
emptyToken :: IO Token
emptyToken = do
   windows <- newIORef emptyWindowBuffer
   errors  <- newIORef emptyErrorBuffer
   version <- GLFW.getVersion
   tk <- newIORef (Right (TokenGlobalState errors windows version))
   GLFW.setErrorCallback $ Just(getErrorHandler errors)
   return $ Token tk

newtype Token = Token { tokenNaughtyBits :: (IORef (Either () TokenGlobalState)) }
