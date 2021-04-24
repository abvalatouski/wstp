module Foreign.Wolfram.Link
    ( Link
    , mkInactiveLink

    , Error (Error, errorMessage, errorCode)

    , LinkT
    , runLink

    , putString
    , endPacket
    , flush
    , getString
    )
  where

import           Foreign
import           Foreign.C

import           Control.Monad.Except
import           Control.Monad.State

import qualified Foreign.Wolfram.Link.WSTP.Functions as WSTP
import qualified Foreign.Wolfram.Link.WSTP.Types     as WSTP

data Link = Link
    { linkName :: {-# UNPACK #-} !String
    , linkEnv  :: {-# UNPACK #-} !(Ptr WSTP.Env)
    , linkPtr  :: {-# UNPACK #-} !(Ptr WSTP.Link)
    }

mkInactiveLink :: String -> Link
mkInactiveLink name = Link name env link
  where
    env  = nullPtr
    link = nullPtr

data Error = Error
    { errorMessage :: String
    , errorCode    :: Maybe Errno
    }
  deriving Show

newtype LinkT m a = LinkT
    { runLinkT :: ExceptT Error (StateT Link m) a
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError Error
    , MonadIO
    , MonadState Link
    )

-- | Connects to a link with the given names
--   and waits until the program at the other end will respond.
--
--   WARNING:
--   This function can block indefinitely, waiting for the response. 
runLink :: MonadIO m => String -> LinkT m a -> m (Either Error a)
runLink name action = do
    let link    = mkInactiveLink name
        action' = initLibrary *> connect *> activate *> action
    (result, link') <- runStateT (runExceptT (runLinkT action')) link
    releaseLink link'
    pure result

releaseLink :: MonadIO m => Link -> m ()
releaseLink (Link _ env ptr) = liftIO $ do
    unless (ptr == nullPtr) $
        WSTP.close ptr
    unless (env == nullPtr) $
        WSTP.deinitialize env

-- TODO: Refactor.
connect :: MonadIO m => LinkT m ()
connect =  do
    name <- gets linkName
    env  <- gets linkEnv
    ptr  <- wrapEither <=< liftIO $ do
        let argc = length argv
            argv =
                [ "-mathlink"
                , "-linkname"     , name
                , "-linkprotocol" , "TCPIP"
                , "-linkmode"     , "connect"
                ]
        alloca $ \errnoPtr -> do
            argv'  <- mapM newCString argv
            argv'' <- newArray argv'
            poke errnoPtr eOK
            link' <- WSTP.openArgcArgv env (fromIntegral argc) argv'' errnoPtr
            errno <- peek errnoPtr
            free argv''
            mapM_ free argv'
            if link' == nullPtr || errno /= eOK
                then pure $ Left $ Error "Failed to create WSTP connection." (Just errno)
                else pure $ Right link'
    modify' $ \link -> link { linkPtr = ptr }

initLibrary :: MonadIO m => LinkT m ()
initLibrary = do
    let envParams = nullPtr
    env <- liftIO $ WSTP.initialize envParams
    if env == nullPtr
        then do
            let errorCode = Nothing
            throwError $ Error "Failed to initialize WSTP library functions." errorCode
        else
            modify' $ \link -> link { linkEnv = env }

-- | Activates a WSTP connection, waiting for the program at the other end to respond.
--
--   WARNING:
--   This function can block indefinitely, unless the program at the other end will respond. 
activate :: MonadIO m => LinkT m ()
activate = withLinkPtr_ "Failed to activate WSTP connection." WSTP.activate

-- | Puts a null-terminated string of C characters to the WSTP connection.
putString :: MonadIO m => String -> LinkT m ()
putString string = do
    link <- gets linkPtr
    wrapEither <=< liftIO $ do
        string' <- newCString string
        result  <- wrapWSTP "Failed to put a C string onto WSTP connection." $ WSTP.putString link string'
        free string'
        pure result

-- | Inserts an indicator in the expression stream that says the current expression is complete
--   and ready to be sent.
endPacket :: MonadIO m => LinkT m ()
endPacket = withLinkPtr_ "Failed to end WSTP packet." WSTP.endPacket

-- | Flushes out any buffers containing data waiting to be sent on link.
flush :: MonadIO m => LinkT m ()
flush = withLinkPtr_ "Failed to flush WSTP buffer." WSTP.flush

-- | Flushes out any buffers containing data waiting to be sent on link.
getString :: MonadIO m => LinkT m String
getString = do
    link <- gets linkPtr
    wrapEither <=< liftIO $ alloca $ \stringPtr -> do
        result <- wrapWSTP "Failed to read a C string from WSTP connection" $ WSTP.getString link stringPtr
        case result of
            Left errorMessage ->
                pure $ Left errorMessage
            Right () -> do
                string <- peek stringPtr
                Right <$> peekCString string

-- Utils.

wrapWSTP :: String -> IO WSTP.Bool -> IO (Either Error ())
wrapWSTP errorMessage action = do
    WSTP.Bool result <- action
    if result /= 0
        then pure $ Right ()
        else do
            let errorCode = Nothing
            pure $ Left $ Error errorMessage errorCode

wrapEither :: Monad m => Either Error a -> LinkT m a
wrapEither = either throwError pure

withLinkPtr_ :: MonadIO m => String -> (Ptr WSTP.Link -> IO WSTP.Bool) -> LinkT m ()
withLinkPtr_ errorMessage f =
    wrapEither =<< (liftIO . wrapWSTP errorMessage . f) =<< gets linkPtr
