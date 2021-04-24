module Foreign.Wolfram.Link.WSTP.Functions
  where

import Foreign
import Foreign.C
import Prelude                         hiding (Bool)

import Foreign.Wolfram.Link.WSTP.Types

-- | Initializes the WSTP environment object with the given parameter set.
--
--   See [Wolfram documentation](https://reference.wolfram.com/language/ref/c/WSInitialize.html).
foreign import ccall "WSInitialize" initialize :: Ptr EnvParam -> IO (Ptr Env)

-- | Destructs the WSTP environment object.
--
--   See [Wolfram documentation](https://reference.wolfram.com/language/ref/c/WSDeinitialize.html).
foreign import ccall  "WSDeinitialize" deinitialize     :: Ptr Env -> IO ()
foreign import ccall "&WSDeinitialize" deinitialize_ptr :: FunPtr (Ptr Env -> IO ())

-- | Opens a WSTP connection, taking parameters from command-line arguments..
--
--   See [Wolfram documentation](https://reference.wolfram.com/language/ref/c/WSOpenArgcArgv.html).
foreign import ccall "WSOpenArgcArgv" openArgcArgv
    :: Ptr Env
    -> CInt
    -> Ptr CString
    -> Ptr Errno
    -> IO (Ptr Link)

-- | Closes a WSTP connection.
--
--   See [Wolfram documentation](https://reference.wolfram.com/language/ref/c/WSClose.html).
foreign import ccall  "WSClose" close     :: Ptr Link -> IO ()
foreign import ccall "&WSClose" close_ptr :: FunPtr (Ptr Link -> IO ())

-- | Activates a WSTP connection, waiting for the program at the other end to respond.
--
--   WARNING:
--   The function can block indefinitely unless the program at the other end will respond.
--
--   See [Wolfram documentation](https://reference.wolfram.com/language/ref/c/WSActivate.html).
foreign import ccall "WSActivate" activate :: Ptr Link -> IO Bool

-- | Returns a value identifying the last error to occur on link.
--
--   See [Wolfram documentation](https://reference.wolfram.com/language/ref/c/WSError.html).
foreign import ccall "WSError" error :: Ptr Link -> IO Errno

-- | Puts a null-terminated string of C characters to the WSTP connection.
--
--   See [Wolfram documentation](https://reference.wolfram.com/language/ref/c/WSPutString.html).
foreign import ccall "WSPutString" putString :: Ptr Link -> CString -> IO Bool

-- | Inserts an indicator in the expression stream that says the current expression is complete
--   and ready to be sent.
--
--   See [Wolfram documentation](https://reference.wolfram.com/language/ref/c/WSEndPacket.html).
foreign import ccall "WSEndPacket" endPacket :: Ptr Link -> IO Bool

-- | Flushes out any buffers containing data waiting to be sent on link.
--
--   See [Wolfram documentation](https://reference.wolfram.com/language/ref/c/WSFlush.html).
foreign import ccall "WSFlush" flush :: Ptr Link -> IO Bool

-- | Gets a character string from the WSTP connection.
--
--   See [Wolfram documentation](https://reference.wolfram.com/language/ref/c/WSGetString.html).
foreign import ccall "WSGetString" getString :: Ptr Link -> Ptr CString -> IO Bool

-- | Disowns memory allocated by 'getString' to store the character string.
--
--   See [Wolfram documentation](https://reference.wolfram.com/language/ref/c/WSReleaseString.html).
foreign import ccall "WSReleaseString" releaseString :: Ptr Link -> CString -> IO ()
