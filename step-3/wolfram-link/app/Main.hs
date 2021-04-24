import           System.Environment
import           System.Exit
import           System.IO

import           Control.Monad.IO.Class
import qualified Foreign.Wolfram.Link   as WM

main :: IO ()
main = do
    [linkName, request] <- getArgs
    catchErrors =<< WM.runLink linkName (client request)

client :: String -> WM.LinkT IO ()
client request = do
    -- Send data.
    WM.putString request
    WM.endPacket
    WM.flush

    -- Receive data.
    liftIO . putStrLn =<< WM.getString

catchErrors :: Either WM.Error () -> IO ()
catchErrors = \case
    Left (WM.Error message _errno) -> do
        hPutStrLn stderr message
        exitFailure
    Right () ->
        exitSuccess
