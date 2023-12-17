import Network.Socket
import Control.Concurrent
import Control.Monad

main :: IO ()
main = withSocketsDo $ do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet 1234 iNADDR_ANY)
    listen sock 5
    putStrLn "Server listening on port 1234"

    forever $ do
        (client, _) <- accept sock
        putStrLn "Accepted connection"
        forkIO $ handleClient client

handleClient :: Socket -> IO ()
handleClient client = do
    send client "Welcome to the Haskell Chat Server!"
    forever $ do
        message <- recv client 1024
        putStrLn $ "Received message: " ++ message
