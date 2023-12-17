import Graphics.UI.Gtk
import Network.Socket
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
    _ <- initGUI
    window <- windowNew
    set window [windowTitle := "Haskell Chat Client", containerBorderWidth := 10]

    vBox <- vBoxNew False 5
    containerAdd window vBox

    chatView <- textViewNew
    textViewSetEditable chatView False
    textViewSetCursorVisible chatView False
    scrolledWindow <- scrolledWindowNew Nothing Nothing
    containerAdd scrolledWindow chatView
    boxPackStart vBox scrolledWindow PackGrow 0

    entry <- entryNew
    boxPackStart vBox entry PackNatural 0

    button <- buttonNewWithLabel "Send"
    boxPackStart vBox button PackNatural 0

    widgetShowAll window

    -- Connect to the server
    sock <- socket AF_INET Stream defaultProtocol
    hostEntry <- getHostByName "localhost"
    let serverAddr = SockAddrInet 1234 (hostAddress hostEntry)
    connect sock serverAddr

    -- Start a new thread to listen for messages
    forkIO $ forever $ do
        message <- recv sock 1024
        postGUIAsync $ appendText chatView (message ++ "\n")

    -- Event handler for the Send button
    onClicked button $ do
        text <- entryGetText entry
        send sock text
        entrySetText entry ""

    onDestroy window mainQuit
    mainGUI

appendText :: TextView -> String -> IO ()
appendText textView text = do
    buffer <- textViewGetBuffer textView
    endIter <- textBufferGetEndIter buffer
    textBufferInsert buffer endIter (text ++ "\n")
