
module GUI (runGUI) where

import Actions (UI, setStatus, notifyScanAdded, cmdScan)
import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust, isJust)
import Graphics.UI.Gtk
import ScanDB (getDefaultDBPath, initDB)

data GtkUI
  = GtkUI { uiStatusbar :: Statusbar
          , uiStatusContextId :: ContextId
          , uiStatusMessage :: IORef (Maybe MessageId)
          }

instance UI GtkUI where
  setStatus ui s = do
    currentMsg <- readIORef $ uiStatusMessage ui 
    when (isJust currentMsg) $ statusbarRemove (uiStatusbar ui) (uiStatusContextId ui) (fromJust currentMsg)
    newMsg <- statusbarPush (uiStatusbar ui) (uiStatusContextId ui) s
    writeIORef (uiStatusMessage ui) (Just newMsg)
    mainIterationDo False
    return ()

  notifyScanAdded ui s = return ()

runGUI :: IO ()
runGUI = do
  db <- getDefaultDBPath >>= initDB
  initGUI
  window <- windowNew 
  mainVbox <- vBoxNew False 3
  set window [ windowTitle := "OneSock",
               containerChild := mainVbox ]

  scanAction <- actionNew "scan" "_Scan" (Just "Scan a new page") (Just stockNew)

  toolbar <- toolbarNew
  actionCreateToolItem scanAction >>= containerAdd toolbar

  boxPackStart mainVbox toolbar PackNatural 0

  scansScrolledWindow <- scrolledWindowNew Nothing Nothing
  boxPackStart mainVbox scansScrolledWindow PackGrow 0

  statusbar <- statusbarNew
  boxPackStart mainVbox statusbar PackNatural 0

  statusContextId <- statusbarGetContextId statusbar "Actions"
  statusMessage <- newIORef Nothing
  let ui = GtkUI{ uiStatusbar = statusbar
                , uiStatusContextId = statusContextId
                , uiStatusMessage = statusMessage
                }

  onDestroy window mainQuit
  onActionActivate scanAction $ do
    cmdScan db ui

  widgetShowAll window
  mainGUI 
