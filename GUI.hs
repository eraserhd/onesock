
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

scansViewNew = do
  scansView <- treeViewNew

  scansStore <- listStoreNew ["1", "2", "3"]
  set scansView [ treeViewHeadersVisible := True
                , treeViewModel := scansStore
                ]

  -- "Scanned At" column
  dateCol <- treeViewColumnNew
  dateColRenderer <- cellRendererTextNew
  set dateCol [ treeViewColumnTitle := "Scanned At",
                treeViewColumnFixedWidth := 50,
                treeViewColumnResizable := True ]
  treeViewColumnPackStart dateCol dateColRenderer False
  treeViewAppendColumn scansView dateCol

  -- "Pages" column
  pageCol <- treeViewColumnNew
  pageColRenderer <- cellRendererTextNew -- FIXME
  set pageCol [ treeViewColumnTitle := "Pages",
                treeViewColumnResizable := True ]
  treeViewColumnPackStart pageCol pageColRenderer False
  treeViewAppendColumn scansView pageCol

  return scansView


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

  scansView <- scansViewNew

  scansScrolledWindow <- scrolledWindowNew Nothing Nothing
  set scansScrolledWindow [ containerChild := scansView ]

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
