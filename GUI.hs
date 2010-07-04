
module GUI (runGUI) where

import Actions (cmdScan)
import Graphics.UI.Gtk
import ScanDB (getDefaultDBPath, initDB)

runGUI :: IO ()
runGUI = do
  db <- getDefaultDBPath >>= initDB
  initGUI
  window <- windowNew 
  mainVbox <- vBoxNew False 3
  set window [ windowTitle := "OneSock",
               containerChild := mainVbox ]

  scanAction <- actionNew "scan" "_Scan" (Just "Scan a new page") (Just stockNew)
  onActionActivate scanAction $ do
    cmdScan db

  toolbar <- toolbarNew
  actionCreateToolItem scanAction >>= containerAdd toolbar

  boxPackStart mainVbox toolbar PackNatural 0

  scansScrolledWindow <- scrolledWindowNew Nothing Nothing
  boxPackStart mainVbox scansScrolledWindow PackGrow 0

  statusbar <- statusbarNew
  boxPackStart mainVbox statusbar PackNatural 0

  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
