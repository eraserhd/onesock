
module Main where

import Actions
import Control.Monad
import Data.Time.Clock
import GUI (runGUI)
import ImageUtil
import ScanDB (initDB, getDefaultDBPath)
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Random

data Mode = DoScan | StoreScan | RunGUI
data Flag = ModeFlag Mode

usageHeader = "Usage: OneSock [OPTION...]"
options =
  [ Option ['s']  ["scan"]        (NoArg (ModeFlag DoScan))     "scan and store to database"
  , Option ['S']  ["store-scan"]  (NoArg (ModeFlag StoreScan))  "store scan(s) to scan database"
  ]

findMode :: [Flag] -> Mode
findMode fs =
  let
    isModeFlag (ModeFlag m) = True
    isModeFlag _            = False
    ms = filter isModeFlag fs
  in
    case ms of
      []           -> RunGUI
      [ModeFlag m] -> m
      _            -> error "Only one mode argument can be supplied"

data CommandLineUI = CommandLineUI
instance UI CommandLineUI where
  setStatus _ = putStrLn
  notifyScanAdded _ _ = return ()

main :: IO ()
main = do
  args <- getArgs
  let (os,as,errs) = getOpt Permute options args
  when (errs /= []) $
    ioError $ userError $ concat errs ++ usageInfo usageHeader options
  case findMode os of
    DoScan -> do
      db <- getDefaultDBPath >>= initDB
      cmdScan db CommandLineUI
    StoreScan -> do
      db <- getDefaultDBPath >>= initDB
      case as of
        [] -> ioError $ userError $ "No scan files were supplied"
        _  -> mapM_ (cmdStoreScan db CommandLineUI) as
    RunGUI -> runGUI
