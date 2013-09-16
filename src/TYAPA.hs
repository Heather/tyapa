{-# LANGUAGE UnicodeSyntax, CPP #-}
import PhotoRenamer

import Text.Printf
import System.Environment( getArgs )
import System.Exit
import System.Console.GetOpt

version = "1.0.0"
main = do
    args <- getArgs
    let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    let Options { optRename = rename } = opts
    printf "\n  TYAPA v.%s\n\n" version
    rename

data Options = Options  {
    optRename :: IO()
  }

defaultOptions :: Options
defaultOptions = Options {
    optRename = doRename "all"
  }

options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['v'] ["version"] (NoArg showVersion) "show Tyapa version number",
    Option ['h'] ["help"]    (NoArg showHelp) "display this help",
    Option ['r'] ["rename"]  (ReqArg getr "STRING") "rename rules"
  ]

showVersion _ = do
    printf "\n  TYAPA v.%s\n\n" version
        >> exitWith ExitSuccess
  
showHelp _ = do
    putStrLn $ usageInfo "Usage: TYAPA [optional things]" options
    exitWith ExitSuccess

getr arg opt = return opt { optRename = doRename arg }