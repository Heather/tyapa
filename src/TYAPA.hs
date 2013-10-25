{-# LANGUAGE UnicodeSyntax, CPP #-}
import PhotoRenamer

import Text.Printf
import System.Environment( getArgs )
import System.Exit
import System.Console.GetOpt

version = "1.1.2"
main = do
    args <- getArgs
    let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    let Options { optRename = renameOpt } = opts
    printf "\n  TYAPA v.%s\n\n" version
    renameOpt

data Options = Options  {
    optRename :: IO()
  }

defaultOptions :: Options
defaultOptions = Options {
    optRename = doRename False "."
  }

options :: [OptDescr (Options → IO Options)]
options = [
    Option ['v'] ["version"] (NoArg showVersion) "show Tyapa version number",
    Option ['h'] ["help"]    (NoArg showHelp) "display this help",
    Option ['f'] ["force"]   (NoArg forceRename) "force rename",
    Option ['p'] ["path"]    (ReqArg getp "STRING") "rename directory"
  ]

showVersion _ = do
    printf "\n  TYAPA v.%s\n\n" version
        >> exitWith ExitSuccess
  
showHelp _ = do
    putStrLn $ usageInfo "Usage: TYAPA [optional things]" options
    exitWith ExitSuccess

forceRename _ = 
    doRename True "." 
        >> exitWith ExitSuccess

getp arg opt = return opt { optRename = doRename True arg }