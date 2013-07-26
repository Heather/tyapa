{-# LANGUAGE UnicodeSyntax, CPP #-}
import RNM

import Text.Printf
import System.Environment( getArgs )
import System.Exit
import System.Console.GetOpt

version = "0.2.1"
main = do
  args <- getArgs
  let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optRename = rename } = opts
  printf "\n  Tyapa v.%s\n\n" version
  rename

data Options = Options  {
    optRename :: IO()
  }

defaultOptions :: Options
defaultOptions = Options {
    optRename = rnm "all"
  }

options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['v'] ["version"] (NoArg showVersion) "show Tyapa version number",
    Option ['r'] ["rename"]  (ReqArg getr "STRING") "rename rules"
  ]

showVersion _ = do
  printf "\n  Tyapa v.%s\n\n" version
  exitWith ExitSuccess

getr arg opt = return opt { optRename = rnm arg }