import PhotoRenamer

import Text.Printf
import System.Environment( getArgs )
import System.Exit
import System.Console.GetOpt

version = "1.1.3"
main = do
    args <- getArgs
    let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    let Options { optPath = path
                , optRename = renameOpt } = opts
    printf "\n  TYAPA v.%s\n\n" version
    renameOpt path

data Options = Options  {
    optPath  :: String,
    optRename :: String -> IO()
  }

defaultOptions :: Options
defaultOptions = Options {
    optPath = ".",
    optRename = doRename False
  }

options :: [OptDescr (Options -> IO Options)]
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

getp arg opt = return opt { optPath = arg }
forceRename opt = return opt { optRename = doRename True }