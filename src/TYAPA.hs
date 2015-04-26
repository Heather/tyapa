{-# LANGUAGE UnicodeSyntax #-}

import PhotoRenamer

import Text.Printf
import System.Environment( getArgs )
import System.Exit
import System.Console.GetOpt
import Control.Applicative

import qualified Paths_TYAPA as My
import Data.Version (showVersion)

main = do (actions, _, _) <- getOpt RequireOrder options <$> getArgs
          Options { optPath   = path
                  , optRename = renameOpt 
                  } <- foldl (>>=) (return defaultOptions) actions
          printf "\n  TYAPA v.%s\n\n" (showVersion My.version)
          renameOpt path

data Options = Options
    { optPath ∷ String
    , optRename ∷ String → IO()
    }

defaultOptions ∷ Options
defaultOptions = Options 
    { optPath = "."
    , optRename = doRename False
    }

options ∷ [OptDescr (Options → IO Options)]
options = [
    Option ['v'] ["version"] (NoArg showVer) "show Tyapa version number",
    Option ['h'] ["help"]    (NoArg showHelp) "display this help",
    Option ['f'] ["force"]   (NoArg forceRename) "force rename",
    Option ['p'] ["path"]    (ReqArg getp "STRING") "rename directory"
  ]

showVer _ = do
    printf "\n  TYAPA v.%s\n\n" (showVersion My.version)
        >> exitWith ExitSuccess

showHelp _ = do
    putStrLn $ usageInfo "Usage: TYAPA [optional things]" options
    exitWith ExitSuccess

getp arg opt = return opt { optPath = arg }
forceRename opt = return opt { optRename = doRename True }
