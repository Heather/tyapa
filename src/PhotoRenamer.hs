{-# LANGUAGE CPP, UnicodeSyntax #-}

module PhotoRenamer
  ( doRename
  ) where

import NaturalSort

import Data.List
import Data.Function
import Data.Char
import Data.IORef
import Data.Maybe

import System.IO
import System.Directory

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.FilePath.Windows
#else
import System.FilePath.Posix
#endif

import Control.Monad
import Control.Applicative

import Text.Regex
import Text.Printf
{------------------------ Photo rename algorithm ------------------------------------} 
doRename ∷ Bool → String → IO()
doRename force path = do
    -- >
    all <- getDirectoryContents path
    cd  <-  case path of "." → takeBaseName <$> getCurrentDirectory
                         _   → return path
    let rx = printf "(\\%s)(\\.)([0-9]*)(\\.)([1-2])(\\.)." cd
        ziped = zip[1..] . nSort
              . filter (\x → any(`isSuffixOf` map toLower x)
                    [".jpg", ".jpeg", ".png", ".gif", ".bmp"])
                        $ all
    skipped <- newIORef 0 {- IO REF: -}
    renamed <- newIORef 0 {- IO REF: -}
    forM_ ziped $ \(i,xxx) → do
        let x = case path of "."  → xxx
                             _    → printf "%s\\%s" path xxx
        printf "  %s" x
        if or [force, ( isNothing . matchRegex (mkRegex rx) $ x )]
            then let fn= printf "%s.%d.%d%s" c (q∷Int) (z∷Int) s
                         where s = map toUpper $ takeExtension x
                               z = if odd i then 1 else 2
                               q = ceiling $ fromIntegral i / 2.00
                               c = case path of "." → cd
                                                _   → printf "%s\\%s" path cd
                     frename fname fnewname = do
                         printf " <- Renamed\n"
                         counter <- readIORef renamed
                         writeIORef renamed $ counter + 1
                         renameFile fname fnewname
                 in doesFileExist fn >>= \fileExist →
                        if fileExist 
                            then let tyap fi =    {- We are looking for free slot for the file -}
                                        if fi < i {- Because it's the only reason for file to exist -}
                                            then let fnx = printf "%s.%d.%d%s" c (q∷Int) (z∷Int) s
                                                         where s = map toUpper $ takeExtension x
                                                               z = if odd i then 1 else 2
                                                               q = ceiling (fromIntegral i / 2.00 ) - fi
                                                               c = case path of "." → cd
                                                                                _   → printf "%s\\%s" path cd
                                                 in doesFileExist fnx >>= \fileExistx → 
                                                    if fileExistx
                                                        then tyap (fi + 1)
                                                        else printf " --> %s" fnx >> frename x fnx
                                            else do
                                                printf "  <- File exist\n"
                                                counter <- readIORef skipped
                                                writeIORef skipped $ counter + 1
                                 in tyap 1 {- Recursive i-1 → 0 -}
                            else printf " --> %s" fn >> frename x fn
            else putStrLn "  <- File Already Renamed"

    -- >                    Statistics
    printf "\n"
    skpd <- readIORef skipped; printf "    skipped %d files\n" (skpd∷Int)
    cntr <- readIORef renamed; printf "    renamed %d files\n" (cntr∷Int)

    -- > Wait for key press (Only for windows)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    getChar >> return () -- return nothing but IO
#endif
{----------------------------------------------------------------------------------------}