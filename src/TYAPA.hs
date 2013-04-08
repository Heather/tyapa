{-# LANGUAGE UnicodeSyntax, CPP #-}
import Data.List
import Data.Function
import Data.Char
import Data.List
import Data.IORef

import System.IO
import System.Directory

#if defined(mingw32_HOST_OS) || defined(__MINGW32__) 
import System.FilePath.Windows
#else
import System.FilePath.Posix
#endif

import Control.Monad
import Control.Applicative

import Data.Maybe
import Text.Regex

import Text.Printf
------------------------- Narural Sort algorithm ---------------------------------------
nSort   ::  [String] → [String]
nSort s =   if (and . map allFloat) s then    
                let { readFloat = read :: String → Float } 
                in  (map show . sortBy compare . map readFloat) s
            else    
                sortBy natComp s
    where allFloat  =   all (\x → isDigit x || '.' == 'x')

natComp                                 ::  String → String → Ordering
natComp [] []                           =   EQ
natComp [] _                            =   LT
natComp _ []                            =   GT
natComp xxs@(x:xs) yys@(y:ys)
    | noDigit x && noDigit y && x == y  =   natComp xs ys
    | noDigit x || noDigit y            =   compare x y
    | nx == ny                          =   natComp rx ry
    | otherwise                         =   compare nx ny
    where   (nx,rx)     =   getNumber xxs
            (ny,ry)     =   getNumber yys
            noDigit     =   not . isDigit
            getNumber s =   let { digits = takeWhile isDigit s }
                            in (read digits :: Integer, drop (length digits) s)
------------------------------------------------------------------------------------------
version = "0.1.4"
main    = do
    -- >
    all ← getDirectoryContents "."
    cd  ← takeBaseName <$> getCurrentDirectory
    let rx = printf "(\\%s)(\\.)([0-9]*)(\\.)([1-2])(\\.)." cd
    -- >
    let ziped = zip[1..] . nSort
              . filter (\x → any(`isSuffixOf` map toLower x)
                    [".jpg", ".jpeg", ".png", ".gif", ".bmp"])
                        $ all
    -- >
    skipped ← newIORef 0
    renamed ← newIORef 0
    -- >
    printf "\n  Tyapa v.%s\n\n" version
    forM_ ziped $ \(i,x) → do
        -- >             Naming logics
        let fn  = 
                printf "%s.%d.%d%s" cd (q::Int) (z::Int) s
                where s = map toUpper $ takeExtension x
                      q = ceiling $ fromIntegral i / 2.0
                      z = if odd i then 1 else 2
        -- >
        printf "  %s --> %s" x fn
        doesFileExist fn >>= \fx → do
            let nef = isNothing . matchRegex (mkRegex rx) $ x
            putStr $ if nef then "  < New >"
                            else "  < Already Renamed >"
            case fx of
                True -> do
                    printf "  <- File exist\n"  -- skipped + 1
                    counter ← readIORef skipped; writeIORef skipped $ counter + 1
                False -> do
                    printf " <- Renamed\n"      -- renamed + 1
                    counter ← readIORef renamed; writeIORef renamed $ counter + 1
                    renameFile x fn
                    
    -- >                    Statistics
    printf "\n"
    skpd ← readIORef skipped; printf "    skipped %d files\n" (skpd::Int)
    cntr ← readIORef renamed; printf "    renamed %d files\n" (cntr::Int)
    getChar
    -- >