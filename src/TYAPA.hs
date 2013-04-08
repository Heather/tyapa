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

import Text.Printf

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
                   
version = "0.1.2"
main    = do

    all ← getDirectoryContents "."
    cd  ← takeBaseName <$> getCurrentDirectory

    let ziped = zip[1..]
                . nSort
                . filter (\x -> any(`isSuffixOf` map toLower x)
                    [".jpg", ".jpeg", ".png", ".gif", ".bmp"])
                        $ all
                    
    skipped ← newIORef 0
    renamed ← newIORef 0
    
    printf "\n  Tyapa v.%s\n\n" version
    forM_ ziped $ \(i,x) → do
    
        let fn  = 
                printf "%s.%d.%d%s" cd (q::Int) (z::Int) sff
                where sff = map toUpper $ takeExtension x
                      q   = ceiling (fromIntegral i / 2.0 )
                      z   = if odd i then 1 else 2
                      
        printf "  %s --> %s" x fn
        doesFileExist fn >>= \fx → 
            case fx of
                True -> do
                    printf "  <- File exist\n"
                    counter ← readIORef skipped
                    writeIORef skipped $ counter + 1
                False -> do
                    printf " <- Renamed\n"
                    counter ← readIORef renamed
                    writeIORef renamed $ counter + 1
                    renameFile x fn
                    
    printf "\n"
    skpd ← readIORef skipped
    cntr ← readIORef renamed
    printf "    skipped %d files\n" (skpd::Int)
    printf "    renamed %d files\n" (cntr::Int)
    getChar