import Data.List
import Data.Function
import Data.Char
import Data.List

import System.IO
import System.Directory
import System.FilePath.Windows

import Control.Monad
import Text.Printf

--source: https://github.com/ameingast/NaturalSort/blob/master/NaturalSort.hs
nSort   ::  [String] -> [String]
nSort s =   if (and . map allFloat) s then    
                let { readFloat = read :: String -> Float } 
                in  (map show . sortBy compare . map readFloat) s
            else    
                sortBy natComp s
    where allFloat  =   all (\x -> isDigit x || '.' == 'x')

natComp                                 ::  String -> String -> Ordering
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
--source: https://github.com/ameingast/NaturalSort/blob/master/NaturalSort.hs

main = do

    all <- getDirectoryContents "."
    cd  <- takeBaseName `fmap` getCurrentDirectory
    let sorted    = zip[1..] . nSort . filter ((isPrefixOf `on` reverse) ".jpg") $ all
    
    forM_ sorted $ \(i,x) -> do
    
        let z = if odd i then 1 else 2
        let q = ceiling (fromIntegral i / 2.0 )
        let fn = (printf "%s.%d.%d.jpg" cd (q::Int) (z::Int))
        
        printf "  %s --> %s" x fn
        doesFileExist fn >>= \b -> do 
            if b 
                then
                    printf "  <- File exist\n"
                else do
                    printf " <- Renamed\n"
                    renameFile x fn
    printf "FINISH\n"
    getChar