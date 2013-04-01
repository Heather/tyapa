import Data.List
import Data.Function

import System.IO
import System.Directory
import System.FilePath.Windows

import Control.Monad
import Text.Printf

main = do

    all <- getDirectoryContents "."
    cd  <- takeBaseName `fmap` getCurrentDirectory
    let sorted    = zip[1..] . sort . filter ((isPrefixOf `on` reverse) ".jpg") $ all
    
    forM_ sorted $ \(i,x) -> do
    
        let z = if odd i then 1 else 2
        let q = ceiling (fromIntegral i / 2.0 )
        let fn = (printf "%s.%d.%d.jpg" cd (q::Int) (z::Int))
        
        printf "  %s --> %s" x fn
        doesFileExist fn >>= \b -> do 
            if b then
                printf "  <- File exist\n"
            else do
                printf " <- Renamed\n"
                renameFile x fn