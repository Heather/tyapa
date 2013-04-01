{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Function

import Filesystem 
import Filesystem.Path.CurrentOS
import qualified Data.Text as T

import Control.Applicative
import Control.Monad

main = do
    all <- map filename <$> (listDirectory =<< getWorkingDirectory) 
    forM_ (zip [1..] . sort $ all) $ \(i,f) -> 
        let z = i `mod` 2
            q = i `div` 2 + z
            f' = replaceExtensions f [T.pack . show $ q, T.pack . show $ 1+z, "jpg"]
        in isFile f' >>= flip unless (rename f f')