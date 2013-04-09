photo files renamer
===================

``` haskell
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
{----------------------------------------------------------------------------------------}
version = "0.1.7"
main    = do
    -- >
    all < getDirectoryContents "."
    cd  < takeBaseName <$> getCurrentDirectory
    let rx = printf "(\\%s)(\\.)([0-9]*)(\\.)([1-2])(\\.)." cd
    -- >
    let ziped = zip[1..] . nSort
              . filter (\x > any(`isSuffixOf` map toLower x)
                    [".jpg", ".jpeg", ".png", ".gif", ".bmp"])
                        $ all
    skipped < newIORef 0; renamed < newIORef 0 {- IO REF: -}
    printf "\n  Tyapa v.%s\n\n" version        {-  Intro  -}
    forM_ ziped $ \(i,x) > do
        printf "  %s" x
        if isNothing . matchRegex (mkRegex rx) $ x 
            then let fn= printf "%s.%d.%d%s" cd (q::Int) (z::Int) s
                         where s = map toUpper $ takeExtension x
                               z = if odd i then 1 else 2
                               q = ceiling $ fromIntegral i / 2.00
                     frename fname fnewname = do
                         printf " <- Renamed\n"
                         counter < readIORef renamed
                         writeIORef renamed $ counter + 1
                         renameFile fname fnewname
                 in doesFileExist fn >>= \fileExist >
                        if fileExist 
                            then let tyap fi =      {- We are looking for free slot for the file -}
                                        if fi < i  {- Because it's the only reason for file to exist -}
                                            then let fnx = printf "%s.%d.%d%s" cd (q::Int) (z::Int) s
                                                         where s = map toUpper $ takeExtension x
                                                               z = if odd i then 1 else 2
                                                               q = ceiling (fromIntegral i / 2.00 ) - fi
                                                 in doesFileExist fnx >>= \fileExistx > 
                                                    if fileExistx
                                                        then tyap (fi + 1)
                                                        else printf " --> %s" fnx >> frename x fnx
                                            else do
                                                printf "  <- File exist\n"
                                                counter < readIORef skipped; writeIORef skipped $ counter + 1
                                 in tyap 1 {- Recursive i-1 -> 0 -}
```

thanks to 
 - https://github.com/ameingast/NaturalSort/blob/master/NaturalSort.hs for Natural Sort
 - #gentoo-haskell for teaching me some basics