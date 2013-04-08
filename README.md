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
                   
    all < getDirectoryContents "."
    cd  < takeBaseName <$> getCurrentDirectory

    let ziped = zip[1..]
                . nSort
                . filter (\x -> any(`isSuffixOf` map toLower x)
                    [".jpg", ".jpeg", ".png", ".gif", ".bmp"])
                        $ all
                    
    printf "\n  Tyapa v.%s\n\n" version
    forM_ ziped $ \(i,x) > do
    
        let fn  = 
                printf "%s.%d.%d%s" cd (q::Int) (z::Int) sff
                where sff = map toUpper $ takeExtension x
                      q   = ceiling (fromIntegral i / 2.0 )
                      z   = if odd i then 1 else 2
```

thanks to 
 - https://github.com/ameingast/NaturalSort/blob/master/NaturalSort.hs for Natural Sort
 - #gentoo-haskell for teaching me some basics