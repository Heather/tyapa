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

main = do

    all < getDirectoryContents "."
    cd  < takeBaseName `fmap` getCurrentDirectory

    let ziped = zip[1..] 
                . nSort
                . filter ((isPrefixOf `on` reverse. map toLower) ".jpg") 
                    $ all
```

thanks to 
 - https://github.com/ameingast/NaturalSort/blob/master/NaturalSort.hs for Natural Sort
 - #gentoo-haskell for teaching me some basics