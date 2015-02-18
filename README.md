photo files renamer
===================

[![Build Status](https://travis-ci.org/Heather/tyapa.png?branch=master)](https://travis-ci.org/Heather/tyapa)

Usage
-----

``` shell
  Tyapa v.0.1.8

  btrhrthtr.png     --> TYAPA.1.1.PNG <- Renamed
  grtgrgre.jpg      --> TYAPA.1.2.JPG <- Renamed
  gtrhrthtr.png     --> TYAPA.2.1.PNG <- Renamed
  gtrhrtyjtrjt.png  --> TYAPA.2.2.PNG <- Renamed
  hytjjytj.jpg      --> TYAPA.3.1.JPG <- Renamed
  hytjkyt.jpg       --> TYAPA.3.2.JPG <- Renamed
  uikuykyu.jpg      --> TYAPA.4.1.JPG <- Renamed
  ythgrh.png        --> TYAPA.4.2.PNG <- Renamed

    skipped 0 files
    renamed 8 files
```

<img align="left" src="http://fc04.deviantart.net/fs70/f/2012/268/0/a/render_anime_girl_by_mato_kuroi26-d5fvrz6.png"/>

Code snippet
------------

``` haskell
{----------------------------------------------------------------------------------------}
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
all < getDirectoryContents "."
cd  < takeBaseName <$> getCurrentDirectory
let rx = printf "(\\%s)(\\.)([0-9]*)(\\.)([1-2])(\\.)." cd
-- >
let ziped = zip[1..] . nSort
          . filter (\x > any(`isSuffixOf` map toLower x)
                [".jpg", ".jpeg", ".png", ".gif", ".bmp"])
                    $ all
{----------------------------------------------------------------------------------------}
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
{----------------------------------------------------------------------------------------}
```

Thanks to :
 - https://github.com/ameingast for Natural Sort
 - #gentoo-haskell for teaching me some basics

<img align="left" src="http://fc06.deviantart.net/fs71/f/2012/062/8/6/render_takenaka_by_sakucitah-d4rlfjr.png"/>
