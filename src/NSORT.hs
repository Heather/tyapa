{-# LANGUAGE UnicodeSyntax #-}
module NSORT
  ( nSort
  ) where

import Data.List
import Data.Function
import Data.Char
import Data.IORef
import Data.Maybe

import Control.Monad
import Control.Applicative

import Text.Regex
{------------------------- Narural Sort algorithm --------------------------------------}
nSort   ::  [String] → [String]
nSort s =   if all allFloat s then    
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
{----------------------------------------------------------------------------------------}