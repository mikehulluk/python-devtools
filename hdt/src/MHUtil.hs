
{-# LANGUAGE OverloadedStrings #-}
module MHUtil(pad,trim) where

import Data.Char (isSpace)

pad :: Char -> Int -> String -> String
pad c cnt src = src ++ padding
    where  npadding = max (cnt - length src) 0
           padding = replicate npadding c


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
