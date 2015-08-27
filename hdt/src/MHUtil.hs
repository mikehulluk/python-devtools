
{-# LANGUAGE OverloadedStrings #-}
module MHUtil where

pad :: Char -> Int -> String -> String
pad c cnt src = src ++ padding
    where  npadding = max (cnt - length src) 0
           padding = replicate npadding c
