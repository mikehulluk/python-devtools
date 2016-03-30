{-# LANGUAGE OverloadedStrings #-}
module MHUtil(pad,trim, expandUser) where


import Data.Char (isSpace)
import System.Directory


pad :: Char -> Int -> String -> String
pad c cnt src = src ++ padding
    where  npadding = max (cnt - length src) 0
           padding = replicate npadding c


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace





expandUser :: String -> IO String
expandUser "~"         = do
    homeDir <- getHomeDirectory
    return homeDir
expandUser ('~':up)    = do
    homeDir <- getHomeDirectory
    return $ homeDir ++ up

expandUser p           = return p


