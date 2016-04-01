{-# LANGUAGE RecordWildCards #-}

--module HdtOptParseApplicative where

import Control.Applicative
import Control.Monad

import Options.Applicative




mySample :: Maybe Int
mySample = Just 4

toStr :: Maybe Int -> String
toStr Nothing = "<Nothing>"
toStr (Just x) = "<Just: " ++ (show x) ++ ">"


main :: IO ()
main = do
    --putStrLn "Hello, world"
    --line <- getLine
    --let line' = reverse line
    --putStrLn $ "You said: " ++ line' ++ "backwards"
    --putStrLn $ "Yes, you really said: " ++ line

    --line2 <- fmap reverse getLine 
    --putStrLn $ "You said: " ++ line2

    line2 <- fmap (++"??") getLine
    putStrLn $ "You said: " ++ line2

    let i2 = mySample
    let i3 = fmap (*3) i2
    putStrLn $ "HelloWorld:" ++ (toStr i3)





--data Clone = Clone
     --{
       --bare :: Bool
     --, depth :: Int
     --} deriving (Show)
 
--data Commit = Commit
     --{
       --dry_run :: Bool
     --, author :: String
     --} deriving (Show)
 
--data Commands = Commands
    --{
       --clone  :: Clone
     --, commit :: Commit
     --} deriving (Show)
     

--mainOptParse = execParser p_all >>= print


--main2 = do
    --mainOptParse


--p_all :: ParserInfo Commands
--p_all = info (helper <*> prsr) mod
    --where prsr = Commands
             ---- create clone and commit as subparsers
            -- <$> subparser (command "clone" p_clone)
            -- <*> subparser (command "commit" p_commit)
          --mod = fullDesc <> footer "footer"



--p_commit :: ParserInfo Commit
--p_commit = flip info mod . (helper <*>) $ Commit
   -- <$> flag False True (  short 'd' <> long "dry-run" <> help "dry run")
   -- <*> strOption       (  short 'a' <> long "author"  <> help "override author for commit"  <> metavar "<author>" )
    --where mod = fullDesc <> footer "commit footer"

--p_clone :: ParserInfo Clone
--p_clone = flip info mod . (helper <*>) $ Clone
   -- <$> flag False True (  short 'b' <> long "bare"    <> help "create a bare repository"     )
   -- <*> option auto     (  short 'd' <> long "depth"   <> help "create a shallow clone at that depth"  <> metavar "<depth>" )
    --where mod = fullDesc <> footer "clone footer"
