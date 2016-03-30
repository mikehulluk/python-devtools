
{-# LANGUAGE RecordWildCards #-}

module ActionReplace where



import qualified Data.ByteString.Char8 as B

import Text.Regex
import Text.Regex.Posix   -- for regular expressions
import Text.Regex.Posix.String
import Text.Printf


import ActionApply
import HdtTypes
import CmdLineOpts
import HdtProject
import HdtFilePatchStack





execReplace :: MyOptions -> IO ()
execReplace _opts@Repl{..} = do

    -- Find all the files:
    files <- allActiveSrcFiles

    putStrLn $ printf "Replacing: '%s' with '%s' in %d files"  searchString replaceString (length files)
        

    -- Build the regular expression:
    regexCompRes <- compile defaultCompOpt execBlank searchString
    case regexCompRes of
        Left (retCode,errMsg) -> do
            putStrLn $ "Unable to compile regex: " ++ searchString
            putStrLn $ "Error: " ++ (show retCode) ++ " -- " ++ errMsg
            return ()
        Right compiledRegex -> do
            let description = printf "(s/%s/%s/)" searchString replaceString
            mapM_ (actionReplace compiledRegex replaceString description) files

            case noApply of 
                False -> do
                    putStrLn $ printf "\nApplying changes"
                    execApply (Apply{acceptAll=False} )
                True  -> do
                    printf "Not applying changes"
                    return ()
                    
            return ()


execReplace _ = error "execReplace() called with wrong option type"


actionReplace :: Regex -> String -> String-> File -> IO()
actionReplace searchRegex replStr description file = do
    x <- tryReplace searchRegex replStr file
    case x of
        Nothing -> do
            putStrLn $ printf "  No replacements made in: %s" $ filename file
        Just newContents -> do
            putStrLn $ printf "  Replacements made in: %s" $ filename file
            addFileOutstandingPatchs file description newContents



tryReplace :: Regex -> String -> File -> IO( Maybe B.ByteString)
tryReplace searchRegex replStr file = do
    oldContents <- readFile $ filename file
    let newContents = subRegex searchRegex oldContents replStr 
    if newContents==oldContents 
        then return Nothing 
        else return $ Just $ B.pack newContents





