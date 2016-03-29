
{-# LANGUAGE RecordWildCards #-}

module ActionReplace where

import ActionApply
import HdtTypes
import CmdLineOpts
import HdtProject
import HdtFilePatchStack

--import Data.String.Utils
--import Data.List

import qualified Data.ByteString.Char8 as B

import Text.Regex
import Text.Regex.Posix   -- for regular expressions
import Text.Regex.Posix.String

--cleanCmdlineString :: String -> Maybe String
--cleanCmdlineString x = do
--    x0 <- stripPrefix "'" x
--    x1 <- stripPrefix "'" (reverse x0)
--    return ( reverse x1)




execReplace :: MyOptions -> IO ()
execReplace _opts@Repl{..} = do


    putStrLn $ "[Pre-clean:] Replacing " ++ searchString ++ " with " ++ replaceString ++ ""
    putStrLn $ "Replacing " ++ searchString ++ " with " ++ replaceString ++ ""
    
    -- Get the active projects
    activeProjects <- getActiveProjects

    -- Find all the files:
    allfiles' <- mapM srcFiles activeProjects
    let files = concat allfiles'

    -- Check no files belong to more than one project:
    let allFullFilename = map filename files
    putStrLn $ unlines allFullFilename

    -- Build the regular expression:
    regexCompRes <- compile defaultCompOpt execBlank searchString
    case regexCompRes of
        Left (retCode,errMsg) -> do
            putStrLn $ "Unable to compile regex: " ++ searchString
            putStrLn $ "Errorcode: " ++ (show retCode) ++ " -- " ++ errMsg
            return ()
        Right compiledRegex -> do
            putStrLn "Compiled OK"

            mapM_ (actionReplace compiledRegex replaceString) files

            case  noApply of 
                False -> do
                    execApply (Apply)
                True  -> do
                    return ()
                    
            return ()


execReplace _ = error "execReplace() called with wrong option type"


actionReplace :: Regex -> String -> File -> IO()
actionReplace searchRegex replStr file = do
    x <- tryReplace searchRegex replStr file
    case x of
        Nothing -> return ()
        Just newContents -> do
            putStrLn $ "<Replacements made in : " ++ (filename file) ++ " ]"
            let description = "(s/" ++ "???" ++ "/" ++ replStr ++ ")"
            addFileOutstandingPatchs file description newContents



tryReplace :: Regex -> String -> File -> IO( Maybe B.ByteString)
tryReplace searchRegex replStr file = do
    oldContents <- readFile $ filename file
    let newContents = subRegex searchRegex oldContents replStr 
    if newContents==oldContents 
        then return Nothing 
        else return $ Just $ B.pack newContents





