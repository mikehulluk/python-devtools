
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
execReplace opts@Repl{..} = do


    putStrLn $ "[Pre-clean:] Replacing " ++ searchString ++ " with " ++ replaceString ++ ""
    -- Clean up the input strings we expect them to be enclosed in quotes, which we get rid of:
    --let searchString' = cleanCmdlineString (searchString)
    ----let searchString' = cleanCmdlineString (searchString)

    ----putStrLn x
    --searchStr <- case searchString' of 
    --    Nothing -> error "KL:" --return "" --error "??"
    --    Just s -> return s


    putStrLn $ "Replacing " ++ searchString ++ " with " ++ replaceString ++ ""
    -- Get the active projects
    projects <- getAllProjectConfigs
    let activeProjects = filter isActive projects

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
                    return ()
                True  -> do
                    execApply (ModeApply)
                    
            return ()


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
    --let newContents  = replace findStr replStr oldContents
    if newContents==oldContents then return Nothing else return $ Just $ B.pack newContents





