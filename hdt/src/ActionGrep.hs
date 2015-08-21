{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module ActionGrep where

import HdtTypes
import CmdLineOpts

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import Control.Monad (when)
import Data.List
import Data.Text.Format













import System.Console.ANSI
import Control.Monad
import Control.Exception

import Text.Regex.Posix   -- for regular expressions
import Text.Regex.Posix.String

-- Grepping:
-- ^^^^^^^^^^^^^^^^^^^^^^^^
execGrep :: MyOptions -> IO ()
execGrep opts@ModeGrep{..} = do
    putStrLn $ "Grep-time!"
    -- Get the active projects
    projects <- getAllProjectConfigs
    let activeProjects = filter isActive projects

    -- Compile the regular expression:
    regexCompRes <- compile defaultCompOpt execBlank grepString
    case regexCompRes of
        Left wrapError -> do
            putStrLn "Unable to compile"
            return ()
        Right compiledRegex -> do
            putStrLn "Compiled OK"

            forM activeProjects (grepProject compiledRegex opts)
            return ()

grepProject ::  Regex -> MyOptions -> Project -> IO ()
grepProject compiledRegex opts project = do
    forM (srcFiles project) (execGrepFile compiledRegex opts)
    return ()


execGrepFile :: Regex -> MyOptions -> String -> IO ()
execGrepFile compiledRegex opts filename= do
    contents <- catch (readFile filename)
        (\e -> do
                  let err = show (e :: IOException)
                  putStrLn ("Warning: Couldn't open " ++ filename ++ ": " ++ err)
                  return "")

    let ls = lines contents
    let ils = zip [0..] ls

    -- Find all the matching lines:
    grepLinesAll <- mapM (grepLine compiledRegex) ils
    let grepLines = concat(grepLinesAll)


    -- Add in context lines
    let nContextLines = 3
    let nLinesFile = (length ls)
    let linesIncludingContext = addContextLinesNew nContextLines nLinesFile grepLines

    let groupedLinesPrinted = groupLines linesIncludingContext

    -- Print out the lines by group
    case length groupedLinesPrinted of
        0 -> return ()
        _ -> do
            putStrLn $ "In file:" ++ filename
            mapM (printGroupLines ls) groupedLinesPrinted
            return ()

    return ()


data GrepLineMatch = GrepLineMatch (String, String, String, [String]) Int deriving (Data, Typeable, Show, Eq)
data GrepLinePrinted = MatchLine GrepLineMatch | ContextLine Int deriving (Data, Typeable, Show, Eq)

instance Ord GrepLinePrinted where
    compare a b = compare (grepLineNum a) (grepLineNum b)




grepLine :: Regex -> (Int, String) -> IO [GrepLineMatch]
grepLine compiledRegex (lineNo, line) = do
    result <- regexec compiledRegex line
    case result of
        Left (returnCode, errorStr) -> return []
        Right match -> case match of
            Nothing -> return []
            Just (pre, matched, post,subexpression) -> return [ GrepLineMatch (pre, matched, post,subexpression) lineNo ]



grepLineNum :: GrepLinePrinted -> Int
grepLineNum (MatchLine m0) = l where (GrepLineMatch _ l) = m0
grepLineNum (ContextLine l0) = l0

addContextLinesNew :: Int -> Int -> [GrepLineMatch] -> [GrepLinePrinted]
addContextLinesNew nContextLines nLinesFile grepLines =
    sort printedLines
    where linesWithGrep = map (grepLineNum . MatchLine) grepLines
          possibleContextLines = nub  $ concat [ [l-nContextLines..l+nContextLines] | l <- linesWithGrep]
          contextLines = [i | i<- possibleContextLines, not( i `elem` linesWithGrep), i>=0, i<nLinesFile]
          printedLines = (map MatchLine grepLines ) ++ [ ContextLine i | i <- contextLines]




groupLines :: [GrepLinePrinted] -> [ [GrepLinePrinted] ]
groupLines x = groupLines' [] x 
--groupLines x = [x, x]


groupLines' :: [GrepLinePrinted] -> [GrepLinePrinted] -> [[GrepLinePrinted]]
--groupLines' currentBlk remainingLines = ??

groupLines' x [] = [x]
groupLines' [] (x:xs) = groupLines' [x] xs
groupLines' currentBlk (x:xs)
        | thisLineNo > (lastLineNo + maxSep) = [currentBlk] ++ (groupLines' [x] xs )             -- New Block
        | otherwise  = groupLines' (currentBlk ++ [x]) xs


    where maxSep = 1
          lastLineNo = grepLineNum $ last currentBlk
          thisLineNo = grepLineNum x






printGrepLineNo :: (Maybe Int) -> Int -> String
printGrepLineNo Nothing _ = ""
printGrepLineNo (Just lineNumberWidth) x = x_out --(show x) ++ ":"
    where suffix = ": "
          x_str = (show x) ++ ": "
          padding_needed = (lineNumberWidth+ (length suffix)) - (length x_str) + 1
          padding = ( concat $ (replicate padding_needed " ") )
          x_out = padding ++ x_str

printGrepLine :: [String] -> (Maybe Int) -> GrepLinePrinted  -> IO ()
printGrepLine allLines lineNumberWidth (MatchLine m) = do
    putStr $ printGrepLineNo lineNumberWidth lineNo
    --putStr $ (show lineNo) ++ " :   "
    setSGR [SetColor Foreground Dull White]
    putStr $ pre
    setSGR [SetColor Foreground Vivid Green]
    putStr $ matched
    setSGR [SetColor Foreground Dull White]
    putStr $ post ++ "\n"
    setSGR []
    where GrepLineMatch (pre, matched, post,subexpression) lineNo = m



printGrepLine allLines lineNumberWidth (ContextLine lineNo)  = do
    putStr $ printGrepLineNo lineNumberWidth lineNo
    setSGR [SetColor Foreground Dull White]
    putStr $ (allLines!!lineNo)
    putStr $ "\n"
    setSGR []


--grepLinePrintMatch :: (String, String, String, [String]) -> Int -> [String] -> IO()
--grepLinePrintMatch (pre, matched, post,subexpression) lineNo allLines = do
--    putStr $ (show lineNo) ++ " : "
--    setSGR [SetColor Foreground Dull White]
--    putStr $ pre
--    setSGR [SetColor Foreground Vivid Green]
--    putStr $ matched
--    setSGR [SetColor Foreground Dull White]
--    putStr $ post ++ "\n"
--    setSGR []


printGroupLines :: [String] -> [GrepLinePrinted] -> IO ()
printGroupLines allLines lines = do
    putStrLn "Printing.."
    let lineNumberWidth = Just 3
    mapM (printGrepLine allLines lineNumberWidth) lines
    return ()




--Ord GrepLinePrinted










