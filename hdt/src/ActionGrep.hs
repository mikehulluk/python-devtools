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


import Data.Char (isSpace)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace



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
    forM (map filename (srcFiles project)) (execGrepFile compiledRegex opts)
    return ()


execGrepFile :: Regex -> MyOptions -> String -> IO ()
execGrepFile compiledRegex opts filename= do
    contents <- catch (readFile filename)
        (\e -> do
                  let err = show (e :: IOException)
                  putStrLn ("Warning: Couldn't open " ++ filename ++ ": " ++ err)
                  return "")

    --putStrLn $ show opts


    let ls = lines contents
    let ils = zip [0..] ls

    -- Find all the matching lines:
    grepLinesAll <- mapM (grepLine compiledRegex) ils
    let grepLines = concat(grepLinesAll)

    case length grepLines of 
        0 -> return ()
        _ -> do
            -- Add in context lines
            let nContextLines_ = nContextLines opts --3
            let nLinesFile = (length ls)


            case nContextLines_ of
                -- No context?
                0 -> do
                    let defaultLineNumberWidth = Just 1
                    let lineNumberWidth = if lineNumbers opts then defaultLineNumberWidth else Nothing
                    let includeFilename = True
                    let optfilename = if includeFilename then (Just filename) else Nothing
                    mapM (  printLineSimple lineNumberWidth optfilename ) grepLines
                    return ()

                -- With context:
                _ -> do

                    -- By default, print line numbers:
                    let defaultLineNumberWidth = Just 4
                    let lineNumberWidth = if lineNumbers opts then defaultLineNumberWidth else Nothing

                    -- Add context lines, group the lines, then strip empty leading/trailing context lines:
                    let linesIncludingContext = addContextLinesNew nContextLines_ nLinesFile grepLines
                    let groupedLinesPrinted = groupLines linesIncludingContext
                    let groupedLinesPrintedStripped = map (stripEmptyContextLines ls)  groupedLinesPrinted

                    -- Print out the lines by group
                    case length groupedLinesPrinted of
                        0 -> return ()
                        _ -> do
                            setSGR [SetColor Foreground Dull Yellow]
                            putStrLn $ filename
                            setSGR []
                            mapM (printGroupLines ls filename lineNumberWidth) groupedLinesPrintedStripped
                            return ()

    return ()



printLineSimple :: PrintLineNumberWidth -> Maybe String -> GrepLineMatch -> IO () 
printLineSimple    lineNumberWidth filename lineMatch = do 
    putStr $ maybe "" (++":") filename
    (printGrepLine [] lineNumberWidth ) $ MatchLine lineMatch
    return ()

data GrepLineMatch = GrepLineMatch (String, String, String, [String]) Int deriving (Data, Typeable, Show, Eq)
data GrepLinePrinted = MatchLine GrepLineMatch | ContextLine Int deriving (Data, Typeable, Show, Eq)

type PrintLineNumberWidth = Maybe Int

instance Ord GrepLinePrinted where
    compare a b = compare (grepLineNum a) (grepLineNum b)


stripEmptyContextLines ::  [String] -> [GrepLinePrinted] ->[GrepLinePrinted] 
stripEmptyContextLines allLines x = striphead $ striptail x
    where striphead = stripEmptyContextLinesHeads allLines
          striptail = reverse . (stripEmptyContextLinesHeads allLines) . reverse

isEmptyLine :: [String] -> GrepLinePrinted -> Bool
isEmptyLine allLines (ContextLine l) =  (trim (allLines!!l) ) == ""
isEmptyLine allLines _ = False

stripEmptyContextLinesHeads ::  [String] -> [GrepLinePrinted] ->[GrepLinePrinted] 
stripEmptyContextLinesHeads allLines []  = []
stripEmptyContextLinesHeads allLines [x] = if isEmptyLine allLines x then [] else [x] 
stripEmptyContextLinesHeads allLines (x:xs) 
    | isEmptyLine allLines x = stripEmptyContextLinesHeads allLines xs 
    | otherwise = (x:xs)

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






printGrepLineNo :: (PrintLineNumberWidth) -> Int -> String
printGrepLineNo Nothing _ = ""
printGrepLineNo (Just lineNumberWidth) x = x_out
    where suffix = ": "
          x_str = (show x) ++ ": "
          padding_needed = (lineNumberWidth+ (length suffix)) - (length x_str) + 1
          padding = ( concat $ (replicate padding_needed " ") )
          x_out = padding ++ x_str

printGrepLine :: [String] -> PrintLineNumberWidth -> GrepLinePrinted  -> IO ()
printGrepLine allLines lineNumberWidth (MatchLine m) = do
    setSGR [SetColor Foreground Vivid Blue]
    putStr $ printGrepLineNo lineNumberWidth lineNo
    setSGR [SetColor Foreground Dull White]
    putStr $ pre
    setSGR [SetColor Foreground Vivid Green]
    putStr $ matched
    setSGR [SetColor Foreground Dull White]
    putStr $ post ++ "\n"
    setSGR []
    where GrepLineMatch (pre, matched, post,subexpression) lineNo = m



printGrepLine allLines lineNumberWidth (ContextLine lineNo)  = do
    setSGR [SetColor Foreground Vivid Blue]
    putStr $ printGrepLineNo lineNumberWidth lineNo
    setSGR [SetColor Foreground Dull White]
    putStr $ (allLines!!lineNo)
    putStr $ "\n"
    setSGR []




printGroupLines :: [String] -> String -> PrintLineNumberWidth -> [GrepLinePrinted] -> IO ()
printGroupLines allLines filename lineNumberWidth lines = do
    --putStrLn "Printing.."
    putStrLn $ "In file:" ++ filename
    --let lineNumberWidth = Just 3
    mapM (printGrepLine allLines lineNumberWidth) lines
    return ()




--Ord GrepLinePrinted










