{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module ActionGrep(execGrep) where

import HdtTypes
import CmdLineOpts
import HdtProject
import MHUtil
import System.Console.CmdArgs


import Control.Monad
import Data.List




import System.Console.ANSI
import Control.Exception

import Text.Regex.Posix
import Text.Regex.Posix.String




type FileLineType = String





data GrepLineMatch = GrepLineMatch (String, String, String, [String]) Int deriving (Data, Typeable, Show, Eq)
data GrepLinePrinted = MatchLine GrepLineMatch | ContextLine Int FileLineType deriving (Data, Typeable, Show, Eq)

type PrintLineNumberWidth = Maybe Int

instance Ord GrepLinePrinted where
    compare a b = compare (grepLineNum a) (grepLineNum b)


-- Grepping:
-- ^^^^^^^^^^^^^^^^^^^^^^^^
execGrep :: MyOptions -> IO ()
execGrep opts@Grep{..} = do
    -- Get the active projects
    projects <- getAllProjectConfigs
    let activeProjects = filter isActive projects

    -- Compile the regular expression:
    regexCompRes <- compile defaultCompOpt execBlank grepString
    case regexCompRes of
        Left (retCode,errMsg) -> do
            putStrLn $ "Unable to compile regex: " ++ grepString
            putStrLn $ "Errorcode: " ++ (show retCode) ++ " -- " ++ errMsg
            return ()
        Right compiledRegex -> do
            putStrLn "Compiled OK"

            forM_ activeProjects (grepProject compiledRegex opts)
            return ()

grepProject ::  Regex -> MyOptions -> Project -> IO ()
grepProject compiledRegex opts proj = do
    srcfiles <- srcFiles proj
    forM_ (map filename srcfiles) (execGrepFile compiledRegex opts)
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
    let grepLines = concat grepLinesAll

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
                    let optfilename = if includeFilename then Just filename else Nothing
                    mapM_ (  printLineSimple lineNumberWidth optfilename ) grepLines
                    return ()

                -- With context:
                _ -> do
                    -- By default, print line numbers:
                    let defaultLineNumberWidth = Just 4
                    let lineNumberWidth = if lineNumbers opts then defaultLineNumberWidth else Nothing

                    -- Add context lines, group the lines, then strip empty leading/trailing context lines:
                    let linesIncludingContext = addContextLinesNew nContextLines_ nLinesFile ls grepLines
                    let groupedLinesPrinted = groupLines linesIncludingContext
                    let groupedLinesPrintedStripped = map stripEmptyContextLines  groupedLinesPrinted

                    -- Print out the lines by group
                    case length groupedLinesPrinted of
                        0 -> return ()
                        _ -> do
                            setSGR [SetColor Foreground Dull Yellow]
                            putStrLn  filename
                            setSGR []
                            mapM_ (printGroupLines filename lineNumberWidth) groupedLinesPrintedStripped
                            return ()

    return ()



printLineSimple :: PrintLineNumberWidth -> Maybe String -> GrepLineMatch -> IO ()
printLineSimple    lineNumberWidth filename lineMatch = do
    putStr $ maybe "" (++":") filename
    (printGrepLine lineNumberWidth ) $ MatchLine lineMatch
    return ()



stripEmptyContextLines ::  [GrepLinePrinted] ->[GrepLinePrinted]
stripEmptyContextLines x = striphead $ striptail x
    where striphead = stripEmptyContextLinesHeads
          striptail = reverse . stripEmptyContextLinesHeads  . reverse

isEmptyLine :: GrepLinePrinted -> Bool
isEmptyLine (ContextLine _ lineContents ) =  trim lineContents  == ""
isEmptyLine _ = False

stripEmptyContextLinesHeads :: [GrepLinePrinted] ->[GrepLinePrinted]
stripEmptyContextLinesHeads  []  = []
stripEmptyContextLinesHeads  [x] = if isEmptyLine  x then [] else [x]
stripEmptyContextLinesHeads  (x:xs)
    | isEmptyLine x = stripEmptyContextLinesHeads xs
    | otherwise = x:xs

grepLine :: Regex -> (Int, FileLineType) -> IO [GrepLineMatch]
grepLine compiledRegex (lineNo, line) = do
    result <- regexec compiledRegex line
    case result of
        Left (retCode, errMsg) -> do
            putStrLn $ "Unexpected error grepping line " ++ (show lineNo)
            putStrLn $ line
            putStrLn $ "Error: " ++ (show retCode) ++ " -- " ++ errMsg
            return []

        Right matchLine -> case matchLine of
            Nothing -> return []
            Just (pre, matched, post,subexpression) -> return [ GrepLineMatch (pre, matched, post,subexpression) lineNo ]



grepLineNum :: GrepLinePrinted -> Int
grepLineNum (MatchLine m0) = l where (GrepLineMatch _ l) = m0
grepLineNum (ContextLine lineNo _) = lineNo

addContextLinesNew :: Int -> Int -> [FileLineType] -> [GrepLineMatch] -> [GrepLinePrinted]
addContextLinesNew nContextLines nLinesFile allLines grepLines =
    sort printedLines
    where linesWithGrep = map (grepLineNum . MatchLine) grepLines
          possibleContextLines = nub  $ concat [ [l-nContextLines..l+nContextLines] | l <- linesWithGrep]
          contextLines = [i | i<- possibleContextLines, i `notElem` linesWithGrep, i>=0, i<nLinesFile]
          printedLines = (map MatchLine grepLines ) ++ [ ContextLine i (allLines!!i) | i <- contextLines]




groupLines :: [GrepLinePrinted] -> [ [GrepLinePrinted] ]
groupLines = groupLines' []


groupLines' :: [GrepLinePrinted] -> [GrepLinePrinted] -> [[GrepLinePrinted]]
groupLines' x [] = [x]
groupLines' [] (x:xs) = groupLines' [x] xs
groupLines' currentBlk (x:xs)
        | thisLineNo > (lastLineNo + maxSep) = currentBlk:groupLines' [x] xs
        | otherwise  = groupLines' (currentBlk ++ [x]) xs
    where maxSep = 1
          lastLineNo = grepLineNum $ last currentBlk
          thisLineNo = grepLineNum x






printGrepLineNo :: PrintLineNumberWidth -> Int -> String
printGrepLineNo Nothing _ = ""
printGrepLineNo (Just lineNumberWidth) x = x_out
    where suffix = ": "
          x_str = show x ++ ": "
          padding_needed = lineNumberWidth + length suffix - length x_str + 1
          padding =  concat $ replicate padding_needed " "
          x_out = padding ++ x_str

printGrepLine ::  PrintLineNumberWidth -> GrepLinePrinted  -> IO ()
printGrepLine lineNumberWidth (MatchLine m) = do
    setSGR [SetColor Foreground Vivid Blue]
    putStr $ printGrepLineNo lineNumberWidth lineNo
    setSGR [SetColor Foreground Dull White]
    putStr  pre
    setSGR [SetColor Foreground Vivid Green]
    putStr  matched
    setSGR [SetColor Foreground Dull White]
    putStr $ post ++ "\n"
    setSGR []
    where GrepLineMatch (pre, matched, post, _) lineNo = m



printGrepLine lineNumberWidth (ContextLine lineNo lineContents)  = do
    setSGR [SetColor Foreground Vivid Blue]
    putStr $ printGrepLineNo lineNumberWidth lineNo
    setSGR [SetColor Foreground Dull White]
    putStr $ lineContents
    putStr "\n"
    setSGR []




printGroupLines :: String -> PrintLineNumberWidth -> [GrepLinePrinted] -> IO ()
printGroupLines filename lineNumberWidth fileLines = do
    putStrLn $ "In file:" ++ filename
    mapM_ (printGrepLine lineNumberWidth) fileLines
    return ()
