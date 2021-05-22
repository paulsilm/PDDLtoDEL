module Main where

import Parse
import Lex
import PrintPDDL
import Translate
import PDDL
import System.Exit (exitFailure)
import SemanticChecker
import SMCDEL.Internal.TexDisplay
import Plan
import System.Environment (getArgs,getProgName)
import System.IO (hPutStrLn, stderr)
--import Data.Text

main :: IO ()
main = do
  input <- getInput 
  case input of
    Right (domFile,prbFile,depth,icSolves) -> do
      domain <- readFile domFile
      problem <- readFile prbFile
      let splitIndex = length $ lines domain
      let contents = domain ++ "\n" ++ problem
      case parse $ alexScanTokens contents of -- $ map toLower contents of
          Left (lin,col) -> do
            let fileT = if lin < splitIndex then "domain" else "problem"
            let lineNo = if lin < splitIndex then lin else lin - splitIndex
            error ("Parse error in the " ++ fileT ++ " file in line " ++ show lineNo ++ ", column " ++ show col)
          Right pddl -> do
            processPDDL pddl depth icSolves
    Left (filename,depth,icSolves) -> do
      contents <- readFile filename
      case parse $ alexScanTokens $ contents of --map toLower contents of
          Left (lin,col) -> error ("Parse error in line " ++ show lin ++ ", column " ++ show col)
          Right pddl -> do
            processPDDL pddl depth icSolves
        {- let (actionModelMap,problem) = pddlToDEL pddl
        disp $ (map snd) actionModelMap
        putStrLn $ tex problem
        pdfTo problem "problem.pdf"
        putStrLn $ problem
        putStrLn $ (map snd) actionModelMap
        print domain
        print problem -}

--A function just for stuff
processPDDL :: PDDL -> Int -> Bool -> IO ()
processPDDL pddl depth icSolves = 
  case validInput pddl of
    Just str -> do
      putStrLn $ str ++ show pddl -- print error messages
    Nothing -> do
      putStrLn "Succesful parsing"
      --putStrLn $ ppInput pddl
      --putStrLn $ show $ pddlToDEL pddl
      --writeFile fileName $ ppInput pddl --Useful for formatting the file, but loses comments
      putStrLn $ findShortestICPlan pddl icSolves depth

--Returns either (filecontents, depth, icSolves) or (domain, problem, depth, icSolves)
getInput :: IO (Either (String, Int, Bool) (String, String, Int, Bool))
getInput = do
  args <- getArgs
  let input = inputFromArgs (Left ("", 0, False)) args
  case input of
    Just contents -> do
      return contents
    Nothing -> do
      name <- getProgName
      mapM_ (hPutStrLn stderr)
        [ infoline
        , "usage: " ++ name ++ " <filename> | <options>\n"
        --, "  (use filename - for STDIN)\n"
        , "  -dom <filename>   parse the domain file\n"
        , "  -prb <filename>   parse the problem file\n" 
        , "  -d   <int>        the maximum depth of solution\n" 
        , "  -ic               constrain actions to those where the agent knows it will lead to the goal\n" 
        ]
      exitFailure

--TODO? add 
{- TODO add:
    -verbose option for more detailed error messages (for semantic checker, possibly also for parser)
    -print option with a filename to print out a parsed version of the file
    '-' for stdin 
    -tex to generate a tex file with initial state diagram (non-essential)
    -pdf to generate a pdf file with initial state diagram (non-essential)
-}
--TODO clean it up lol
--Returns filenames and stuff
inputFromArgs :: Either (String, Int, Bool) (String, String, Int, Bool) -> [String] -> Maybe (Either (String, Int, Bool) (String, String, Int, Bool))
inputFromArgs i@(Left (fn, _, _)) [] 
  | fn /= "" = Just i
  | otherwise = Nothing
inputFromArgs i@(Right (dom, prb, _, _)) [] 
  | dom /= "" && prb /= "" = Just i
  | otherwise = Nothing
inputFromArgs (Left (fn, _, b)) ("-d":i:args) = 
  inputFromArgs (Left (fn, read i :: Int, b)) args
inputFromArgs (Right (df, pf, _, b)) ("-d":i:args) = 
  inputFromArgs (Right (df, pf, read i :: Int, b)) args
inputFromArgs (Left (fn, d, b)) ("-dom":dom:args) 
  | fn /= "" = Nothing 
  | otherwise = inputFromArgs (Right (dom, "", d, b)) args
inputFromArgs (Right (fn, prb, d, b)) ("-dom":dom:args) 
  | fn /= "" = Nothing
  | otherwise = inputFromArgs (Right (dom, prb, d, b)) args
inputFromArgs (Left (fn, d, b)) ("-prb":prb:args)
  | fn /= "" = Nothing 
  | otherwise = inputFromArgs (Right ("", prb, d, b)) args
inputFromArgs (Right (dom, fn, d, b)) ("-prb":prb:args) 
  | fn /= "" = Nothing
  | otherwise = inputFromArgs (Right (dom, prb, d, b)) args
inputFromArgs (Left (fn, d, _)) ("-ic":args) = inputFromArgs (Left (fn, d, True)) args
inputFromArgs (Right (dom, prb, d, _)) ("-ic":args) = inputFromArgs (Right (dom, prb, d, True)) args
inputFromArgs (Left (fo, d, b)) (fn:args) 
  | fo /= "" = Nothing
  | otherwise = inputFromArgs (Left (fn, d, b)) args
inputFromArgs (Right _) (_:_) = Nothing
--inputFromArgs _ _ = Nothing

infoline :: String
infoline = "PDDLtoDEL " ++ "1.0.0" ++ " -- https://github.com/paulsilm/PDDLtoDEL\n"
