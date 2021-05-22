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
    Right (nosemantics, debug, domFile,prbFile,depth,icSolves) -> do
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
            processPDDL pddl depth icSolves debug nosemantics
    Left (nosemantics, debug, filename,depth,icSolves) -> do
      contents <- readFile filename
      case parse $ alexScanTokens $ contents of --map toLower contents of
          Left (lin,col) -> error ("Parse error in line " ++ show lin ++ ", column " ++ show col)
          Right pddl -> do
            processPDDL pddl depth icSolves debug nosemantics
        {- let (actionModelMap,problem) = pddlToDEL pddl
        disp $ (map snd) actionModelMap
        putStrLn $ tex problem
        pdfTo problem "problem.pdf"
        putStrLn $ problem
        putStrLn $ (map snd) actionModelMap
        print domain
        print problem -}

--A function just for stuff
processPDDL :: PDDL -> Int -> Bool -> Bool -> Bool -> IO ()
processPDDL pddl depth icSolves debug False = 
  case validInput pddl of
    Just str -> do
      putStrLn $ str ++ show pddl -- print error messages
    Nothing -> do
      putStrLn "Succesful parsing"
      putStrLn $ findShortestICPlan pddl icSolves depth debug
processPDDL pddl depth icSolves debug True = 
  putStrLn $ findShortestICPlan pddl icSolves depth debug
--IF PRINT ENABLED SOME DAY
--putStrLn $ ppInput pddl
--putStrLn $ show $ pddlToDEL pddl
--writeFile fileName $ ppInput pddl --Useful for formatting the file, but loses comments


--Returns 
--either (nosemanticchecking, debug, filename, depth, icSolves) 
--or (nosemanticchecking, debug, domainfilename, problemfilename, depth, icSolves)
getInput :: IO (Either (Bool, Bool, String, Int, Bool) (Bool, Bool, String, String, Int, Bool))
getInput = do
  args <- getArgs
  let input = inputFromArgs (Left (False, False, "", 0, False)) args
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
        , "  --debug           print the states being searched through\n" 
        , "  --nosemantics     ignore the semantic checker (in case it's buggy)\n" 
        ]
      exitFailure

{- TODO add:
    -print option with a filename to print out a parsed version of the file
    '-' for stdin 
    -tex to generate a tex file with initial state diagram (non-essential)
    -pdf to generate a pdf file with initial state diagram (non-essential)
-}
--TODO clean it up lol (if cabal only worked properly)
--Returns command line arguments parsed
inputFromArgs :: Either (Bool, Bool, String, Int, Bool) (Bool, Bool, String, String, Int, Bool) -> [String] -> Maybe (Either (Bool, Bool, String, Int, Bool) (Bool, Bool, String, String, Int, Bool))
--read nothing
inputFromArgs i@(Left (_, _, fn, _, _)) [] 
  | fn /= "" = Just i
  | otherwise = Nothing
inputFromArgs i@(Right (_, _, dom, prb, _, _)) [] 
  | dom /= "" && prb /= "" = Just i
  | otherwise = Nothing
--Read maximum depth
inputFromArgs (Left (nos, db, fn, _, b)) ("-d":i:args) = 
  inputFromArgs (Left (nos, db, fn, read i :: Int, b)) args
inputFromArgs (Right (nos, db, df, pf, _, b)) ("-d":i:args) = 
  inputFromArgs (Right (nos, db, df, pf, read i :: Int, b)) args
--read domain filename
inputFromArgs (Left (nos, db, fn, d, b)) ("-dom":dom:args) 
  | fn /= "" = Nothing 
  | otherwise = inputFromArgs (Right (nos, db, dom, "", d, b)) args
inputFromArgs (Right (nos, db, fn, prb, d, b)) ("-dom":dom:args) 
  | fn /= "" = Nothing
  | otherwise = inputFromArgs (Right (nos, db, dom, prb, d, b)) args
--read problem filename
inputFromArgs (Left (nos, db, fn, d, b)) ("-prb":prb:args)
  | fn /= "" = Nothing 
  | otherwise = inputFromArgs (Right (nos, db, "", prb, d, b)) args
inputFromArgs (Right (nos, db, dom, fn, d, b)) ("-prb":prb:args) 
  | fn /= "" = Nothing
  | otherwise = inputFromArgs (Right (nos, db, dom, prb, d, b)) args
--read implicit coordination flag
inputFromArgs (Left (nos, db, fn, d, _)) ("-ic":args) = inputFromArgs (Left (nos, db, fn, d, True)) args
inputFromArgs (Right (nos, db, dom, prb, d, _)) ("-ic":args) = inputFromArgs (Right (nos, db, dom, prb, d, True)) args
--read ignore semantic checker flag
inputFromArgs (Left (_, db, fn, d, ic)) ("--nosemantics":args) = inputFromArgs (Left (True, db, fn, d, ic)) args
inputFromArgs (Right (_, db, dom, prb, d, ic)) ("--nosemantics":args) = inputFromArgs (Right (True, db, dom, prb, d, ic)) args
--read debug flag
inputFromArgs (Left (nos, _, fn, d, ic)) ("--debug":args) = inputFromArgs (Left (nos, True, fn, d, ic)) args
inputFromArgs (Right (nos, _, dom, prb, d, ic)) ("--debug":args) = inputFromArgs (Right (nos, True, dom, prb, d, ic)) args
--Read regular filename
inputFromArgs (Left (nos, db, fo, d, b)) (fn:args) 
  | fo /= "" = Nothing
  | otherwise = inputFromArgs (Left (nos, db, fn, d, b)) args
inputFromArgs (Right _) (_:_) = Nothing
--inputFromArgs _ _ = Nothing

infoline :: String
infoline = "PDDLtoDEL " ++ "1.0.0" ++ " -- https://github.com/paulsilm/PDDLtoDEL\n"
