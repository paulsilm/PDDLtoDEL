module Main where

import Input
import Parse
import Lex
import PrintPDDL
import Translate
import PDDL
import SemanticChecker
import SMCDEL.Internal.TexDisplay
import SMCDEL.Other.Planning
import Plan
import Data.Char

main :: IO ()
main = do
  (args,inputFiles) <- getInput 
  case inputFiles of
    Right (domFile,prbFile) -> do
      domain <- readFile domFile
      problem <- readFile prbFile
      let splitIndex = length $ lines domain
      let contents = domain ++ "\n" ++ problem
      case parse $ alexScanTokens $ map toLower contents of
          Left (lin,col) -> do
            let fileT = if lin < splitIndex then "domain" else "problem"
            let lineNo = if lin < splitIndex then lin else lin - splitIndex - 1
            error ("Parse error in the " ++ fileT ++ " file in line " ++ show lineNo ++ ", column " ++ show col)
          Right pddl -> do
            processPDDL args pddl
    Left filename -> do
      contents <- readFile filename
      case parse $ alexScanTokens $ map toLower contents of
          Left (lin,col) -> error ("Parse error in line " ++ show lin ++ ", column " ++ show col)
          Right pddl -> do
            processPDDL args pddl
        {- let (actionModelMap,problem) = pddlToDEL pddl
        disp $ (map snd) actionModelMap
        pdfTo problem "problem.pdf"
        putStrLn $ problem
        putStrLn $ (map snd) actionModelMap
        print domain
        print problem -}

--A function just for stuff
--Arguments (-print, --debug, --nosemantics, -d, -ic)
processPDDL :: Arguments -> PDDL -> IO ()
processPDDL (printFile, debug, False, depth, ic) pddl = 
  case validInput pddl of
    Just str -> do
      printParsedPDDL printFile pddl
      putStrLn $ str ++ show pddl -- print error messages
    Nothing -> do
      putStrLn "Successful parsing"
      printParsedPDDL printFile pddl
      putStrLn $ findShortestICPlan pddl ic depth debug
processPDDL (printFile, debug, True, depth, ic) pddl = 
  do
    case pddlToDEL pddl of
      (CoopTask problem _ _) -> 
        putStrLn $ tex problem
    printParsedPDDL printFile pddl
    putStrLn $ findShortestICPlan pddl ic depth debug

--Pretty prints the parsed PDDL input file for comparison to the original
printParsedPDDL :: String -> PDDL -> IO ()
printParsedPDDL "" _ = pure ()
printParsedPDDL "-" pddl = putStrLn $ ppInput pddl
printParsedPDDL fname pddl = writeFile fname $ ppInput pddl
