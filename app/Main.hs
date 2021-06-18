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
import SMCDEL.Explicit.S5 
import Debug.Trace

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
      contents <- if filename == "-" then getContents else readFile filename
      case parse $ alexScanTokens $ map toLower contents of
          Left (lin,col) -> error ("Parse error in line " ++ show lin ++ ", column " ++ show col)
          Right pddl -> do
            processPDDL args pddl

--TODO clean up
--A function for processing the pddl file according to the requests of the user
--Arguments (-pdf, -tex, -print, --debug, --nosemantics, -d, -ic)
processPDDL :: Arguments -> PDDL -> IO ()
processPDDL (pdf, texM, printFile, debug, False, depth, ic) pddl = 
  case validInput pddl of
    Just str -> do
      printParsedPDDL printFile pddl --print original file
      putStrLn $ str ++ (if debug then show pddl else "") -- print error messages
      case pddlToDEL pddl of
        (CoopTask problem actions _) -> do
          printModel pdf texM problem actions --print the tex file of the model, if desired
    Nothing -> do
      putStr $ if debug then "Successful parsing\n" else []
      putStrLn $ if debug then show pddl else ""
      case pddlToDEL pddl of
        (CoopTask problem actions _) -> do
          putStrLn $ if debug then show problem else ""
          printModel pdf texM problem actions
      printParsedPDDL printFile pddl
      putStrLn $ findShortestICPlan pddl ic depth debug
processPDDL (pdf, texM, printFile, debug, True, depth, ic) pddl = 
  do
    case pddlToDEL pddl of
      (CoopTask problem actions _) -> do
        putStrLn $ if debug then show pddl else ""
        putStrLn $ if debug then show problem else ""
        printModel pdf texM problem actions
    printParsedPDDL printFile pddl
    putStrLn $ findShortestICPlan pddl ic depth debug

--Pretty prints the parsed PDDL input file for comparison to the original
printParsedPDDL :: String -> PDDL -> IO ()
printParsedPDDL "" _ = pure ()
printParsedPDDL "-" pddl = putStrLn $ ppInput pddl
printParsedPDDL fname pddl = writeFile fname $ ppInput pddl

--TODO rename predicates in each action using the atomMap, reformat also to include actionlabels
--Prints the tex file of the model
printModel :: String -> String -> MultipointedModelS5 -> [Owned MultipointedActionModelS5] -> IO ()
printModel _ "-" problem acts = putStrLn $ (tex problem) ++ (unlines $ map (tex.snd.snd) acts) --(show $ length acts)
printModel _ "" _ _ = pure ()
printModel _ fname problem acts = writeFile fname $ (tex problem) ++ (unlines $ map (tex.snd.snd) acts)
