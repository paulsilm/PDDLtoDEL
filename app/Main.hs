module Main where

import Parse
import Lex
import PrintPDDL
import Translate
import PDDL
import System.Exit (exitFailure)
import SemanticChecker
import SMCDEL.Internal.TexDisplay
import SMCDEL.Other.Planning
import System.Environment (getArgs,getProgName)
import System.IO (hPutStrLn, stderr)


main :: IO ()
main = do
  (fileName,_) <- getFilenameAndSettings
  input <- readFile fileName
  case parse $ alexScanTokens input of
      Left (lin,col) -> error ("Parse error in line " ++ show lin ++ ", column " ++ show col)
      Right pddl -> do
        case validInput pddl of
          Nothing -> do
            putStrLn "Succesful parsing"
            --putStrLn $ show $ pddlToDEL pddl
            --writeFile fileName $ ppInput pddl --Useful for formatting the file, but loses comments
            putStrLn $ findShortestICPlan $ pddl
          Just str -> putStrLn $ str ++ show pddl

        {- let (actionModelMap,problem) = pddlToDEL pddl
        disp $ (map snd) actionModelMap
        putStrLn $ tex problem
        pdfTo problem "problem.pdf"
        putStrLn $ problem
        putStrLn $ (map snd) actionModelMap
        print domain
        print problem -}

findShortestICPlan :: PDDL -> String
findShortestICPlan pddl = 
  head [concatMap ppICPlan plan 
        | plan <- [findSequentialIcPlan i $ pddlToDEL pddl | i <- [1..]] -- plan :: [[ICPlan Action]]
        , plan /= []] -- :: [[String]]
    

getFilenameAndSettings :: IO (String,[String])
getFilenameAndSettings = do
  args <- getArgs
  case args of
    ("-":options) -> do
      let filename = "examples/key.pddl"
      return (filename,options)
    (filename:options) -> do
      return (filename,options)
    _ -> do
      name <- getProgName
      mapM_ (hPutStrLn stderr)
        [ infoline
        , "usage: " ++ name ++ " <filename> {options}"
        , "       (use filename - for STDIN)\n"
        --, "  -tex   generate LaTeX code\n"
        --, "  -show  write to /tmp, generate PDF and show it (implies -tex)\n" 
        ]
      exitFailure

infoline :: String
infoline = "PDDLtoDEL " ++ "1.0.0" ++ " -- https://github.com/paulsilm/PDDLtoDEL\n"
