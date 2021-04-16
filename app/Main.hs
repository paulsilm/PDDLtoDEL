module Main where

import Parse
import Lex
import PrintPDDL
import Translate
import PDDL
import SemanticChecker
import SMCDEL.Internal.TexDisplay


main :: IO ()
main = do
  input <- readFile "example.pddl"
  case parse $ alexScanTokens input of
      Left (lin,col) -> error ("Parse error in line " ++ show lin ++ ", column " ++ show col)
      Right pddl -> do
        putStrLn $ show $ validInput pddl
        --let (actionModelMap,problem) = pddlToDEL pddl
        --disp $ (map snd) actionModelMap
        --putStrLn $ tex problem
        --pdfTo problem "problem.pdf"
        --putStrLn $ problem
        --putStrLn $ (map snd) actionModelMap
        --print domain
        --print problem
        {-
        let mykns = KnS (map P vocabInts) (boolBddOf lawform) (map (second (map P)) obs)
        when texMode $
          hPutStrLn outHandle $ unlines
            [ "\\section{Given Knowledge Structure}", "\\[ (\\mathcal{F},s) = (" ++ tex ((mykns,[])::KnowScene) ++ ") \\]", "\n\n\\section{Results}" ]
        mapM_ (doJob outHandle texMode mykns) jobs
        when texMode $ hPutStrLn outHandle texEnd
        when showMode $ do
          hClose outHandle
          let command = "cd /tmp && pdflatex -interaction=nonstopmode " ++ takeBaseName texFilePath ++ ".tex > " ++ takeBaseName texFilePath ++ ".pdflatex.log && xdg-open "++ takeBaseName texFilePath ++ ".pdf"
          putStrLn $ "Now running: " ++ command
          _ <- system command
          return ()
        putStrLn "\nDoei!"
        -}
