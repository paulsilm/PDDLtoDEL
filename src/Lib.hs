module Lib where

import SMCDEL.Language
import PDDL 
import PrintPDDL (ppPred)
import SMCDEL.Internal.Help ((!))
import Data.Tuple (swap)

translationForPP :: [(Predicate, Prp)] -> (Prp -> String)
translationForPP atomMap = \p -> ppPred $ map swap atomMap ! p

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)
