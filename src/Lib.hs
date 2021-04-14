module Lib where

import SMCDEL.Language
import PDDL 
import PrintPDDL (ppPred)
import SMCDEL.Internal.Help ((!))
import Data.Tuple (swap)

translationForPP :: [(Predicate, Prp)] -> (Prp -> String)
translationForPP atomMap = \p -> ppPred $ map swap atomMap ! p
