module Translate where

import PDDL
import SMCDEL.Language

PDDLtoDEL :: PDDL -> CheckInput
PDDLtoDEL (CheckPDDL domain problem) =
  CheckInput (vocabInts domain) ...

