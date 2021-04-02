module PrintPDDL where

import PDDL

--String [Req] [String] [Predicate] [Action]
ppDomain :: CheckDomain -> String
ppDomain (CheckDomain str reqs strs preds actions) = 
     "(define (domain " ++ str ++ ")\n" 
  ++ "\t(:requirements" ++ (concat $ map (\r -> " " ++ ppReq r) reqs) ++ ")\n"
  ++ "\t(:types" ++ (concat $ map (" " ++) strs) ++ ")\n"
  ++ "\t(predicates" ++ (concat $ map (\p -> "\n\t\t" ++ ppPred p) preds) ++ "\n\t)\n"
  -- ++ foldr ppAction "\n" actions 
  ++ ")"

ppReq :: Req -> String 
ppReq Strips = ":strips"
ppReq Typing = ":typing"

ppPred :: Predicate -> String 
ppPred (PredAtom a) = "(" ++ a ++ ")"
ppPred (PredSpec p vars) = "(" ++  p ++ (concat $ map (" " ++) vars) ++ ")"
ppPred (PredDef p varTypes) = "(" ++  p ++ (concat $ map (\v -> " " ++ ppVars v) varTypes) ++ ")"

ppVars :: VarType -> String
ppVars (VTL vars objType) = (concat $ map (" ?" ++) vars) ++ " - " ++ objType
