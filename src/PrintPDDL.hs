module PrintPDDL where

import PDDL

--String [Req] [String] [Predicate] [Action]
ppDomain :: CheckDomain -> String
ppDomain (CheckDomain str reqs strs preds actions) = 
     "(define (domain " ++ str ++ ")\n" 
  ++ "\t(:requirements" ++ (foldr (\r s -> s ++ " " ++ (ppReq r)) "" reqs) ++ ")\n"
  -- ++ "\t(:types" ++ (foldr (\s -> " " ++ s) "" strs) ++ ")\n"
  -- ++ "\t(predicates" ++ (ppPreds preds) ++ ")\n"
  -- ++ foldr ppAction "\n" actions 
  ++ ")"

ppReq :: Req -> String 
ppReq Strips = ":strips"
ppReq Typing = ":typing"