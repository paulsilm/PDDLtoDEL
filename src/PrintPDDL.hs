module PrintPDDL where

import PDDL

--String [Req] [String] [Predicate] [Action]
ppDomain :: CheckDomain -> String
ppDomain (CheckDomain str reqs strs preds actions) = 
     "(define (domain " ++ str ++ ")\n" 
  ++ "\t(:requirements" ++ (concat $ map (\r -> " " ++ ppReq r) reqs) ++ ")\n"
  ++ "\t(:types" ++ (concat $ map (" " ++) strs) ++ ")\n"
  ++ "\t(predicates" ++ (concat $ map (\p -> "\n\t\t" ++ ppPred p) preds) ++ "\n\t)"
  ++ (concat $ map (\a -> "\n\n\t" ++ ppAction a) actions) ++ "\n"
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

ppAction :: Action -> String
ppAction (Action name params actor events obss) = 
     "(:action " ++ name ++ "\n\t\t"
  ++ ":parameters (" ++ (concat $ map (\v -> " " ++ ppVars v) params) ++ ")\n\t\t"
  ++ ":byagent " ++ actor ++ "\n\t\t"
  ++ (concat $ map (\e -> "\n\t\t" ++ ppEvent e) events) ++ "\n\t\t"
  ++ (concat $ map (\f -> "\n\t\t" ++ ppObs f) obss) ++ "\n\t\t)"


ppEvents :: [Event] -> String 
--ppEvents (e:[]) = (concat $ map (\e -> "\n\t\t" ++ ppEvent e) events)
ppEvents events = (concat $ map (\e -> "\n\t\t" ++ ppEvent e) events)

ppEvent :: Event -> String 
ppEvent (Event des name pres effect) = 
     "(:event-" ++ dess ++ "designated " ++ name ++ "\n\t\t\t"
  ++ (ppForm pres) ++ "\n\t\t\t"
  ++ (ppForm effect) ++ "\n\t\t\t"
    where dess = if des then "" else "non"

ppObs :: Obs -> String
ppObs (ObsDef obsType) = ppObsType obsType
ppObs (ObsSpec obsType ags) = ppObsType obsType ++ (concat $ map (\a -> " ?" ++ a) ags)

ppObsType :: ObsType -> String
ppObsType Full = " full"
ppObsType None = " none"
ppObsType (Partition sss) = "(partition " ++ (concat $ map (\ss -> " (" ++ (concat $ map (\s -> "(" ++ s ++ ")") ss )) sss) ++ ")"

ppForm :: Form -> String
ppForm _ = "Stub"