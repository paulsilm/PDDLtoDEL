module PrintPDDL where

import PDDL

ppInput :: CheckInput -> String
ppInput (CheckInput domain problem) = (ppDomain domain) ++ "\n\n" ++ (ppProblem problem)

ppDomain :: CheckDomain -> String
ppDomain (CheckDomain str reqs strs preds actions) = 
     "(define (domain " ++ str ++ ")\n" 
  ++ "\t(:requirements" ++ (concat $ map (\r -> " " ++ ppReq r) reqs) ++ ")\n"
  ++ "\t(:types" ++ (concat $ map (" " ++) strs) ++ ")\n"
  ++ "\t(:predicates" ++ (concat $ map (\p -> "\n\t\t" ++ ppPred p) preds) ++ "\n\t)"
  ++ (concat $ map (\a -> "\n\n\t" ++ ppAction a) actions) ++ "\n"
  ++ ")"

ppReq :: Req -> String 
ppReq Strips = ":strips"
ppReq Typing = ":typing"

ppPred :: Predicate -> String 
ppPred (PredAtom a) = "(" ++ a ++ ")"
ppPred (PredSpec p vars) = "(" ++  p ++ (concat $ map (" ?" ++) vars) ++ ")"
ppPred (PredDef p varTypes) = "(" ++  p ++ (concat $ map (\v -> " " ++ ppVars v) varTypes) ++ ")"

ppVars :: VarType -> String
ppVars (VTL vars objType) = (concat $ map (" ?" ++) vars) ++ " - " ++ objType

ppAction :: Action -> String
ppAction (Action name params actor events obss) = 
     "(:action " ++ name ++ "\n\t\t"
  ++ ":parameters (" ++ (concat $ map (\v -> "" ++ ppVars v) params) ++ ")\n\t\t"
  ++ ":byagent ?" ++ actor ++ "\n\t\t"
  ++ (concat $ map (\e -> "\n\t\t" ++ ppEvent e) events) ++ "\n\t\t"
  ++ (concat $ map (\o -> "\n\t\t:observability" ++ ppObs o) obss) ++ "\n\t\t)"


ppEvents :: [Event] -> String 
--ppEvents (e:[]) = (concat $ map (\e -> "\n\t\t" ++ ppEvent e) events)
ppEvents events = (concat $ map (\e -> "\n\t\t" ++ ppEvent e) events)

ppEvent :: Event -> String 
ppEvent (Event des "" pres effect) = 
     ":precondition " ++ (ppForm pres) ++ "\n\t\t\t"
  ++ ":effect " ++ (ppForm effect) ++ "\n\t\t\t"
ppEvent (Event des name pres effect) = 
     "(:event-" ++ dess ++ "designated " ++ name ++ "\n\t\t\t"
  ++ ":precondition " ++ (ppForm pres) ++ "\n\t\t\t"
  ++ ":effect " ++ (ppForm effect) ++ "\n\t\t\t)"
    where dess = if des then "" else "non"

ppObs :: Obs -> String
ppObs (ObsDef obsType) = ppObsType obsType
ppObs (ObsSpec obsType ags) = ppObsType obsType ++ (concat $ map (\a -> " ?" ++ a) ags)

ppObsType :: ObsType -> String
ppObsType Full = " full"
ppObsType None = " none"
ppObsType (Partition sss) = " (partition" ++ (concat $ map (\ss -> " (" ++ (concat $ map (\s -> " " ++ s) ss ) ++ ")") sss) ++ ")"

ppForm :: Form -> String
ppForm (Atom a) = ppPred a
ppForm (Not f) = "(not " ++ (ppForm f) ++ ")"
ppForm (And fs) = "(and " ++ (concat $ map (\f -> "\n\t\t" ++ ppForm f) fs) ++ ")"
ppForm (Or fs) = "(or " ++ (concat $ map (\f -> "\n\t\t" ++ ppForm f) fs) ++ ")"
ppForm (Imply f1 f2) = "(imply " ++ "\n\t\t\t" ++ (ppForm f1) ++ "\n\t\t\t" ++ (ppForm f2) ++ "\n\t\t)"
ppForm (Knows ag f) = "(knows ?" ++ ag ++ " " ++ (ppForm f) ++ ")"
ppForm (Forall vt f) = "(forall \n\t\t(" ++ (ppVars vt) ++ ")\n\t\t\t" ++ (ppForm f) ++ "\n\t\t)"
ppForm (ForallWhen vt fw ft) = "(forall " ++ (ppVars vt) ++ "\n\t\t\twhen " ++ (ppForm fw) ++ "\n\t\t\t\t" ++ (ppForm ft) ++ "\n\t\t)"
ppForm (Exists vt f) = "(exists " ++ (ppVars vt) ++ "\n\t\t\t" ++ (ppForm f) ++ "\n\t\t)"


--CheckProblem String String [ObjType] [[String]] [World] [Obs] Form
ppProblem :: CheckProblem -> String 
ppProblem (CheckProblem problemName domainName objects init worlds obss goal) =
     "(define (problem " ++ problemName ++ ")\n\t"
  ++ "(:domain " ++ domainName ++ ")\n\t"
  ++ "(:objects" ++ (concat $ map (\o -> "\n\t\t" ++ ppObj o) objects) ++ ")\n\t"
  ++ "(:init" ++ (ppStringListList init) ++ ")\n\t"
  ++ "(:worlds " ++ (concat $ map (\w -> "\n\t\t" ++ ppWorld w) worlds) ++ ")\n\t"
  ++ "( " ++ (concat $ map (\o -> "\n\t:observability" ++ ppObs o) obss) ++ ")\n\t"
  ++ "(:goal\n\t" ++ (ppForm goal) ++ "\n\t)\n)\n"

ppObj :: ObjType -> String 
ppObj (OTL objs objType) = (concat $ map (" " ++) objs) ++ " - " ++ objType

ppWorld :: World -> String
ppWorld (World des name objects) = 
  "(:world-" ++ dess ++ "designated " ++ name ++ (ppStringListList objects) ++ "\n\t\t)\n"
    where dess = if des then "" else "non"

ppStringListList :: [[String]] -> String
ppStringListList sss = (concat $ map (\ss -> "\n\t\t(" ++ (concat $ map (\s -> " " ++ s) ss) ++ ")") sss)