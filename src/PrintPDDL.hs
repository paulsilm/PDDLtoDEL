module PrintPDDL where

import PDDL

ppInput :: PDDL -> String
ppInput (CheckPDDL domain problem) = ppDomain domain ++ "\n\n" ++ ppProblem problem

ppDomain :: Domain -> String
ppDomain (Domain str reqs types preds actions) = 
     "(define (domain " ++ str ++ ")\n" 
  ++ "\t(:requirements" ++ (concatMap (\r -> " " ++ ppReq r) reqs) ++ ")\n"
  ++ "\t(:types" ++ (concatMap (" " ++) types) ++ ")\n"
  ++ "\t(:predicates" ++ (concatMap (\p -> "\n\t\t" ++ ppPred p) preds) ++ "\n\t)"
  ++ (concatMap (\a -> "\n\n\t" ++ ppAction a) actions) ++ "\n"
  ++ ")"

ppReq :: Req -> String 
ppReq Strips = ":strips"
ppReq Typing = ":typing"

ppPred :: Predicate -> String 
ppPred (PredAtom a) = "(" ++ a ++ ")"
ppPred (PredSpec p vars qm) = "(" ++  p ++ (concatMap ((if qm then " ?" else " ") ++) vars) ++ ")"
ppPred (PredDef p varTypes) = "(" ++  p ++ (concatMap (\v -> " " ++ ppVars v) varTypes) ++ ")"

ppVars :: VarType -> String
ppVars (VTL vars typedObjs) = "?" ++ head vars ++ (concatMap (" ?" ++) $ tail vars) ++ " - " ++ typedObjs

ppAction :: Action -> String
ppAction (Action name params actor events obss) = 
     "(:action " ++ name ++ "\n\t\t"
  ++ ":parameters (" ++ ppVars (head params) ++ (concatMap (\p -> " " ++ ppVars p) $ tail params) ++ ")\n\t\t"
  ++ ":byagent ?" ++ actor
  ++ (concatMap (\e -> "\n\t\t" ++ ppEvent e) events) 
  ++ (concatMap (\o -> "\n\t\t:observability" ++ ppObs o) obss) ++ "\n\t)"

ppEvent :: Event -> String 
ppEvent (Event des "" pres effect) = 
     ":precondition " ++ (ppForm pres "\t\t\t") ++ "\n\t\t"
  ++ ":effect " ++ (ppForm effect "\t\t\t")
ppEvent (Event des name pres effect) = 
     "(:event-" ++ dess ++ "designated " ++ name ++ "\n\t\t\t"
  ++ ":precondition " ++ (ppForm pres "\t\t\t\t") ++ "\n\t\t\t"
  ++ ":effect " ++ (ppForm effect "\t\t\t\t") ++ "\n\t\t)"
    where dess = if des then "" else "non"

ppObs :: Obs -> String
ppObs (ObsDef obsType) = ppObsType obsType
ppObs (ObsSpec obsType ags) = ppObsType obsType ++ (concatMap (" ?" ++) ags)

ppObsType :: ObsType -> String
ppObsType Full = " full"
ppObsType None = " none"
ppObsType (Partition sss) = " (partition" ++ (concatMap (\ss -> " (" ++ (concatMap (" " ++) ss ) ++ ")") sss) ++ ")"

ppForm :: Form -> String -> String
ppForm (Atom a) _ = ppPred a
ppForm (Not f) indent = "(not " ++ (ppForm f $ indent ++ "\t") ++ ")"
ppForm (And fs) indent = "\n" ++ indent ++ "(and" ++ (concatMap (\f -> "\n\t" ++ indent ++ (ppForm f $ indent ++ "\t")) fs) ++ ")"
ppForm (Or fs) indent = "\n" ++ indent ++ "(or" ++ (concatMap (\f -> "\n\t" ++ indent ++ (ppForm f $ indent ++ "\t")) fs) ++ ")"
ppForm (Imply f1 f2) indent = "(imply " ++ (ppForm f1 $ indent ++ "\t") ++ (ppForm f2 $ indent ++ "\t") ++ ")"
ppForm (Knows ag f) indent = "(knows ?" ++ ag ++ " " ++ (ppForm f $ indent ++ "\t") ++ ")"
ppForm (Forall vt f) indent = 
  "(forall (" ++ (ppVars vt) ++ 
  ")\n" ++ indent ++ "\t" ++ (ppForm f $ indent ++ "\t") ++ 
  "\n" ++ indent ++ ")"
ppForm (ForallWhen vt fw ft) indent = 
  "(forall (" ++ (ppVars vt) ++ 
  ")\n" ++ indent ++ "\twhen " ++ (ppForm fw $ indent ++ "\t") ++ 
  "\n" ++ indent ++ "\t\t" ++ (ppForm ft $ indent ++ "\t\t") ++ 
  "\n" ++ indent ++ ")"
ppForm (Exists vt f) indent = 
  "(exists " ++ (ppVars vt) ++ 
  "\n" ++ indent ++ "\t" ++ (ppForm f $ indent ++ "\t") ++ 
  "\n" ++ indent ++ ")"

--Problem String String [TypedObjs] [[String]] [World] [Obs] Form
ppProblem :: Problem -> String 
ppProblem (Problem problemName domainName objects init worlds obss goal) =
     "(define (problem " ++ problemName ++ ")\n\t"
  ++ "(:domain " ++ domainName ++ ")\n\t"
  ++ "(:objects" ++ (concatMap (\o -> "\n\t\t" ++ ppObj o) objects) ++ ")\n\t"
  ++ "(:init" ++ (concatMap (\p -> "\n\t\t" ++ ppPred p) init) ++ ")\n\t"
  ++ "(:worlds\n" ++ (concatMap ppWorld worlds) ++ "\t)\n\n\t"
  ++ "(:observability" ++ ppObs (head obss) ++ (concatMap (\o -> "\n\t :observability" ++ ppObs o) $ tail obss) ++ ")\n\t"
  ++ "(:goal\n\t\t" ++ (ppForm goal "\t\t") ++ "\n\t)\n)\n"

ppObj :: TypedObjs -> String 
ppObj (TO objs typedObjs) = (concatMap (" " ++) objs) ++ " - " ++ typedObjs

ppWorld :: World -> String
ppWorld (World des name truePreds) = 
  "\t\t(:world-" ++ dess ++ "designated " ++ name ++ (concatMap (\p -> "\n\t\t\t" ++ ppPred p) truePreds) ++ "\n\t\t)\n"
    where dess = if des then "" else "non"

ppStringListList :: [[String]] -> String
ppStringListList sss = (concatMap (\ss -> "\n\t\t(" ++ (concatMap (" " ++) ss) ++ ")") sss)