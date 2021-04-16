module SemanticChecker where

import PDDL 
import Translate

validInput :: PDDL -> Bool
validInput (CheckPDDL (Domain name reqs types preds actions) (Problem _ domName objects initPreds worlds obss goal)) =
     and [objType `elem` types | (TO _ objType) <- objects]
  && all (predTypesExist types) preds
  && all requirementSupported reqs
  && name == domName 
  && allDifferent [name | (World _ name _) <- worlds]
  && allDifferent [name | (Action name _ _ _ _) <- actions]
  && formInCorrectFormat goal
  && all (\p -> p `elem` concatMap (predToProps objects) preds) (initPreds ++ concat [preds | (World _ _ preds) <- worlds]) --check all predicates are legit
  && [des | (World des _ _) <- worlds, des] == [True] --Just one designated world

validAction :: [String] -> Action -> Bool
validAction typeList (Action name params actor events obss) =
     and [formInCorrectFormat pre | (Event _ _ pre _) <- events]
  && and [effInCorrectFormat eff | (Event _ _ _ eff) <- events]
  && allDifferent [name | (Event _ name _ _) <- events]
  -- && observabilitiesOnlyForAgents (getObjNames "agent" typeList) obss --TODO add objectlist to validAction params
  && or [des | (Event des _ _ _) <- events, des] --at least one designated event
  && and [paramType `elem` typeList | (VTL _ paramType) <- params]


--converts predicate to its type --TODO requires parameters of action to convert PredSpecc v
--predConvert :: Predicate -> Predicate
--predConvert (PredDef name vars) = 
--  PredSpec name (map snd $ concatMap typify vars) False
--predConvert (PredSpec name vars _) = predConvert $ PredDef name [VTL varNames | ]
--predConvert (PredAtom name) = predConvert $ PredDef name []

observabilitiesOnlyForAgents :: [String] -> [Obs] -> Bool
observabilitiesOnlyForAgents _ [] = True
observabilitiesOnlyForAgents ags ((ObsDef _):obss) = observabilitiesOnlyForAgents ags obss
observabilitiesOnlyForAgents ags ((ObsSpec _ ags2):obss) = 
     all (\ag -> ag `elem` ags) ags2
  && observabilitiesOnlyForAgents ags obss

observabilityPartitionCorrect :: [String] -> Obs -> Bool
observabilityPartitionCorrect legitNames (ObsDef (Partition part)) = and $ concatMap (map (\s -> s `elem` legitNames)) part 
observabilityPartitionCorrect legitNames (ObsSpec (Partition part) _) = and $ concatMap (map (\s -> s `elem` legitNames)) part 
observabilityPartitionCorrect _ _ = True

predTypesExist :: [String] -> Predicate -> Bool
predTypesExist types (PredDef _ vars) = and [varType `elem` types | (VTL _ varType) <- vars]
predTypesExist _ _ = True

allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs) = notElem x xs && allDifferent xs

--TODO need to rewrite it with checking types and names of predicates to see if they match the given ones.
formInCorrectFormat :: Form -> Bool
formInCorrectFormat (And preds) = all isPred preds
formInCorrectFormat (Or preds) = all isPred preds
formInCorrectFormat (Not f) = formInCorrectFormat f
formInCorrectFormat (Imply p1 p2) = all isPred [p1,p2]
formInCorrectFormat (Forall _ f) = formInCorrectFormat f
formInCorrectFormat (ForallWhen _ _ f) = formInCorrectFormat f
formInCorrectFormat (Exists _ f) = formInCorrectFormat f
formInCorrectFormat (Knows _ f) = formInCorrectFormat f
formInCorrectFormat f = isPred f

--TODO same goes for this one
effInCorrectFormat :: Form -> Bool
effInCorrectFormat (And preds) = all isPred preds
effInCorrectFormat (Atom pred) = True
effInCorrectFormat _ = False

isPred :: Form -> Bool
isPred (Atom _) = True
isPred _ = False

requirementSupported :: Req -> Bool --TODO add more reqs to parser and language
requirementSupported Strips = True
requirementSupported Typing = True
