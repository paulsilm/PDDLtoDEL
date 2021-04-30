module SemanticChecker where

import PDDL
import Translate
import Lib

--Checks whether the input is semantically consistent, if not returns a Just String with
--an error message
validInput :: PDDL -> Maybe String
validInput (CheckPDDL
            (Domain name reqs types conss preds actions)
            (Problem _ domName objects initPreds worlds obss goal)) =
              let
                allObjects = addConstantsToObjs conss objects
                allPreds = concatMap (predToProps allObjects) preds
                tests =
                  [ (and [objType `elem` types | (TO _ objType) <- allObjects], "Object type is not declared in :types"),
                    (and [count objType [objType | (TO _ objType) <- allObjects] == 1 | (TO _ objType) <- allObjects],
                      "Multiple definitions of same object type, sorry for limited translation"),
                    --(and [consType `elem` types | (VTL _ consType) <- conss], "Constant type is not declared in :types"),
                    (all (predTypesExist types) preds, "Predicate type is missing"),
                    (all requirementSupported reqs, "requirements not supported"), --TODO which one
                    (name == domName, "Problem's domain-name does not match domain's name"),
                    (allDifferent [name | (World _ name _) <- worlds], "Multiple worlds have the same name"),
                    (allDifferent [name | (Action name _ _ _ _) <- actions], "Multiple actions have the same name"),
                    (formInCorrectFormat goal, "Goal format is incorrect")] ++
                    [ (convertPred pred `elem` allPreds
                      , "World " ++ name ++ " has invalid predicate: " ++ show pred)
                      | (World _ name worldPreds) <- worlds, pred <- worldPreds] ++
                    [ (convertPred pred `elem` allPreds
                      , "Init has invalid predicate: " ++ show pred)
                      | pred <- initPreds] ++
                    [([des | (World des _ _) <- worlds, des] == [True], "Either too many or too few designated worlds")
                  ] ++ map (checkAction types) actions
                in
                  case [ error | (False,error) <- tests ] of
                    [] -> Nothing
                    errors -> Just $ unlines errors

--
convertPred :: Predicate -> Predicate 
convertPred (PredAtom name) = PredSpec name [] False
convertPred (PredDef name vars) = error $ "Error in checking semantics, predicate " ++ name ++ " is defined in problem." ++
  " Maybe you need to remove the types?"
convertPred pred = pred

checkAction :: [String] -> Action -> (Bool,String) --TODO Check observability legitness, also that actor is a listed agent
checkAction typeList (Action name params actor events obss) =
  let tuples =
            [ (and [formInCorrectFormat pre | (Event _ _ pre _) <- events], "Precondition is in an incorrect format"),
              (and [effInCorrectFormat eff | (Event _ _ _ eff) <- events], "Effect is in incorrect format"),
              (allDifferent [name | (Event _ name _ _) <- events], "Multiple events have the same name"),
              (or [des | (Event des _ _ _) <- events, des], "There needs to be at least one designated event"),
              (and [paramType `elem` typeList | (VTL _ paramType) <- params], "Some parameter type is not defined in :types")
            ]
  in
    (all fst tuples, concatMap (\e -> "Error in action \"" ++ name ++ "\": " ++ e ++ "\n") [error | (False, error) <- tuples])



  -- && observabilitiesOnlyForAgents (getObjNames "agent" typeList) obss --TODO add objectlist to validAction params


--converts predicate to its type --TODO requires parameters of action to convert PredSpecc v
--predConvert :: Predicate -> Predicate
--predConvert (PredDef name vars) = 
--  PredSpec name (map snd $ concatMap typify vars) False
--predConvert (PredSpec name vars _) = predConvert $ PredDef name [VTL varNames | ]
--predConvert (PredAtom name) = predConvert $ PredDef name []

observabilitiesOnlyForAgents :: [String] -> [Obs] -> Bool
observabilitiesOnlyForAgents _ [] = True
observabilitiesOnlyForAgents ags ((ObsDef _):obss) = observabilitiesOnlyForAgents ags obss
observabilitiesOnlyForAgents ags ((ObsSpec _ ags2 _):obss) =
     all (`elem` ags) ags2
  && observabilitiesOnlyForAgents ags obss

observabilityPartitionCorrect :: [String] -> Obs -> Bool
observabilityPartitionCorrect legitNames (ObsDef (Partition part)) = and $ concatMap (map (`elem` legitNames)) part
observabilityPartitionCorrect legitNames (ObsSpec (Partition part) _ _) = and $ concatMap (map (`elem` legitNames)) part
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
effInCorrectFormat (Atom _) = True
effInCorrectFormat _ = False

isPred :: Form -> Bool
isPred (Atom _) = True
isPred (Knows _ _) = True
isPred (Not p) = isPred p
isPred _ = False

requirementSupported :: Req -> Bool --TODO add more reqs to parser and language
requirementSupported Strips = True
requirementSupported Typing = True
