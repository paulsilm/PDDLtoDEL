module SemanticChecker where

import PDDL
import Translate
import Lib
import SMCDEL.Internal.Help ((!))

--TODO Change parsing to allow no worlds be defined.
--TODO also make sure that it is indeed allowed to not have any agents in an action.
--TODO check that Forall and Exists only have variables defined

--Checks whether the input is semantically consistent, if not returns a Just String with
--an error message
validInput :: PDDL -> Maybe String
validInput (CheckPDDL
            (Domain domainName _ types conss preds actions)
            p@(Problem _ domName objects initPreds worlds obss goal)) =
              let
                allObjects = addConstantsToObjs conss objects
                allPreds = concatMap (predToProps allObjects) preds
                predsTyped = [ PredSpec name $ map snd $ concatMap typify vars | (PredDef name vars) <- preds] 
                          ++ [ a | a@(PredAtom _) <- preds]-- e.g., PredSpec "predicate1" ["agent", "agent", "brick"]
                tests =
                  [ (and [objType `elem` types | (TO _ objType) <- objects], "Object type in problem file is not declared in :types"),
                    (domainName == domName, "Problem" ++ (pname p) ++ "'s domain-name does not match domain's name"),
                    (allDifferent [name | (World _ name _) <- worlds], "Multiple worlds have the same name"),
                    (allDifferent [name | (Action name _ _ _ _) <- actions], "Multiple actions have the same name"),
                    (allDifferent $ map getPredDefOrAtomName preds, "Multiple predicates have the same name"),
                    (and [ head var == '?' | (PredDef name params) <- preds, (VTL vars name) <- params, var <- vars, var /= []],
                      "Variables of predicates are written as objects (maybe add '?')"),
                    (all (predTypesExist types) preds, "Predicate type is missing"),
                    (and [count objType [objType | (TO _ objType) <- objects] == 1 | (TO _ objType) <- objects],
                      "Multiple definitions of same object type in problem file"),
                    (and [count objType [objType | (TO _ objType) <- conss] == 1 | (TO _ objType) <- conss],
                      "Multiple definitions of same object type in constants"),
                    (and [consType `elem` types | (TO _ consType) <- conss], "Constant type is not declared in :types"),
                    (observabilitiesOnlyForAgents (getObjNames "agent" allObjects) obss, "Observability can only be defined for agents" ),
                    (and [observabilityPartitionCorrect [name | (World _ name _) <- worlds ] obs | obs <- obss], 
                      "Observability partition (of worlds) can only include names of worlds.")
                    ] ++  
                    [ (False, "Goal format is incorrect: " ++ err)
                    | (False, err) <- [validForm predsTyped
                      [ (obj, objType) | (TO objs objType) <- allObjects, obj <- objs] --Map from object name to its type
                      goal]
                    ] ++
                    [ (convertPred pred `elem` allPreds
                      , "World " ++ name ++ " has invalid predicate: " ++ show pred)
                      | (World _ name worldPreds) <- worlds, pred <- worldPreds
                    ] ++
                    [ (convertPred pred `elem` allPreds
                      , "Init has invalid predicate: " ++ show pred)
                      | pred <- initPreds
                    ] ++ 
                    [([des | (World des _ _) <- worlds, des] /= [], "No designated worlds")
                  ] ++ map (checkAction types preds allObjects) actions
                in
                  case [ error | (False,error) <- tests ] of
                    [] -> Nothing
                    errors -> Just $ unlines errors

--Converts PredAtom to PredSpec. Shoots an error for PredEq or PredDef
--since it's only used for world's and init's predicates.
convertPred :: Predicate -> Predicate 
convertPred (PredAtom name) = PredSpec name []
convertPred (PredDef name _) = 
  error $ "Error in checking semantics, predicate " ++ name 
  ++ " is defined in problem with the object types. " 
  ++ "Maybe you need to remove the types?"
convertPred (PredEq v1 v2) = 
  error $ "Error in checking semantics, the equality " 
  ++ "(= " ++ v1 ++ " " ++ v2 ++ ") "
  ++ "is assigned true in problem file. " 
  ++ "Equality is only usable to check values."
convertPred pred = pred

--NB! only use for the definition versions of predicates (for those in :predicates)
getPredDefOrAtomName :: Predicate -> String
getPredDefOrAtomName (PredAtom name) = name
getPredDefOrAtomName (PredDef name _) = name
getPredDefOrAtomName (PredEq v1 v2) = 
  error $ "Error in checking semantics, the equality " 
  ++ "(= " ++ v1 ++ " " ++ v2 ++ ") "
  ++ "is defined in the domain. " 
  ++ "Equality is already defined as part of MAEPL."
getPredDefOrAtomName (PredSpec name _) = 
  error $ "Error in checking semantics, predicate " ++ name 
  ++ " is missing the object types in its definition."

--Adds objects to their suiting types
addParamstoObjs :: [VarType] -> [TypedObjs] -> [TypedObjs]
addParamstoObjs [] objs = objs
addParamstoObjs ((VTL cNames cType):cs) objs =
  addParamstoObjs cs $ (TO ((getObjNames cType objs) ++ cNames) cType):objs

--Checks action's validity
checkAction :: [String] -> [Predicate] -> [TypedObjs] -> Action -> (Bool,String) 
checkAction typeList preds objects (Action name params actor events obss) =
  let 
    allObjects = addParamstoObjs params objects
    objMap = [ (obj, objType) | (TO objs objType) <- allObjects, obj <- objs]--Map from object name to its type
    predsTyped = [ PredSpec name $ map snd $ concatMap typify vars | (PredDef name vars) <- preds] 
                  ++ [ a | a@(PredAtom _) <- preds]-- e.g., PredSpec "predicate1" ["agent", "agent", "brick"]
    tuples = 
              [ (False, "Precondition format of event " ++ name ++ " is incorrect: " ++ err) 
              | (name,(False,err)) <- 
                [(name,validForm predsTyped objMap pre)  
                | (Event _ name pre _) <- events]
              ] ++
              [ (False, "Effect format of event " ++ name ++ " is incorrect: " ++ err) 
              | (name,(False,err)) <- 
                [(name,validForm predsTyped objMap eff)  
                | (Event _ name _ eff) <- events]
              ] ++
              [(actor == "" || actor `elem` (getObjNames "agent" allObjects), 
                "Actor needs to be an agent"),
              (allDifferent [name | (Event _ name _ _) <- events], 
                "Multiple events have the same name"),
              (or [des | (Event des _ _ _) <- events, des], 
                "There needs to be at least one designated event"),
              (and [paramType `elem` typeList | (VTL _ paramType) <- params], 
                "Some parameter type is not defined in :types"),
              (and [ head var == '?' | (VTL vars name) <- params, var <- vars, var /= []], 
                "Parameter names are not in the required form: \"?_\""),
              (observabilitiesOnlyForAgents (getObjNames "agent" allObjects) obss, 
                "Observability can only be defined for agents" ),
              (and [observabilityPartitionCorrect [name | (Event _ name _ _) <- events ] obs | obs <- obss], 
                "Observability partition can only include names of events.")
            ]
  in
    (all fst tuples, concatMap (\e -> "Error in action \"" ++ name ++ "\": " ++ e ++ "\n") [error | (False, error) <- tuples])

--Checks that the observabilities are defined only for agents, none other
observabilitiesOnlyForAgents :: [String] -> [Obs] -> Bool
observabilitiesOnlyForAgents _ [] = True
observabilitiesOnlyForAgents ags ((ObsDef _):obss) = observabilitiesOnlyForAgents ags obss
observabilitiesOnlyForAgents ags ((ObsSpec _ ags2):obss) =
     all (`elem` ags) ags2
  && observabilitiesOnlyForAgents ags obss

--Checks that all names in partition are legitimate (i.e. world names or event names)
observabilityPartitionCorrect :: [String] -> Obs -> Bool
observabilityPartitionCorrect legitNames (ObsDef (Partition part)) = and $ concatMap (map (`elem` legitNames)) part
observabilityPartitionCorrect legitNames (ObsSpec (Partition part) _) = and $ concatMap (map (`elem` legitNames)) part
observabilityPartitionCorrect _ _ = True

--Checks that the types in the predicate definition are defined
predTypesExist :: [String] -> Predicate -> Bool
predTypesExist types (PredDef _ vars) = and [varType `elem` types | (VTL _ varType) <- vars]
predTypesExist _ _ = True

--Checks that all elements are different
allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs) = notElem x xs && allDifferent xs

--Checks that the formula is in a correct format. Takes formula, a list of defined predicates with their types,
--e.g., (PredSpec "connected" ["agent", "agent"]) (achieved by using Translate.typify)
--and a map from variable names to their corresponding type
validForm :: [Predicate] -> [(String,String)] -> Form -> (Bool,String)
validForm ps os (And forms) = 
  case [ err | (False, err) <- map (validForm ps os) forms] of 
    [] -> (True, "")
    errors -> (False, "(and " ++ concatMap (\e -> "(" ++ e ++ ") ") errors ++ ")")
validForm ps os (Or forms) = 
  case [ err | (False, err) <- map (validForm ps os) forms] of 
    [] -> (True, "")
    errors -> (False, "(or " ++ concatMap (\e -> "(" ++ e ++ ") ") errors ++ ")")
validForm ps os (Not f) = 
  case validForm ps os f of
    (True,_) -> (True,"")
    (False, err) -> (False,"not (" ++ err ++ ")")
validForm ps os (Imply f1 f2) = 
  case validForm ps os f1 of
    (True,_) -> 
      case validForm ps os f2 of
        (True,_) -> (True,"")
        (False,err) -> (False, "(_ -> (" ++ err ++ "))")
    (False,err1) ->
      case validLiteral ps os f2 of
        (True,_) -> (False, "((" ++ err1 ++ ") -> _ )")
        (False,err2) -> (False, "((imply (" ++ err1 ++ ") -> (" ++ err2 ++ ")")
validForm _ _ (Forall [] _) = (False, "(Forall ### missing typed variables ### _)")
validForm _ _ (Exists [] _) = (False, "(Forall ### missing typed variables ### _)")
validForm _ _ (ForallWhen [] _ _) = (False, "(Forall ### missing typed variables ### when _ _)")
validForm ps os (Forall vars f) = 
  case [ obj | (VTL objs _) <- vars, obj <- objs, obj `elem` map fst os] of
    [] ->
      case validForm ps (os ++ [ (obj,objType) | (VTL objs objType) <- vars, obj <- objs]) f of
        (True,_) -> (True,"")
        (False,error) -> (False, "(Forall _ " ++ error ++ ")")
    objs -> (False, "(Forall ### Redefining variables: " ++ concatMapTail id objs (", " ++) ++ "### _)")  
validForm ps os (ForallWhen vars f1 f2) = 
  case [ obj | (VTL objs _) <- vars, obj <- objs, obj `elem` map fst os] of
    [] ->
      case validForm ps (os ++ [ (obj,objType) | (VTL objs objType) <- vars, obj <- objs]) f1 of
        (True,_) -> case validForm ps (os ++ [ (obj,objType) | (VTL objs objType) <- vars, obj <- objs]) f2 of
          (True,_) -> (True,"")
          (False,error) -> (False, "(Forall _ when _ " ++ error ++ ")")
        (False,error1) -> case validForm ps (os ++ [ (obj,objType) | (VTL objs objType) <- vars, obj <- objs]) f2 of
          (True,_) -> (False, "(Forall " ++ error1 ++ " when _ _)")
          (False,error2) -> (False, "(Forall " ++ error1 ++ " " ++ error2 ++ ")")
    objs -> (False, "(Forall ### Redefining variables: " ++ concatMapTail id objs (", " ++) ++ "### when _ _)") 
validForm ps os (Exists vars f) = 
  case [ obj | (VTL objs _) <- vars, obj <- objs, obj `elem` map fst os] of
    [] ->
      case validForm ps (os ++ [ (obj,objType) | (VTL objs objType) <- vars, obj <- objs]) f of
        (True,_) -> (True,"")
        (False,error) -> (False, "(Exists _ " ++ error ++ ")")
    objs -> (False, "(Exists ### Redefining variables: " ++ concatMapTail id objs (", " ++) ++ "### _)") 
validForm ps os (Knows a f) 
  | a `notElem` map fst os = (False, "### atom " ++ a ++ " is not defined ###")
  | os ! a /= "agent" = (False, "### knows should be about agent, but " ++ a ++ " is of type " ++ os ! a ++ " ###")
  | otherwise = 
    case validForm ps os f of
      (True,_) -> (True,"")
      (False, err) -> (False,"(knows " ++ a ++ " " ++ err ++ ")")
validForm ps os (CommonKnow f) =
  case validForm ps os f of
    (True,_) -> (True,"")
    (False, err) -> (False,"(common-knowledge " ++ err ++ ")")
validForm ps os (Atom a) = validPred ps os a

{-Allowed:
Conj<EffForm>
-}
validEffect :: [Predicate] -> [(String,String)] -> Form -> (Bool,String)
validEffect ps os (And ls) = 
  case [ err | (False, err) <- map (validEffForm ps os) ls] of
    [] -> (True, "")
    errors -> (False, "(and " ++ (concatMap (++ " ") errors) ++ ")")
validEffect _ _ _ = (False, "### effect can###")

{- allowed: 
Literal
Form -> EffElement
-}
validEffForm :: [Predicate] -> [(String,String)] -> Form -> (Bool,String)
validEffForm ps os a@(Atom p) = validLiteral ps os a
validEffForm ps os (Imply a b) = 
  case validForm ps os a of
    (True, _) -> case validEffElement ps os b of
      (True, _) -> (True, "")
      (False, err) -> (False, "((_) -> (" ++ err ++ "))")
    (False, err1) -> case validEffElement ps os b of
      (True, _) -> (False, "((" ++ err1 ++ ") -> _ )") 
      (False, err2) -> (False, "((" ++ err1 ++ ") -> (" ++ err2 ++ "))") 

{-Allowed:
Literal
Conj<Literal>
-}--TODO CHECK THAT THERE'S NO EQUALITY PREDICATES
validEffElement :: [Predicate] -> [(String,String)] -> Form -> (Bool,String)
validEffElement ps os a@(Atom _) = validLiteral ps os a
validEffElement ps os (And ls) = 
  case [ err | (False, err) <- map (validLiteral ps os) ls] of
    [] -> (True, "")
    errors -> (False, "(and " ++ (concatMap (++ " ") errors) ++ ")")
    
{-Allowed:
Atom
not(Atom)
-}
validLiteral :: [Predicate] -> [(String,String)] -> Form -> (Bool,String)
validLiteral ps os (Atom p) = validPred ps os p
validLiteral ps os (Not (Not p)) = (False, "### double negation isn't supported ###")
validLiteral ps os (Not p) = 
  case validLiteral ps os p of 
    (True,_) -> (True,"")
    (False,err) -> (False, "not (" ++ err ++ ")")
validLiteral _ _ _ = (False, "### should be a literal (atomic proposition or negation thereof) ###")

{-Allowed:
Predicates that are defined,
In case of equality only those where both propositions have same typ and
In case of specific instance of a predicate (e.g. not-connected A1 A2) checks the types match the definition
-}
validPred :: [Predicate] -> [(String,String)] -> Predicate -> (Bool,String)
validPred _ _ (PredDef name _) = (False, "### Predicate " ++ name ++ " defined with types. Maybe remove the types? ###")
validPred _ os (PredEq a b) 
  | a `notElem` map fst os = (False, "### " ++ a ++ " from (= " ++ a ++ " " ++ b ++ ") is not defined ###")
  | b `notElem` map fst os = (False, "### " ++ b ++ " from (= " ++ a ++ " " ++ b ++ ") is not defined ###")
  | os ! a == os ! b = (True, "")
  | otherwise = (False, "### " ++ b ++ " and " ++ a ++ " from (= " ++ a ++ " " ++ b ++ ") have different types ###")
validPred ps _ p@(PredAtom a) 
  | p `elem` ps = (True, "")
  | otherwise = (False, "### " ++ a ++ " is not defined ###" ++ show ps )
validPred ps os (PredSpec name objs) = --TODO check that the predicate is defined
  case [ obj | obj <- objs, obj `notElem` map fst os] of
    [] -> case [ err | (False, err) <- zipWith (\a b -> (os ! a == b, "(" ++ a ++ " " ++ b ++ ")" )) objs 
                $ concat [ types | (PredSpec n types) <- ps, n == name]] of
      [] -> (True, "")
      errors -> (False, "### predicate " ++ name ++ " has incorrect types for: " ++ show errors ++ "###") 
    objects -> (False, "### objects " ++ show objects ++ " not defined ###")
  