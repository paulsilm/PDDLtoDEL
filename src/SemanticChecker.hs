module SemanticChecker where

import PDDL
import Translate
import Lib
import SMCDEL.Internal.Help ((!))

--Checks whether the input is semantically consistent, if not returns a Just String with
--an error message
validInput :: PDDL -> Maybe String
validInput (CheckPDDL
            (Domain domainName _ types conss preds actions)
            p@(Problem _ domName objects initPreds worlds obss goal)) =
              let 
                allTypes = getAllSubTypes "object" types
                allObjects = addConstantsToObjs conss objects
                allPreds = concatMap (predToProps types allObjects) preds
                predsTyped = [ PredSpec name $ map snd $ concatMap typify vars | (PredDef name vars) <- preds] 
                          ++ [ a | a@(PredAtom _) <- preds]-- e.g., PredSpec "predicate1" ["agent", "agent", "brick"]
                tests =
                  [ checkTypes types ["object"],
                    (allDifferent $ map getVars allObjects, "Multiple objects have the same name"),
                    (and [ objType `elem` allTypes | (TO _ objType) <- objects], "Object type in problem file is not declared in :types"),
                    (and [ not $ null $ getObjsMatchingType types t allObjects | t <- allTypes ], "Existing type has no matching objects"),
                    (domainName == domName, "Problem" ++ (pname p) ++ "'s domain-name does not match domain's name"),
                    (allDifferent [name | (World _ name _) <- worlds], "Multiple worlds have the same name"),
                    (allDifferent [name | (Action name _ _ _ _) <- actions], "Multiple actions have the same name"),
                    (allDifferent $ map getPredDefOrAtomName preds, "Multiple predicates have the same name"),
                    (and [ head var == '?' | (PredDef name params) <- preds, var <- concatMap getVars params, var /= []],
                      "Variables of predicates are written as objects (maybe add '?')"),
                    (all (predTypesExist allTypes) preds, "Predicate type is missing"),
                    (and [count objType [objType | (TO _ objType) <- objects] == 1 | (TO _ objType) <- objects],
                      "Multiple definitions of same object type in problem file"),
                    (and [count objType [objType | (TO _ objType) <- conss] == 1 | (TO _ objType) <- conss],
                      "Multiple definitions of same object type in constants"),
                    (and [consType `elem` allTypes | (TO _ consType) <- conss], "Constant type is not declared in :types"),
                    (not $ null worlds && obss /= [], "Observabilities cannot be defined if there are no worlds"),
                    (observabilitiesOnlyForAgents (getObjsMatchingType types "agent" allObjects) obss, "Observability can only be defined for agents" ),
                    (and [observabilityPartitionCorrect [name | (World _ name _) <- worlds ] obs | obs <- obss], 
                      "Observability partition (of worlds) can only include names of worlds.")
                    ] ++  
                    [ (False, "Goal format is incorrect: " ++ err)
                    | (False, err) <- [validForm types predsTyped
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
                    [(null worlds || or [ True | (World True _ _) <- worlds], "No designated worlds")
                  ] ++ map (checkAction types preds allObjects) actions
                in
                  case [ error | (False,error) <- tests ] of
                    [] -> Nothing
                    errors -> Just $ unlines errors

--Converts PredAtom to PredSpec. Shoots an error for PredEq or PredDef
--since it's only used for world's and init's predicates.
convertPred :: Predicate -> Predicate 
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
addParamstoObjs :: [TypedVars] -> [TypedObjs] -> [TypedObjs]
addParamstoObjs [] objs = objs
addParamstoObjs ((TV cNames cType):cs) objs =
  addParamstoObjs cs $ (TO ((getObjNames cType objs) ++ cNames) cType):objs

checkTypes :: [TypedTypes] -> [String] -> (Bool,String)
checkTypes [] types = (True,"")
checkTypes ((TT subs topt):rest) types 
  | topt `elem` types = checkTypes rest $ types ++ subs
  | otherwise = (False, "Type " ++ topt ++ " is not declared as a subtype of any other type.")

--Checks action's validity
checkAction :: [TypedTypes] -> [Predicate] -> [TypedObjs] -> Action -> (Bool,String) 
checkAction types preds objects (Action name params actor events obss) =
  let 
    typeList = getAllSubTypes "object" types
    allObjects = addParamstoObjs params objects
    objMap = [ (obj, objType) | (TO objs objType) <- allObjects, obj <- objs]--Map from object name to its type
    predsTyped = [ PredSpec name $ map snd $ concatMap typify vars | (PredDef name vars) <- preds] 
                  ++ [ a | a@(PredAtom _) <- preds]-- e.g., PredSpec "predicate1" ["agent", "agent", "brick"]
    tuples = 
              [ (False, "Precondition format of event " ++ name ++ " is incorrect: " ++ err) 
              | (name,(False,err)) <- 
                [(name,validForm types predsTyped objMap pre)  
                | (Event _ name pre _) <- events]
              ] ++
              [ (False, "Effect format of event " ++ name ++ " is incorrect: " ++ err) 
              | (name,(False,err)) <- 
                [(name,validForm types predsTyped objMap eff)  
                | (Event _ name _ eff) <- events]
              ] ++
              [(actor /= "", 
                "Currently global actions are not supported"),
              (actor `elem` (getObjsMatchingType types "agent" allObjects), 
                "Actor needs to be an agent"),
              (allDifferent [name | (Event _ name _ _) <- events], 
                "Multiple events have the same name"),
              (or [des | (Event des _ _ _) <- events, des], 
                "There needs to be at least one designated event"),
              (and [paramType `elem` typeList | paramType <- map getType params], 
                "Some parameter type is not defined in :types"),
              --(and [paramType `elem` typeList | paramType <- map getType params], 
              --  "Some parameter type has no matching objects"),
              (and [ head var == '?' | var <- concatMap getVars params, var /= []], 
                "Parameter names are not in the required form: \"?_\""),
              (observabilitiesOnlyForAgents (getObjsMatchingType types "agent" allObjects) obss, 
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
predTypesExist types (PredDef _ vars) = and [varType `elem` types | (TV _ varType) <- vars]
predTypesExist _ _ = True

--Checks that the formula is in a correct format. Takes formula, a list of defined predicates with their types,
--e.g., (PredSpec "connected" ["agent", "agent"]) (achieved by using Translate.typify)
--and a map from variable names to their corresponding type
validForm :: [TypedTypes] -> [Predicate] -> [(String,String)] -> Form -> (Bool,String)
validForm ts ps os (And forms) = 
  case [ err | (False, err) <- map (validForm ts ps os) forms] of 
    [] -> (True, "")
    errors -> (False, "(and " ++ concatMap (\e -> "(" ++ e ++ ") ") errors ++ ")")
validForm ts ps os (Or forms) = 
  case [ err | (False, err) <- map (validForm ts ps os) forms] of 
    [] -> (True, "")
    errors -> (False, "(or " ++ concatMap (\e -> "(" ++ e ++ ") ") errors ++ ")")
validForm ts ps os (Not f) = 
  case validForm ts ps os f of
    (True,_) -> (True,"")
    (False, err) -> (False,"not (" ++ err ++ ")")
validForm ts ps os (Imply f1 f2) = 
  case validForm ts ps os f1 of
    (True,_) -> 
      case validForm ts ps os f2 of
        (True,_) -> (True,"")
        (False,err) -> (False, "(_ -> (" ++ err ++ "))")
    (False,err1) ->
      case validForm ts ps os f2 of
        (True,_) -> (False, "((" ++ err1 ++ ") -> _ )")
        (False,err2) -> (False, "((imply (" ++ err1 ++ ") -> (" ++ err2 ++ ")")
validForm _ _ _ (Forall [] _) = (False, "(Forall ### missing typed variables ### _)")
validForm _ _ _ (Exists [] _) = (False, "(Forall ### missing typed variables ### _)")
validForm _ _ _ (ForallWhen [] _ _) = (False, "(Forall ### missing typed variables ### when _ _)")
validForm ts ps os (Forall vars f) = 
  case [ obj | obj <- concatMap getVars vars, obj `elem` map fst os] of
    [] ->
      case validForm ts ps (os ++ [ (obj,objType) | (TV objs objType) <- vars, obj <- objs]) f of
        (True,_) -> (True,"")
        (False,error) -> (False, "(Forall _ " ++ error ++ ")")
    objs -> (False, "(Forall ### Redefining variables: " ++ concatMapTail id objs (", " ++) ++ "### _)")  
validForm ts ps os (ForallWhen vars f1 f2) = 
  case [ obj | obj <- concatMap getVars vars, obj `elem` map fst os] of
    [] ->
      case validForm ts ps (os ++ [ (obj,objType) | (TV objs objType) <- vars, obj <- objs]) f1 of
        (True,_) -> case validForm ts ps (os ++ [ (obj,objType) | (TV objs objType) <- vars, obj <- objs]) f2 of
          (True,_) -> (True,"")
          (False,error) -> (False, "(Forall _ when _ " ++ error ++ ")")
        (False,error1) -> case validForm ts ps (os ++ [ (obj,objType) | (TV objs objType) <- vars, obj <- objs]) f2 of
          (True,_) -> (False, "(Forall " ++ error1 ++ " when _ _)")
          (False,error2) -> (False, "(Forall " ++ error1 ++ " " ++ error2 ++ ")")
    objs -> (False, "(Forall ### Redefining variables: " ++ concatMapTail id objs (", " ++) ++ "### when _ _)") 
validForm ts ps os (Exists vars f) = 
  case [ obj | obj <- concatMap getVars vars, obj `elem` map fst os] of
    [] ->
      case validForm ts ps (os ++ [ (obj,objType) | (TV objs objType) <- vars, obj <- objs]) f of
        (True,_) -> (True,"")
        (False,error) -> (False, "(Exists _ " ++ error ++ ")")
    objs -> (False, "(Exists ### Redefining variables: " ++ concatMapTail id objs (", " ++) ++ "### _)") 
validForm ts ps os (Knows a f) 
  | a `notElem` map fst os = (False, "### atom " ++ a ++ " is not defined ###")
  | not $ subType (os ! a) "agent" ts = (False, "### knows should be about agent, but " ++ a ++ " is of type " ++ os ! a ++ " ###") 
  | otherwise = 
    case validForm ts ps os f of
      (True,_) -> (True,"")
      (False, err) -> (False,"(knows " ++ a ++ " " ++ err ++ ")")
validForm ts ps os (CommonKnow f) =
  case validForm ts ps os f of
    (True,_) -> (True,"")
    (False, err) -> (False,"(common-knowledge " ++ err ++ ")")
validForm ts ps os (Atom a) = validPred ts ps os a

{-Allowed:
Conj<EffForm>
-}
validEffect :: [TypedTypes] -> [Predicate] -> [(String,String)] -> Form -> (Bool,String)
validEffect ts ps os (And ls) = 
  case [ err | (False, err) <- map (validEffForm ts ps os) ls] of
    [] -> (True, "")
    errors -> (False, "(and " ++ (concatMap (++ " ") errors) ++ ")")
validEffect _ _ _ _ = (False, "### effect can###")

{- allowed: 
Literal
Form -> EffElement
-}
validEffForm :: [TypedTypes] -> [Predicate] -> [(String,String)] -> Form -> (Bool,String)
validEffForm ts ps os a@(Atom p) = validLiteral ts ps os a
validEffForm ts ps os (Imply a b) = 
  case validForm ts ps os a of
    (True, _) -> case validEffElement ts ps os b of
      (True, _) -> (True, "")
      (False, err) -> (False, "((_) -> (" ++ err ++ "))")
    (False, err1) -> case validEffElement ts ps os b of
      (True, _) -> (False, "((" ++ err1 ++ ") -> _ )") 
      (False, err2) -> (False, "((" ++ err1 ++ ") -> (" ++ err2 ++ "))") 

{-Allowed:
Literal
Conj<Literal>
-}
validEffElement :: [TypedTypes] -> [Predicate] -> [(String,String)] -> Form -> (Bool,String)
validEffElement ts ps os a@(Atom _) = validLiteral ts ps os a
validEffElement ts ps os (And ls) = 
  case [ err | (False, err) <- map (validLiteral ts ps os) ls] of
    [] -> (True, "")
    errors -> (False, "(and " ++ (concatMap (++ " ") errors) ++ ")")
    
{-Allowed:
Atom
not(Atom)
-}
validLiteral :: [TypedTypes] -> [Predicate] -> [(String,String)] -> Form -> (Bool,String)
validLiteral ts ps os (Atom p) = validEffPred ts ps os p
validLiteral ts ps os (Not p) = 
  case validLiteral ts ps os p of 
    (True,_) -> (True,"")
    (False,err) -> (False, "not (" ++ err ++ ")")
validLiteral _ _ _ _ = (False, "### should be a literal (atomic proposition or negation thereof) ###")

{-Allowed:
Predicates that are defined,
In case of specific instance of a predicate (e.g. not-connected A1 A2) checks the types match the definition
-}
validPred :: [TypedTypes] -> [Predicate] -> [(String,String)] -> Predicate -> (Bool,String)
validPred _ _ os (PredEq a b) 
  | a `notElem` map fst os = (False, "### " ++ a ++ " from (= " ++ a ++ " " ++ b ++ ") is not defined ###")
  | b `notElem` map fst os = (False, "### " ++ b ++ " from (= " ++ a ++ " " ++ b ++ ") is not defined ###")
  | otherwise = (True, "")
validPred ts ps os p = validEffPred ts ps os p

{-Allowed:
Predicates that are defined,
In case of specific instance of a predicate (e.g. not-connected A1 A2) checks the types match the definition
NotAllowed:
Equality predicates
-}
validEffPred :: [TypedTypes] -> [Predicate] -> [(String,String)] -> Predicate -> (Bool,String)
validEffPred _ _ _ (PredDef name _) = (False, "### Predicate " ++ name ++ " defined with types. Maybe remove the types? ###")
validEffPred _ _ _ (PredEq a b) = (False, "### Can't assign " ++ a ++ " to be equal to " ++ b ++ " ###")
validEffPred _ ps _ p@(PredAtom a) 
  | p `elem` ps = (True, "")
  | otherwise = (False, "### " ++ a ++ " is not defined ###" ++ show ps )
validEffPred types ps os (PredSpec name objs) =
  case [ obj | obj <- objs, obj `notElem` map fst os] of
    [] -> case [ err | (False, err) <- zipWith (\a b -> (subType (os ! a) b types, "(" ++ a ++ " " ++ b ++ ")" )) objs 
                $ concat [ ts | (PredSpec n ts) <- ps, n == name]] of
      [] -> if name `elem` [ n | (PredSpec n _) <- ps] then (True, "") else (False, "### predicate " ++ name ++ " is not defined ###")
      errors -> (False, "### predicate " ++ name ++ " has incorrect types for: " ++ show errors ++ "###") 
    objects -> (False, "### objects " ++ show objects ++ " not defined ###")
  