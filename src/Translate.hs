module Translate where

import PDDL
import SMCDEL.Explicit.S5 
import SMCDEL.Language (Prp(..), Form(..))
import SMCDEL.Internal.Help ((!))
import SMCDEL.Other.Planning
import Debug.Trace
import Data.List


type DelEvent = (Bool,String,SMCDEL.Language.Form,[(Prp,SMCDEL.Language.Form)])


--Top level translation function, takes a valid PDDL problem and domain combo and returns 
pddlToDEL :: PDDL -> CoopTask MultipointedModelS5 MultipointedActionModelS5
pddlToDEL (CheckPDDL (Domain _ _ types conss preds actions) (Problem _ _ objects initPreds parsedWorlds obss goal)) =
  let
    allObjects = addConstantsToObjs conss objects
    atomMap = getAtomMap types allObjects preds 
    actionModelMap = translateActions types atomMap allObjects conss actions
    kripkeModel = problemToKripkeModel types atomMap (Problem "" "" allObjects initPreds parsedWorlds obss goal)
  in
    CoopTask kripkeModel actionModelMap (pddlFormToDelForm types goal atomMap [(o,o) | (TO os _) <- allObjects, o <- os] allObjects)

--Translates the PDDL actions to a list of actionModels
translateActions :: [TypedTypes] -> [(Predicate, Prp)] -> [TypedObjs] -> [TypedObjs] -> [PDDL.Action] -> [Owned MultipointedActionModelS5]
translateActions _ _ _ _ [] = []
translateActions types atomMap objs conss (a@(Action name params _ _ _):actions) =
  let 
    paramMaps = map ([(obj,obj) | obj <- concatMap getVars conss] ++) (parameterMaps types params objs)
    actionModels = map (actionToActionModel types atomMap objs a) paramMaps 
    actorName actor = if actor == "" then "" else actor ++ ": " 
    addModelIfEmpty ms = if null ms then [actionToActionModel types atomMap objs a [(obj,obj) | obj <- concatMap getVars conss]] else ms
    allModels = addModelIfEmpty actionModels
    ownedModels = map (\(actress,model,paramNames) -> (actress, (actorName actress ++ name ++ " " ++ show paramNames,model))) allModels
  in
    ownedModels ++ (translateActions types atomMap objs conss actions)

--Translates the PDDL action to a tuple (actor,actionmodel,parameter objects), based on a specific parameter to object map
actionToActionModel :: [TypedTypes] -> [(Predicate, Prp)] -> [TypedObjs] -> PDDL.Action -> [(String,String)] -> (String,MultipointedActionModelS5, [String])
actionToActionModel types atomMap objs (Action _ params actor events obss) varMap =
  let
    convertedEvents = [(b,n,pddlFormToDelForm types pre atomMap varMap objs, formToMap (pddlFormToDelForm types eff atomMap varMap objs)) 
                      | (Event b n pre eff) <- events]
    {-
    convertedEvents = [(b,n,pddlFormToDelForm types pre atomMap varMap objs, concatMap formToMap preds) 
                      | (Event b n pre eff) <- events, (Conj preds) <- [(pddlFormToDelForm types eff atomMap varMap objs)]]
                      --TODO doesn't work for some reason
                      -}
    convertedObss = map (translateObs varMap) obss 
    eventMap = zip convertedEvents [0..]
    translateEvent s = head [ i | ((_,name,_,_), i) <- eventMap, name == s ] -- takes name s of event and returns its index (unsafe)
    agentRels = [ (ag, map (map translateEvent) $ eventPart (getObs convertedObss ag) convertedEvents) -- translate partition (String->Int)
                | ag <- getObjsMatchingType types "agent" objs]
    actualEvents = [i | ((des,_,_,_), i) <- eventMap, des]
    actress = if actor == "" then "" else varMap ! actor
  in
    (actress, (ActMS5 [(i, (pre, eff)) | ((_,_,pre,eff), i) <- eventMap ] agentRels, actualEvents), [varMap ! p | p <- concatMap getVars params])

--translates the effect formula to a list of predicate tuples
formToMap :: SMCDEL.Language.Form -> [(Prp,SMCDEL.Language.Form)]
formToMap (Conj preds) = concatMap formToMap preds
formToMap (Neg (PrpF p)) = [(p,Bot)]
formToMap (PrpF p) = [(p,Top)]
formToMap (Impl f (     PrpF p )) = [(p, Conj [Impl f Top, Impl (Neg f) $ PrpF p])]
formToMap (Impl f (Neg (PrpF p))) = [(p, Conj [Impl f Bot, Impl (Neg f) $ PrpF p])]

{-
--translates the effect formula to a list of predicate tuples --TODO broken somehow
formToMap :: SMCDEL.Language.Form -> [(Prp,SMCDEL.Language.Form)]
formToMap (Impl f (     PrpF p )) = [(p, Conj [Impl f Top, Impl (Neg f) $ PrpF p])]
formToMap (Impl f (Neg (PrpF p))) = [(p, Conj [Impl f Bot, Impl (Neg f) $ PrpF p])]-}

--translates a literal to a tuple of proposition and its assigned truth value
literalToMap :: SMCDEL.Language.Form -> (Prp,SMCDEL.Language.Form)
literalToMap (Neg (PrpF p)) = (p,Bot)
literalToMap (PrpF p) = (p,Top)

--Adds objects to their suiting types
addConstantsToObjs :: [TypedObjs] -> [TypedObjs] -> [TypedObjs]
addConstantsToObjs [] objs = objs
addConstantsToObjs ((TO cNames cType):cs) objs =
  addConstantsToObjs cs $ (TO ((getObjNames cType objs) ++ cNames) cType):objs 

--Translates the observabilities to specific agents
translateObs :: [(String,String)] -> Obs -> Obs
translateObs varMap (ObsSpec ot ags) = ObsSpec ot (map (varMap !) ags)
translateObs _ obs = obs

--translates the PDDL problem to a Kripke model
problemToKripkeModel :: [TypedTypes] -> [(Predicate, Prp)] -> Problem -> MultipointedModelS5
problemToKripkeModel types atomMap (Problem _ _ objects initialPreds parsedWorlds obss _) =
  let
    initialWorlds = if null parsedWorlds then [World True "" []] else parsedWorlds
    worldMap = zip initialWorlds [0..]
    val = [ (i, [ (P k, elem statement $ trueHere ++ initialPreds) 
                | (statement, P k) <- atomMap ])
          | (World _ _ trueHere, i)  <- worldMap ]
    worlds = map snd worldMap
    --get all agents alongside their worldpartition, mapped onto worlds::[Int]
    translateWorld s = head [ i | (World _ name _, i) <- worldMap, name == s ] -- takes name s of world and returns its index (unsafe)
    agentRels = [ (ag, map (map translateWorld) $ worldPart (getObs obss ag) initialWorlds) -- translate partition (String->Int)
                | ag <- getObjsMatchingType types "agent" objects]
    actualWorlds = [i | (World des _ _, i) <- worldMap, des]
  in
    (KrMS5 worlds agentRels val, actualWorlds)

--Takes observations and returns partition
getObs :: [Obs] -> String -> ObsType
getObs [] _ = Full -- The default case
getObs (ObsDef ot:obss) ag 
  | any (isInObs ag) obss = getObs obss ag
  | otherwise = ot
getObs ((ObsSpec ot ags):obss) ag 
  | ag `elem` ags = ot
  | otherwise = getObs obss ag

--Checks if the observability is defined for the agent
isInObs :: String -> Obs -> Bool
isInObs _ (ObsDef _) = False
isInObs ag (ObsSpec _ ags) = ag `elem` ags

--Takes in list of all worlds and the obstype of the agent, returns the partition in strings
worldPart :: ObsType -> [PDDL.World] -> [[String]]
worldPart None worlds = [[name | (World _ name _) <- worlds]]
worldPart Full worlds = [[name] | (World _ name _) <- worlds]
worldPart (Partition partition) _ = partition

--Takes in list of all events and the obstype of the agent, returns the partition in strings
eventPart :: ObsType -> [DelEvent] -> [[String]]
eventPart None events = [[name | (_,name,_,_) <- events]]
eventPart Full events =  [[name] | (_,name,_,_) <- events]
eventPart (Partition partition) _ = partition

--Returns a mapping between propositions of type Prp and their corresponding predicate
getAtomMap :: [TypedTypes] -> [TypedObjs] -> [Predicate] -> [(Predicate, Prp)]
getAtomMap types objects preds = zip (concatMap (predToProps types objects) preds) (map P [1..]) 

--converts the predicate to a list of definite (objectifed) propositions
predToProps :: [TypedTypes] -> [TypedObjs] -> Predicate -> [Predicate]
predToProps types objects (PredDef name vars) =
  let 
    objMap = concatMap typify vars 
    objTypes = map snd objMap -- [letter agent agent]
    allObjectedVarLists = foldr (objectify types objects) [[]] objTypes -- [[L1,A1,A1], [L2,A1,A1], ...]
    allPreds = map (PredSpec name) allObjectedVarLists -- [(PredSpec name [L1,A1,A1]), 
                                                                       --  (PredSpec name [L2,A1,A1])...]
  in allPreds
predToProps _ _ (PredAtom name) = [PredAtom name]

--function that augments the existing varListList by all objects
--that suit the type of the current variable
--objectify [(TO ["A1","A2"] "agent"),...] "agent" [["L1"]] = [["A1","L1"],["A2","L1"]]
objectify :: [TypedTypes] -> [TypedObjs] -> String -> [[String]] -> [[String]] 
objectify types objs objType varListList = concatMap (addTo varListList) (getObjsMatchingType types objType objs)

--Adds the newVar in front of every list of vars in varListList
addTo :: [[String]] -> String -> [[String]]
addTo varListList newVar = map (newVar:) varListList

-- returns list of objectnames of type objType
getObjNames :: String -> [TypedObjs] -> [String]
getObjNames _ [] = []
getObjNames objType ((TO names objName):objs) = if objType == objName then names else getObjNames objType objs

-- Returns all objects that are of the type or subtype of objType {Could be optimized if only unique names are taken}
getObjsMatchingType :: [TypedTypes] -> String -> [TypedObjs] -> [String]
getObjsMatchingType types tt objs = 
  let 
    allTypes = getAllSubTypes tt types
    allObjectNames = concatMap (`getObjNames` objs) allTypes
  in nub allObjectNames

--adds agent variables their type and converts TypedVars to list
--(TV "at" ["L1","L2"] - "letter") -> [("L1,"letter"),("L2,"letter")]
typify :: TypedVars -> [(String,String)]
typify (TV objs objType) = zip objs $ replicate (length objs) objType

--  Function to translate a PDDL Formula to the list of matching SMCDEL Formulas
--params: 
--  The PDDL formula to be translated
--  mapping between object-specific PDDL predicates and SMCDEL propositions
--  mapping between variable names and the specific object it refers to in its corresponding DEL action/state instance
--  objects in the problem file, used for forall and exists statements
pddlFormToDelForm :: [TypedTypes] -> PDDL.Form -> [(Predicate, Prp)] -> [(String, String)] -> [TypedObjs] -> SMCDEL.Language.Form 
pddlFormToDelForm types (Atom (PredEq n1 n2)) _ objectMap _
  | objectMap ! n1 == objectMap ! n2 = Top
  | otherwise = Bot
--pddlFormToDelForm types (Atom (PredDef name vars)) atomMap objectMap _ = Bot
pddlFormToDelForm types (Atom (PredSpec name vars)) atomMap objectMap _ = 
  PrpF $ atomMap ! PredSpec name (map (objectMap !) vars)
pddlFormToDelForm types (Atom (PredAtom name)) atomMap _ _ = 
  PrpF $ atomMap ! PredAtom name
pddlFormToDelForm types (Not f) pm om os = Neg $ pddlFormToDelForm types f pm om os 
pddlFormToDelForm types (And fs) pm om os = Conj $ map (\f -> pddlFormToDelForm types f pm om os) fs
pddlFormToDelForm types (Or fs) pm om os = Disj $ map (\f -> pddlFormToDelForm types f pm om os) fs
pddlFormToDelForm types (Imply f1 f2) pm om os = Impl (pddlFormToDelForm types f1 pm om os) (pddlFormToDelForm types f2 pm om os)
--Singleton cases (Forall, ForallWhen, Exists) e.g. forall (?b1 ?b2 - bricks) ...
pddlFormToDelForm types (PDDL.Forall [(TV vars objType)] f) pmap oMap ojs = 
  Conj [pddlFormToDelForm types f pmap ((var,objName):oMap) ojs 
        | objName <- (getObjsMatchingType types objType ojs)
        , var <- vars]
pddlFormToDelForm types (PDDL.Exists [(TV vars objType)] f) pmap oMap ojs = 
  Disj [pddlFormToDelForm types f pmap ((var,objName):oMap) ojs 
        | objName <- (getObjsMatchingType types objType ojs)
        , var <- vars]
pddlFormToDelForm types (ForallWhen [(TV vars objType)] f1 f2) pmap oMap ojs = 
  Conj [Impl 
          (pddlFormToDelForm types f1 pmap ((var,objName):oMap) ojs) 
          (pddlFormToDelForm types f2 pmap ((var,objName):oMap) ojs) 
        | objName <- (getObjsMatchingType types objType ojs)
        , var <- vars] 
--Permutation cases (Forall, ForallWhen, Exists) e.g. forall (?b1 ?b2 - bricks ?a - agent) ...
pddlFormToDelForm types (PDDL.Forall ((TV vars objType):vts) f) pmap oMap ojs = 
  Conj [pddlFormToDelForm types (PDDL.Forall vts f) pmap ((var,objName):oMap) ojs 
        | objName <- (getObjsMatchingType types objType ojs)
        , var <- vars]
pddlFormToDelForm types (PDDL.Exists ((TV vars objType):vts) f) pmap oMap ojs = 
  Disj [pddlFormToDelForm types (PDDL.Exists vts f) pmap ((var,objName):oMap) ojs 
        | objName <- (getObjsMatchingType types objType ojs)
        , var <- vars]
pddlFormToDelForm types (ForallWhen ((TV vars objType):vts) f1 f2) pmap oMap ojs = 
  Conj [pddlFormToDelForm types (ForallWhen vts f1 f2) pmap ((var,objName):oMap) ojs 
        | objName <- (getObjsMatchingType types objType ojs)
        , var <- vars] --Add the variables to the object map and move to next type
--Agent knows about something
pddlFormToDelForm types (Knows ag f) pmap oMap ojs = K (oMap ! ag) $ pddlFormToDelForm types f pmap oMap ojs
--something is common knowledge
pddlFormToDelForm types (CommonKnow f) pmap oMap ojs = Ck (getObjsMatchingType types "agent" ojs) $ pddlFormToDelForm types f pmap oMap ojs

-- Takes the list of all variables, and returns the permutation (list of lists) 
-- of mappings from variable to object names
-- parameterMaps [(TV ["a1","a2"] "agent")] [(TO ["A1","A2"] "agent"),...] = 
--  [[("a1","A1"),("a2","A1")],
--   ...
--   [("a1","A2"),("a2","A2")]]
parameterMaps :: [TypedTypes] -> [TypedVars] -> [TypedObjs] -> [[(String, String)]]
parameterMaps _ [] _ = []
parameterMaps types params objList = 
  let 
    typeMap = concatMap typify params
    -- [("A1","agent"),("A2","agent")]
    allObjectedVarLists = foldr (objectify types objList . snd) [[]] typeMap
    -- [["A1","A1"],["A1","A2"],["A2","A1"],["A2","A2"]]
    paramMap = map (zip (map fst typeMap)) allObjectedVarLists
    -- [["A1","A1","L1"],["A1","A2","L1"],["A2","A1","L1"],["A2","A2","L1"]]
  in 
    paramMap
