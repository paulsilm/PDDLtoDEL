module Translate where

import PDDL
import SMCDEL.Explicit.S5 
import SMCDEL.Language (Prp(..), Form(..))
import SMCDEL.Internal.Help ((!))
import SMCDEL.Other.Planning
import Debug.Trace


type DelEvent = (Bool,String,SMCDEL.Language.Form,[(Prp,SMCDEL.Language.Form)])


--Top level translation function, takes a valid PDDL problem and domain combo and returns 
pddlToDEL :: PDDL -> CoopTask MultipointedModelS5 MultipointedActionModelS5
pddlToDEL (CheckPDDL (Domain _ _ _ conss preds actions) (Problem _ _ objects initPreds parsedWorlds obss goal)) =
  let
    allObjects = addConstantsToObjs conss objects
    atomMap = getAtomMap allObjects preds 
    actionModelMap = translateActions atomMap allObjects conss actions
    kripkeModel = problemToKripkeModel atomMap (Problem "" "" allObjects initPreds parsedWorlds obss goal)
  in
    CoopTask kripkeModel actionModelMap (pddlFormToDelForm goal atomMap [(o,o) | (TO os _) <- allObjects, o <- os] allObjects)

--Translates the PDDL actions to a list of actionModels
translateActions :: [(Predicate, Prp)] -> [TypedObjs] -> [TypedObjs] -> [PDDL.Action] -> [Owned MultipointedActionModelS5]
translateActions _ _ _ [] = []
translateActions atomMap objs conss (a@(Action name params _ _ _):actions) =
  let 
    paramMaps = map ([(o,o) | (TO os _) <- conss, o <- os] ++) (parameterMaps params objs)
    actionModels = map (actionToActionModel atomMap objs a) paramMaps 
    actorName actor = if actor == "" then "" else actor ++ ": " 
    addModel ms = if ms == [] then [actionToActionModel atomMap objs a []] else ms
    allModels = addModel actionModels
    ownedModels = map (\(actress,model,paramNames) -> (actress, (actorName actress ++ name ++ " " ++ show paramNames,model))) allModels
  in
    ownedModels ++ (translateActions atomMap objs conss actions)

--Translates the PDDL action to a tuple (actor,actionmodel,parameter objects), based on a specific parameter to object map
actionToActionModel :: [(Predicate, Prp)] -> [TypedObjs] -> PDDL.Action -> [(String,String)] -> (String,MultipointedActionModelS5, [String])
actionToActionModel atomMap objs (Action _ params actor events obss) varMap =
  let
    convertedEvents = [(b,n,pddlFormToDelForm pre atomMap varMap objs,
                            formToMap (pddlFormToDelForm eff atomMap varMap objs)) 
                      | (Event b n pre eff) <- events]
    convertedObss = map (translateObs varMap) obss 
    eventMap = zip convertedEvents [0..]
    translateEvent s = head [ i | ((_,name,_,_), i) <- eventMap, name == s ] -- takes name s of event and returns its index (unsafe)
    agentRels = [ (ag, map (map translateEvent) $ eventPart (getObs convertedObss ag) convertedEvents) -- translate partition (String->Int)
                | ag <- getObjNames "agent" objs]
    actualEvents = [i | ((des,_,_,_), i) <- eventMap, des]
    actress = if actor == "" then "" else varMap ! actor
  in
    (actress, (ActMS5 [(i, (pre, eff)) | ((_,_,pre,eff), i) <- eventMap ] agentRels, actualEvents), [varMap ! p | (VTL ps _) <- params, p <- ps])

--translates the effect formula to a list of predicate tuples
-- TODO: check that no Prp is contained more than once here! also check that in this case implication is only applied to a [conj of] predicate
formToMap :: SMCDEL.Language.Form -> [(Prp,SMCDEL.Language.Form)]
formToMap (Conj preds) = concatMap formToMap preds
formToMap (Neg (PrpF p)) = [(p,Bot)]
formToMap (PrpF p) = [(p,Top)]
formToMap (Impl f (     PrpF p )) = [(p, Conj [Impl f Top, Impl (Neg f) $ PrpF p])]
formToMap (Impl f (Neg (PrpF p))) = [(p, Conj [Impl f Bot, Impl (Neg f) $ PrpF p])]

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
problemToKripkeModel :: [(Predicate, Prp)] -> Problem -> MultipointedModelS5
problemToKripkeModel atomMap (Problem _ _ objects initialPreds parsedWorlds obss _) =
  let
    worldMap = zip parsedWorlds [0..]
    val = [ (i, [ (P k, elem statement $ trueHere ++ initialPreds) 
                | (statement, P k) <- atomMap ])
          | (World _ _ trueHere, i)  <- worldMap ]
    worlds = map snd worldMap
    --get all agents alongside their worldpartition, mapped onto worlds::[Int]
    translateWorld s = head [ i | (World _ name _, i) <- worldMap, name == s ] -- takes name s of world and returns its index (unsafe)
    agentRels = [ (ag, map (map translateWorld) $ worldPart (getObs obss ag) parsedWorlds) -- translate partition (String->Int)
                | ag <- getObjNames "agent" objects]
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
getAtomMap :: [TypedObjs] -> [Predicate] -> [(Predicate, Prp)]
getAtomMap objects preds = zip (concatMap (predToProps objects) preds) (map P [1..]) 

--converts the predicate to a list of definite (objectifed) propositions
predToProps :: [TypedObjs] -> Predicate -> [Predicate]
predToProps objects (PredDef name vars) =
  let 
    objMap = concatMap typify vars 
    objTypes = map snd objMap -- [letter agent agent]
    allObjectedVarLists = foldr (objectify objects) [[]] objTypes -- [[L1,A1,A1], [L2,A1,A1], ...]
    allPreds = map (\as -> PredSpec name as) allObjectedVarLists -- [(PredSpec name [L1,A1,A1]), 
                                                                       --  (PredSpec name [L2,A1,A1])...]
  in allPreds
predToProps _ (PredAtom name) = [PredSpec name []]


--function that augments the existing varListList by all objects
--that suit the type of the current variable
--objectify [(TO ["A1","A2"] "agent"),...] "agent" [["L1"]] = [["A1","L1"],["A2","L1"]]
objectify :: [TypedObjs] -> String -> [[String]] -> [[String]] 
objectify objs objType varListList = concatMap (addTo varListList) (getObjNames objType objs)

--Adds the newVar in front of every list of vars in varListList
addTo :: [[String]] -> String -> [[String]]
addTo varListList newVar = map (newVar:) varListList

-- returns list of objectnames of type objType
getObjNames :: String -> [TypedObjs] -> [String]
getObjNames _ [] = []
getObjNames objType ((TO names objName):objs) = if objType == objName then names else getObjNames objType objs

--adds agent variables their type and converts VarType to list
--(VTL "at" ["L1","L2"] - "letter") -> [("L1,"letter"),("L2,"letter")]
typify :: VarType -> [(String,String)]
typify (VTL objs objType) = zip objs $ replicate (length objs) objType

--  Function to translate a PDDL Formula to the list of matching SMCDEL Formulas
--params: 
--  The PDDL formula to be translated
--  mapping between object-specific PDDL predicates and SMCDEL propositions
--  mapping between variable names and the specific object it refers to in its corresponding DEL action/state instance
--  objects in the problem file, used for forall and exists statements
pddlFormToDelForm :: PDDL.Form -> [(Predicate, Prp)] -> [(String, String)] -> [TypedObjs] -> SMCDEL.Language.Form 
pddlFormToDelForm (Atom (PredEq n1 n2)) _ objectMap _
  | objectMap ! n1 == objectMap ! n2 = Top
  | otherwise = Bot
--pddlFormToDelForm (Atom (PredDef name vars)) atomMap objectMap _ = Bot
pddlFormToDelForm (Atom (PredSpec name vars)) atomMap objectMap _ = 
  PrpF $ atomMap ! PredSpec name (map (objectMap !) vars)
pddlFormToDelForm (Atom (PredAtom name)) atomMap _ _ = 
  PrpF $ atomMap ! PredSpec name []
pddlFormToDelForm (Not f) pm om os = Neg $ pddlFormToDelForm f pm om os 
pddlFormToDelForm (And fs) pm om os = Conj $ map (\f -> pddlFormToDelForm f pm om os) fs
pddlFormToDelForm (Or fs) pm om os = Disj $ map (\f -> pddlFormToDelForm f pm om os) fs
pddlFormToDelForm (Imply f1 f2) pm om os = Impl (pddlFormToDelForm f1 pm om os) (pddlFormToDelForm f2 pm om os)
--Singleton cases (Forall, ForallWhen, Exists) e.g. forall (?b1 ?b2 - bricks) ...
pddlFormToDelForm (PDDL.Forall [(VTL vars objType)] f) pmap oMap ojs = 
  Conj [pddlFormToDelForm f pmap ((var,objName):oMap) ojs 
        | objName <- (getObjNames objType ojs)
        , var <- vars]
pddlFormToDelForm (PDDL.Exists [(VTL vars objType)] f) pmap oMap ojs = 
  Disj [pddlFormToDelForm f pmap ((var,objName):oMap) ojs 
        | objName <- (getObjNames objType ojs)
        , var <- vars]
pddlFormToDelForm (ForallWhen [(VTL vars objType)] f1 f2) pmap oMap ojs = 
  Conj [Impl 
          (pddlFormToDelForm f1 pmap ((var,objName):oMap) ojs) 
          (pddlFormToDelForm f2 pmap ((var,objName):oMap) ojs) 
        | objName <- (getObjNames objType ojs)
        , var <- vars] --TODO double-check if this is valid
--Permutation cases (Forall, ForallWhen, Exists) e.g. forall (?b1 ?b2 - bricks ?a - agent) ...
pddlFormToDelForm (PDDL.Forall ((VTL vars objType):vts) f) pmap oMap ojs = 
  Conj [pddlFormToDelForm (PDDL.Forall vts f) pmap ((var,objName):oMap) ojs 
        | objName <- (getObjNames objType ojs)
        , var <- vars]
pddlFormToDelForm (PDDL.Exists ((VTL vars objType):vts) f) pmap oMap ojs = 
  Disj [pddlFormToDelForm (PDDL.Exists vts f) pmap ((var,objName):oMap) ojs 
        | objName <- (getObjNames objType ojs)
        , var <- vars]
pddlFormToDelForm (ForallWhen ((VTL vars objType):vts) f1 f2) pmap oMap ojs = 
  Conj [pddlFormToDelForm (ForallWhen vts f1 f2) pmap ((var,objName):oMap) ojs 
        | objName <- (getObjNames objType ojs)
        , var <- vars] --Add the variables to the object map and move to next type
--Agent knows about something
pddlFormToDelForm (Knows ag f) pmap oMap ojs = K (oMap ! ag) $ pddlFormToDelForm f pmap oMap ojs

-- Takes the list of all variables, and returns the permutation (list of lists) 
-- of mappings from variable to object names
-- parameterMaps [(VTL ["a1","a2"] "agent")] [(TO ["A1","A2"] "agent"),...] = 
--  [[("a1","A1"),("a2","A1")],
--   ...
--   [("a1","A2"),("a2","A2")]]
parameterMaps :: [VarType] -> [TypedObjs] -> [[(String, String)]]
parameterMaps [] _ = []
parameterMaps params objList = 
  let 
    typeMap = concatMap typify params
    -- [("A1","agent"),("A2","agent")]
    allObjectedVarLists = foldr (objectify objList . snd) [[]] typeMap
    -- [["A1","A1"],["A1","A2"],["A2","A1"],["A2","A2"]]
    paramMap = map (zip (map fst typeMap)) allObjectedVarLists
    -- [["A1","A1","L1"],["A1","A2","L1"],["A2","A1","L1"],["A2","A2","L1"]]
  in 
    paramMap
