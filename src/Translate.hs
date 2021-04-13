module Translate where

import PDDL
import SMCDEL.Explicit.S5 
import SMCDEL.Language (Prp(..), Form(..))
import SMCDEL.Internal.Help ((!))

type DEL = ([MultipointedActionModelS5], MultipointedModelS5)
type DelEvent = (Bool,String,SMCDEL.Language.Form,SMCDEL.Language.Form)
--pddlToDEL :: PDDL -> DEL
--pddlToDEL (CheckPDDL domain problem) =
--  (DomainToActionModels domain (getObjs problem), ProblemToKripke problem)

domainToActionModels :: Domain -> [TypedObjs] -> [(String, MultipointedActionModelS5)]
domainToActionModels (Domain str reqs types preds actions) objs =
  translateActions (getAtomMap objs preds) objs actions

translateActions :: [(Predicate, Prp)] -> [TypedObjs] -> [PDDL.Action] -> [(String, MultipointedActionModelS5)]
translateActions _ _ [] = []
translateActions atomMap objs ((Action name params _ events obss):actions) =
  let 
    paramMaps = parameterMaps params objs
    convertedEvents = [ (b,n,(pddlFormToDelForm pre atomMap varMap objs),(pddlFormToDelForm eff atomMap varMap objs)) 
                        | (Event b n pre eff) <- events, varMap <- paramMaps]
    eventMap = zip convertedEvents [0..]
    allAgents = getObjNames "agent" objs
    translateEvent s = head [ i | ((_,name,_,_), i) <- eventMap, name == s ] -- takes name s of world and returns its index (unsafe)
    agentRels = 
      map (\ag -> (ag, map (map translateEvent) $ -- take the agent's partition with eventnames and map names to ints
            eventPart (getObs obss ag) convertedEvents)) -- get agent's partition of events and convert to names
          allAgents
    actualEvents = [i | ((des,_,_,_), i) <- eventMap, des]
    action = ActMS5 [(i, (pre, [(P i, eff)])) | ((_,_,pre,eff), i) <- eventMap ] agentRels
  in
    (name, (action, actualEvents)):(translateActions atomMap objs actions)
--effect :: [(Prp,Form)], pre :: Form, agent :: String, partition :: [[Int]], desEvents :: [Int]

problemToKripkeModel ::  [(Predicate, Prp)] -> Problem -> MultipointedModelS5
problemToKripkeModel atomMap (Problem _ _ objects init parsedWorlds obss _) =
  let
    worldMap = zip parsedWorlds [0..]
    val = [ (i, [ (P k, elem statement $ trueHere ++ init) 
                | (statement, P k) <- atomMap ])
          | ((World _ _ trueHere),i)  <- worldMap ]
    worlds = map snd worldMap
    allAgents = getObjNames "agent" objects
    --get all agents alongside their worldpartition, mapped onto worlds::[Int]
    translateWorld s = head [ i | (World _ name _, i) <- worldMap, name == s ] -- takes name s of world and returns its index (unsafe)
    agentRels = 
      map (\ag -> (ag, map (map translateWorld) $ -- take the agent's partition with worldnames and map names to ints
            worldPart (getObs obss ag) parsedWorlds)) -- get agent's partition of worlds and convert to names
          allAgents
    actualWorlds = [i | ((World des _ _), i) <- worldMap, des]
  in
    (KrMS5 worlds agentRels val, actualWorlds)

--Takes observartions and returns partition
getObs :: [Obs] -> String -> ObsType
getObs [] _ = error ("Error: Either no default option or getObs is broken")
getObs ((ObsDef ot):obss) ag 
  | any (isInObs ag) obss = getObs obss ag
  | otherwise = ot
getObs ((ObsSpec ot ags):obss) ag 
  | ag `elem` ags = ot
  | otherwise = getObs obss ag

isInObs :: String -> Obs -> Bool
isInObs _ (ObsDef _) = False
isInObs ag (ObsSpec _ ags) = ag `elem` ags

--Takes in list of all worlds and the obstype of the agent, returns the partition in strings
worldPart :: ObsType -> [PDDL.World] -> [[String]]
worldPart None worlds = [map getWorldName worlds]
worldPart Full worlds = map (:[]) (map getWorldName worlds)
worldPart (Partition partition) _ = partition

--Takes in list of all events and the obstype of the agent, returns the partition in strings
eventPart :: ObsType -> [DelEvent] -> [[String]]
eventPart None events = [[name | (_,name,_,_) <- events]]
eventPart Full events = map (:[]) ([name | (_,name,_,_) <- events])
eventPart (Partition partition) _ = partition

getWorldName :: PDDL.World -> String
getWorldName (World _ name _) = name

--Returns a mapping between propositions of type Prp and their corresponding predicate
getAtomMap :: [TypedObjs] -> [Predicate] -> [(Predicate, Prp)]
getAtomMap objects preds = zip (concatMap (predToProps objects) preds) (map P [1..]) 

getPreds :: Domain -> [Predicate]
getPreds (Domain _ _ _ preds _) = preds

--converts the predicate to a list of definite (objectifed) propositions
predToProps :: [TypedObjs] -> Predicate -> [Predicate]
predToProps objects (PredDef name vars) =
  let 
    objMap = concatMap typify vars 
    objTypes = map snd objMap -- [letter agent agent]
    allObjectedVarLists = foldr (objectify objects) [[]] objTypes -- [[L1,A1,A1], [L2,A1,A1], ...]
    allPreds = map (\as -> PredSpec name as False) allObjectedVarLists -- [(PredSpec name [L1,A1,A1] False), 
                                                                       --  (PredSpec name [L2,A1,A1] False)...]
  in allPreds
predToProps _ (PredAtom name) = [(PredSpec name [] False)]


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

--adds agent variables their type e.g. (VTL "at" ["L1","L2"] - "letter") -> [("L1,"letter"),("L2,"letter")]
typify :: VarType -> [(String,String)]
typify (VTL objs objType) = zip objs $ replicate (length objs) objType

getObjs :: Problem -> [TypedObjs]
getObjs (Problem _ _ objects _ _ _ _) = objects

--  Function to translate a PDDL Formula to the list of matching SMCDEL Formulas
--params: 
--  mapping between object-specific PDDL predicates and SMCDEL propositions
--  mapping between variable names and the specific object it refers to in the DEL action instance
--  objects in the problem file, used for forall and exists statements
pddlFormToDelForm :: PDDL.Form -> [(Predicate, Prp)] -> [(String, String)] -> [TypedObjs] -> SMCDEL.Language.Form 
pddlFormToDelForm (Atom (PredSpec name vars True)) predMap objectMap _ = 
  PrpF $ predMap ! (PredSpec name (map ((!) objectMap) vars) False)
pddlFormToDelForm (Not f) pm om os = Neg $ (pddlFormToDelForm f pm om os) 
pddlFormToDelForm (And fs) pm om os = Conj $ map (\f -> pddlFormToDelForm f pm om os) fs
pddlFormToDelForm (Or fs) pm om os = Disj $ map (\f -> pddlFormToDelForm f pm om os) fs
pddlFormToDelForm (Imply f1 f2) pm om os = Impl (pddlFormToDelForm f1 pm om os) (pddlFormToDelForm f2 pm om os)
pddlFormToDelForm (PDDL.Forall (VTL [var] objType) f) pmap oMap ojs = 
  Conj $ map (\s -> pddlFormToDelForm f pmap ((var,s):oMap) ojs) $ getObjNames objType ojs
pddlFormToDelForm (PDDL.Exists (VTL [var] objType) f) pmap oMap ojs = 
  Disj $ map (\s -> pddlFormToDelForm f pmap ((var,s):oMap) ojs) $ getObjNames objType ojs
pddlFormToDelForm (ForallWhen (VTL [var] objType) f1 f2) pmap oMap ojs = 
  Conj $ map 
        (\s -> Impl 
                (pddlFormToDelForm f1 pmap ((var,s):oMap) ojs) 
                (pddlFormToDelForm f2 pmap ((var,s):oMap) ojs)
        ) $ getObjNames objType ojs
pddlFormToDelForm (Knows ag f) pmap oMap ojs = K ag $ pddlFormToDelForm f pmap oMap ojs

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
    allObjectedVarLists = foldr (objectify objList) [[]] (map snd typeMap)
    -- [["A1","A1"],["A1","A2"],["A2","A1"],["A2","A2"]]
    paramMap = map (zip (map fst typeMap)) allObjectedVarLists
    -- [["A1","A1","L1"],["A1","A2","L1"],["A2","A1","L1"],["A2","A2","L1"]]
  in 
    paramMap


{-
type Action = Int     
--                     atom bool
type PostCondition = [(Prp,Form)]

--                            index   pre  effect             name <- obs
data ActionModelS5 = ActMS5 [(Action,(Form,PostCondition))] [(Agent,Partition)]
  deriving (Eq,Ord,Show)

instance Pointed ActionModelS5 [World]            des
type MultipointedActionModelS5 = (ActionModelS5,[Action])


(:action try-take
        (:event-designated e1
          :precondition (not (key-under-mat))
          :effect T)
        (:event-designated e2
          :precondition (key-under-mat)
          :effect and (not (key-under-mat)) (has-key bob))
        :observability none anne
        :observability full bob

tryTake :: MultipointedActionModelS5
tryTake = (ActMS5
  [ (0, (p              ,[]))
  , (1, (Disj [p, Neg q],[]) )]
  [ ("Anne",[[0],[1]])
  , ("Bob" ,[[0,1]]  )], [0])
-}
