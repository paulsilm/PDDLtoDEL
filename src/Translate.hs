module Translate where

import PDDL
import SMCDEL.Explicit.S5 
import SMCDEL.Language (Prp(..))
import SMCDEL.Internal.Help ((!))
import Data.Tuple (swap)

type DEL = ([MultipointedActionModelS5], MultipointedModelS5)
--pddlToDEL :: PDDL -> DEL
--pddlToDEL (CheckPDDL domain problem) =
--  (DomainToActionModels domain (getObjs problem), ProblemToKripke problem)

--(Problem problemName domainName objects init worlds obss goal)
--domainToActionModels :: Domain -> [TypedObjs] -> [MultipointedActionModelS5]
--domainToActionModels objs (Domain str reqs types preds actions) =
--  map (pddlActionToActionModel (getAtomMap types preds objs)) actions

--pddlActionToActionModel :: [(Prp, Predicate)] -> Action -> MultipointedActionModelS5
--pddlActionToActionModel atomMap 

problemToKripkeModel ::  [(Prp, Predicate)] -> Problem -> MultipointedModelS5
problemToKripkeModel atomMap (Problem _ _ objects init parsedWorlds obss _) =
  let
    worldMap = zip parsedWorlds [0..]
    val = [ (i, [ (P k, elem statement $ trueHere ++ init) 
                | (P k, statement) <- atomMap ])
          | ((World _ _ trueHere),i)  <- worldMap ]
    worlds = map snd worldMap
    allAgents = getObjNames "agent" objects
    --get all agents alongside their worldpartition, mapped onto worlds (intlist) 
    worldMapWithNames = zip (map (getWorldName . fst) worldMap) worldMap -- (String, (World,Int))--                           partition, agent
    partitionsInStringsMapToAgents = zip (map (\obs -> convertPart (getObs obss obs) parsedWorlds) allAgents) allAgents   --([[String]], String)
    partitionsInIntsMapToAgents = map (\(partition, agent) -> (mapListOfLists (\s -> snd (worldMapWithNames ! s)) partition, agent)) partitionsInStringsMapToAgents   --([[Int]], String)
    agentRels = map swap partitionsInIntsMapToAgents 
    actualWorlds = [i | ((World des _ _), i) <- worldMap, des]
  in
    (KrMS5 worlds agentRels val, actualWorlds)

mapListOfLists :: (a -> b) -> [[a]] -> [[b]]
mapListOfLists _ [] = []
mapListOfLists func (ss:sss) = (map func ss):(mapListOfLists func sss)

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
isInObs ag (ObsDef _) = False
isInObs ag (ObsSpec _ ags) = ag `elem` ags

--Takes in list of all worlds and the obstype of the agent, returns the partition in strings
convertPart :: ObsType -> [PDDL.World] -> [[String]]
convertPart None worlds = [map getWorldName worlds]
convertPart Full worlds = map (:[]) (map getWorldName worlds)
convertPart (Partition partition) _ = partition

getWorldName :: PDDL.World -> String
getWorldName (World _ name _) = name

--Returns a mapping between propositions of type Prp and their corresponding predicate
getAtomMap :: [TypedObjs] -> [Predicate] -> [(Prp, Predicate)]
getAtomMap objects preds = zip (map P [1..]) $ concat $ map (predToProps objects) preds 

getPreds :: Domain -> [Predicate]
getPreds (Domain _ _ _ preds _) = preds

--converts the predicate to a list of definite (objectifed) propositions
predToProps :: [TypedObjs] -> Predicate -> [Predicate]
predToProps objects (PredDef name vars) =
  let 
    predTypes = concat $ map typify vars -- [letter agent agent]
    allAtoms = foldr (objectify objects) [[]] predTypes
    allPreds = map (\as -> PredSpec name as False) allAtoms
  in allPreds

--function that augments the existing varListList by all objects
--that suit the type of the current variable
objectify :: [TypedObjs] -> String -> [[String]] -> [[String]]
objectify objs predType varListList = concat $ map (addTo varListList) (getObjNames predType objs)

--Adds the newVar in front of every list of vars in varListList
addTo :: [[String]] -> String -> [[String]]
addTo varListList newVar = map (newVar:) varListList

-- returns list of objectnames of type pred
getObjNames :: String -> [TypedObjs] -> [String]
getObjNames _ [] = []
getObjNames pred ((TO names objName):objs) = if pred == objName then names else getObjNames pred objs

--replaces agent variables with their type e.g. at ?l1 ?l2 - "letter" -> at "letter" "letter"
typify :: VarType -> [String]
typify (VTL objs objType) = replicate (length objs) objType

getObjs :: Problem -> [TypedObjs]
getObjs (Problem _ _ objects _ _ _ _) = objects

{-

problemToKripkeModel :: _ -> MultipointedModelS5
problemToKripkeModel =
  let
    atomMap = zip [1..] parsedAtoms
    val = [ (i, [ ( P k, statement `elem` trueHere ) |
    (k,statement) <- atomMap ])
          | ((name,trueHere),i)  <- zip parsedWorlds [1..] ]
    worlds = map fst val
    agentRels = [ (ag,[worlds]) | a <- allAgents ]
  in
    (KrMS5 worlds agentRels val, actualWorlds)

init :: MultipointedModelS5
init = KrMS5
  [0,1]
  [ (anne,[[0],[1]]), (bob,[[0,1]]) ]
  [ (0,[(P 0,True ),(P 1,False), (P 2, False)])
  , (1,[(P 0,False),(P 1,False), (P 2, False)]) ], [0] )


type Action = Int      atom bool
type PostCondition = [(Prp,Form)]

                              index   pre  effect             name <- obs
data ActionModelS5 = ActMS5 [(Action,(Form,PostCondition))] [(Agent,Partition)]
  deriving (Eq,Ord,Show)

instance HasAgents ActionModelS5 where
  agentsOf (ActMS5 _ rel) = map fst rel

-- | A safe way to `lookup` all postconditions
safepost :: PostCondition -> Prp -> Form
safepost posts p = fromMaybe (PrpF p) (lookup p posts)

instance Pointed ActionModelS5 Action
type PointedActionModelS5 = (ActionModelS5, Action)

instance HasPrecondition ActionModelS5 where
  preOf _ = Top

instance HasPrecondition PointedActionModelS5 where
  preOf (ActMS5 acts _, actual) = fst (acts ! actual)

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
