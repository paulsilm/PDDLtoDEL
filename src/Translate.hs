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
            convertPart (getObs obss ag) parsedWorlds)) -- get agent's partition of worlds and convert to names
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
getAtomMap :: [TypedObjs] -> [Predicate] -> [(Predicate, Prp)]
getAtomMap objects preds = zip (concat $ map (predToProps objects) preds) (map P [1..]) 

getPreds :: Domain -> [Predicate]
getPreds (Domain _ _ _ preds _) = preds

--converts the predicate to a list of definite (objectifed) propositions
predToProps :: [TypedObjs] -> Predicate -> [Predicate]
predToProps objects (PredDef name vars) =
  let 
    predTypes = concat $ map typify vars -- [letter agent agent]
    allObjectedPreds = foldr (objectify objects) [[]] predTypes -- [[L1,A1,A1], [L2,A1,A1], ...]
    allPreds = map (\as -> PredSpec name as False) allObjectedPreds -- [(PredSpec name [L1,A1,A1] False), 
                                                                    --  (PredSpec name [L2,A1,A1] False)...]
  in allPreds
predToProps _ (PredAtom name) = [(PredSpec name [] False)]--TODO maybe '[(PredAtom name)]' instead?

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

--replaces agent variables with their type e.g. (VTL "at" ["L1","L2"] - "letter") -> ["letter","letter"]
typify :: VarType -> [String]
typify (VTL objs objType) = replicate (length objs) objType

getObjs :: Problem -> [TypedObjs]
getObjs (Problem _ _ objects _ _ _ _) = objects


--pddlFormToDelForm :: PDDL.Form -> [(Predicate, Prp)] -> SMCDEL.Form 
--pddlFormToDelForm pred predMap = 

{-
data Form
  = Top                         -- ^ True Constant
  | Bot                         -- ^ False Constant
  | PrpF Prp                    -- ^ Atomic Proposition
  | Neg Form                    -- ^ Negation
  | Conj [Form]                 -- ^ Conjunction
  | Disj [Form]                 -- ^ Disjunction
  | Xor [Form]                  -- ^ n-ary X-OR
  | Impl Form Form              -- ^ Implication
  | Equi Form Form              -- ^ Bi-Implication
  | Forall [Prp] Form           -- ^ Boolean Universal Quantification
  | Exists [Prp] Form           -- ^ Boolean Existential Quantification
  | K Agent Form                -- ^ Knowing that
  | Ck [Agent] Form             -- ^ Common knowing that
  | Kw Agent Form               -- ^ Knowing whether
  | Ckw [Agent] Form            -- ^ Common knowing whether
  | PubAnnounce Form Form       -- ^ Public announcement that
  | PubAnnounceW Form Form      -- ^ Public announcement whether
  | Announce [Agent] Form Form  -- ^ (Semi-)Private announcement that
  | AnnounceW [Agent] Form Form -- ^ (Semi-)Private announcement whether
  | Dia DynamicOp Form          -- ^ Dynamic Diamond
  deriving (Eq,Ord,Show)
-}

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
