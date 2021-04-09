module Translate where

import PDDL
import SMCDEL.Explicit.S5
import SMCDEL.Language (Prp(..))

type DEL = ([MultipointedActionModelS5], MultipointedModelS5)
--pddlToDEL :: PDDL -> DEL
--pddlToDEL (CheckPDDL domain problem) =
--  (DomainToActionModels domain (getObjs problem), ProblemToKripke problem)

getAtomMap :: [TypedObjs] -> [Predicate] -> [(Prp, Predicate)]
getAtomMap objects preds = zip (map P [1..]) $ concat $ map (predToProps objects) preds 

getPreds :: Domain -> [Predicate]
getPreds (Domain _ _ _ preds _) = preds

predToProps :: [TypedObjs] -> Predicate -> [Predicate]
predToProps objects (PredDef name vars) =
  let 
    predTypes = concat $ map typify vars -- [letter agent agent]
    allAtoms = foldr (objectify objects) [] predTypes
    allPreds = map (PredSpec name) allAtoms
  in allPreds

--predTypes :: [String] (e.g. [letter agent agent])

--function that augments the existing varList by all objects
--that suit the type of the current variable
-- a :: String
-- b :: [[String]] (something like [ [L, A1, A1], 
--                                   [L, A1, A2] ]) -- where L,A1,A2 are names of objects
--(objectify objects) -> [] -> predTypes  -> allAtoms 
--(a -> b -> b)       -> b  -> [a]        -> b

--objectify objs :: a -> b -> b
--                  String -> [[String]] -> [[String]]

objectify :: [TypedObjs] -> String -> [[String]] -> [[String]]
objectify objs predType varList = concat $ map (addTo varList) (getObjNames predType objs)

addTo :: [[String]] -> String -> [[String]]
addTo varList newVar = map (newVar:) varList

-- returns list of objectnames of type pred
getObjNames :: String -> [TypedObjs] -> [String]
getObjNames _ [] = []
getObjNames pred ((TO names objName):objs)
  | pred == objName = names
  | otherwise = getObjNames pred objs

typify :: VarType -> [String]
typify (VTL objs objType) = replicate (length objs) objType



--allAtom = [ connected a b | a <- allAgents, b <- allAgents ]

getObjs :: Problem -> [TypedObjs]
getObjs (Problem _ _ objects _ _ _ _) = objects

--(Problem problemName domainName objects init worlds obss goal)
--domainToActionModels :: Domain -> [TypedObjs] -> [MultipointedActionModelS5]
--domainToActionModels objs (Domain str reqs types preds actions) =
--  map (pddlActionToActionModel (getAtomMap types preds objs)) actions



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
