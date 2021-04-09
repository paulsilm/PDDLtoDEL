module Translate where

import PDDL
import SMCDEL.Explicit.S5

type DEL = ([MultipointedActionModelS5], MultipointedModelS5)
--pddlToDEL :: PDDL -> DEL
--pddlToDEL (CheckPDDL domain problem) =
--  (DomainToActionModels domain, ProblemToKripke problem)
--(Problem problemName domainName objects init worlds obss goal)
--domainToActionModels :: Domain -> [TypedObjs] -> [MultipointedActionModelS5]
--domainToActionModels (Domain str reqs types preds actions) =
--  map (pddlActionToActionModel (betterPreds types preds)) actions


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
