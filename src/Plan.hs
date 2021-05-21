module Plan where

import SMCDEL.Language
import SMCDEL.Other.Planning
import PDDL
import Data.Dynamic
import Data.List
import Translate
import Debug.Trace

--Takes in a PDDL file, whether the plan should have the condition that each agent knows
--before taking an action that it will lead to the goal, and maximum depth of search
--If maxDepth == 0, then it will search infinitely (maxBound::Int)
findShortestICPlan :: PDDL -> Bool -> Int -> String
findShortestICPlan pddl True d
  | planList /= [] = formatSolution $ head planList
  | otherwise = "No solution within " ++ show maxDepth ++ " actions, with icSolves constraint."
  where maxDepth = if d == 0 then maxBound :: Int else d
        planList = [concatMap ppICPlan plan 
                   | plan <- [dls i $ pddlToDEL pddl | i <- [1..maxDepth]] -- Iterative DFS
                   , plan /= []] -- :: [[String]]
findShortestICPlan pddl False d
  | planList /= [] = formatSolution $ head planList
  | otherwise = "No solution within " ++ show maxDepth ++ " actions."
  where maxDepth = if d == 0 then maxBound :: Int else d
        planList = [concatMap ppICPlan plan 
                   | plan <- [bfs maxDepth $ pddlToDEL pddl] -- BFS
                   , plan /= Nothing] 

--Depth Limited Search: Copied and adjusted from SMCDEL.Other.Planning (findSequentialIcPlan)
dls :: (Typeable action, Eq state, Update state action) => Int -> CoopTask state action -> [ICPlan action]
dls d (CoopTask now acts goal)
  | now |= goal = [ [] ] -- goal reached
  | d == 0      = [    ] -- give up
  | otherwise   = [ (agent,(label, act)) : continue
                  | a@(agent,(label,act)) <- acts
                  , now |= preOf act           -- action must be executable
                  , now |= K agent (preOf act) -- agent must know that it is executable!
                  , now /= update now act      -- ignore useless actions
                  , continue <- dls (d-1) (CoopTask (update now act) acts goal) -- DFS!
                  , icSolves (CoopTask now acts goal) (a:continue) ]

--Breadth First Search: Copied and adjusted from SMCDEL.Other.Planning (findSequentialIcPlanBFS)
bfs :: (Typeable action, Eq state, Update state action) => Int -> CoopTask state action -> Maybe (ICPlan action)
bfs maxDepth (CoopTask start acts goal) = loop [([],start)] where
  loop [] = Nothing
  loop ((done,now):rest)
    | now |= goal = Just done
    | otherwise   = loop $ rest ++
                      [ (done ++ [a], update now act) 
                      | length done < maxDepth     -- do not use more than maxDepth actions
                      , a@(agent,(name,act)) <- acts
                      , now |= preOf act           -- action must be executable
                      , now |= K agent (preOf act) -- agent must know that it is executable!
                      , trace (name ++ "\t" ++ show now) True
                      , now /= update now act      -- ignore useless actions
                      ]


formatSolution :: String -> String
formatSolution actions = 
  concatMap (\a -> tail $ dropWhile (/= ':') a ++ ";\n") $ split ';' actions 

split :: Eq a => a -> [a] -> [[a]]
split delim str = case break (==delim) str of
                (a, _delim:b) -> a : split delim b
                (a, _empty)   -> [a]