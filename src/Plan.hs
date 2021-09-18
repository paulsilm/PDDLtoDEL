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
findShortestICPlan :: PDDL -> Bool -> Int -> Bool -> String
findShortestICPlan pddl True d debug
  | planList /= [] = formatSolution $ head planList
  | otherwise = "No implicitly coordinated solution within " ++ show maxDepth ++ " actions."
  where maxDepth = if d == 0 then maxBound :: Int else d
        planList = [head plan | plan <- [map ppICPlan plans 
                                   | plans <- [dfs i debug $ pddlToDEL pddl | i <- [1..maxDepth]] -- Iterative DFS
                                   , plans /= []] -- :: [[String]]
                   ]
findShortestICPlan pddl False d debug
  | planList /= [] = formatSolution $ head planList
  | otherwise = "No solution within " ++ show maxDepth ++ " actions."
  where maxDepth = if d == 0 then maxBound :: Int else d
        planList = [concatMap ppICPlan plan 
                   | plan <- [bfs maxDepth debug $ pddlToDEL pddl] -- BFS
                   , plan /= Nothing] 

--just a wrapper for dls
dfs :: (Typeable action, Eq state, Update state action) => Int -> Bool -> CoopTask state action -> [ICPlan action]
dfs = dls 0
--Depth Limited Search: Copied and adjusted from SMCDEL.Other.Planning (findSequentialIcPlan)
dls :: (Typeable action, Eq state, Update state action) => Int -> Int -> Bool -> CoopTask state action -> [ICPlan action]
dls d maxDepth debug (CoopTask now acts goal)
  | now |= goal   = [ [] ] -- goal reached
  | d == maxDepth = [    ] -- give up
  | otherwise     = [ (agent,(label, act)) : continue
                    | a@(agent,(label,act)) <- acts
                    , now |= preOf act           -- action must be executable
                    , now |= K agent (preOf act) -- agent must know that it is executable!
                    , now /= update now act      -- ignore useless actions
                    , if debug then trace ((replicate d '\t') ++ label ++ "\t" ++ show now) True else True
                    , continue <- dls (d+1) maxDepth debug (CoopTask (update now act) acts goal) -- DFS!
                    , icSolves (CoopTask now acts goal) (a:continue) ]

--Breadth First Search: Copied and adjusted from SMCDEL.Other.Planning (findSequentialIcPlanBFS)
bfs :: (Typeable action, Eq state, Update state action) => Int -> Bool -> CoopTask state action -> Maybe (ICPlan action)
bfs maxDepth debug (CoopTask start acts goal) = loop [([],start)] where
  loop [] = Nothing
  loop ((done,now):rest)
    | now |= goal = Just done
    | otherwise   = loop $ rest ++
                      [ (done ++ [a], update now act) 
                      | length done < maxDepth     -- do not use more than maxDepth actions
                      , a@(agent,(label,act)) <- acts
                      , now |= preOf act           -- action must be executable
                      , now |= K agent (preOf act) -- agent must know that it is executable!
                      , now /= update now act      -- ignore useless actions
                      , if debug then trace ((replicate (length done) '\t') ++ label ++ "\t" ++ show now) True else True
                      ]


formatSolution :: String -> String
formatSolution actions = 
  concatMap (\a -> tail $ dropWhile (/= ':') a ++ ";\n") $ split ';' actions

split :: Eq a => a -> [a] -> [[a]]
split delim str = case break (==delim) str of
                (a, _delim:b) -> a : split delim b
                (a, _empty)   -> [a]