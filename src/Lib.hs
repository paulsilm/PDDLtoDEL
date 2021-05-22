module Lib where

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

concatMapTail :: (a -> [b]) -> [a] -> [b]
concatMapTail _ [] = []
concatMapTail pp (a:as) = pp a ++ concatMap pp as
