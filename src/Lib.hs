module Lib where

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)


--Checks that all elements are different
allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs) = notElem x xs && allDifferent xs

--Map the first function to the whole list, then apply
--the second function to the tail of the output
concatMapTail :: (a -> [b])-> [a] -> ([b] -> [b]) -> [b]
concatMapTail _ [] _ = []
concatMapTail f (a:as) tf = f a ++ concatMap (tf.f) as

--concatMapTail ppVars params (" " ++)
