module Lib where

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)
--                (" " ++)     vars   printedvars
-- a is VarType
-- b is Char
-- a -> b is ppVars, should be applied to all vars
-- [b] -> [b] is (" " ++), should only be applied to tail

--Map the first function to the whole list, then apply
--the second function to the tail of the output
concatMapTail :: (a -> [b])-> [a] -> ([b] -> [b]) -> [b]
concatMapTail _ [] _ = []
concatMapTail f (a:as) tf = f a ++ concatMap (tf.f) as

--concatMapTail ppVars params (" " ++)
