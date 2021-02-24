module Lib where

import SMCDEL.Language
import SMCDEL.Symbolic.S5

someFunc :: IO ()
someFunc = putStrLn "someFunc"


pddlToPrp :: [String] -> [(String,Prp)]
pddlToPrp names = zip names (map P [0..])
