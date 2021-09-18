module PDDL where

-- A first order logic formula
data Form = Atom Predicate 
          | And [Form] 
          | Or [Form]
          | Not Form 
          | Imply Form Form
          | Forall [TypedVars] Form
          | ForallWhen [TypedVars] Form Form
          | Exists [TypedVars] Form 
          | Knows String Form
          | CommonKnow Form
          deriving (Show, Eq)

-- TODO enable subtyping
-- PDDL Domain: name requirements types constants all_predicates actions
data Domain = Domain 
               {name :: String, 
               reqs :: [Req], 
               types :: [TypedTypes], 
               constants :: [TypedObjs], 
               predDefs :: [Predicate], 
               actions :: [Action]}
          deriving (Show, Eq)

-- TypeElement    list of types, supertype
data TypedTypes = TT [String] String
                    deriving (Show, Eq)

-- Requirement for PDDL parser (:strips|:typing)
data Req = Strips
         | Typing
         | Equality
         | Adl
          deriving (Show, Eq)

-- Predicate: Atomic name | Definition name typed_variables | Specific name object_names
data Predicate = PredAtom String
               | PredDef String [TypedVars]
               | PredSpec String [String]
               | PredEq String String
               deriving (Show, Eq)

--Typed variables: variables type
data TypedVars = TV [String] String 
          deriving (Show, Eq)

-- PDDL action: name params actor events observabilities
data Action = Action {
               aname :: String,
               params :: [TypedVars],
               actor :: String, 
               events :: [Event], 
               evobss :: [Obs]}
          deriving (Show, Eq)

-- Action events: designated? name precondition effect
data Event = Event Bool String Form Form 
          deriving (Show, Eq)

-- Action or world observabilities: Default ObsType | Specific_per_agent ObsType agents
data Obs = ObsDef ObsType 
         | ObsSpec ObsType [String]
         deriving (Show, Eq)

--Observability type: (Full|None|Partitioned partition)
data ObsType = Full 
             | None
             | Partition [[String]]
             deriving (Show, Eq)

--PDDL Problem: problem_name domain_name objects initial_true_predicates worlds observabilities goal
data Problem = 
     Problem 
     {pname :: String,
     dname :: String,
     objects :: [TypedObjs],
     init :: [Predicate],
     worlds :: [World],
     wobss :: [Obs],
     goal :: Form}
     deriving (Show, Eq)

--Objects with their type: object_names type
data TypedObjs = TO [String] String
             deriving (Show, Eq)
                  
--World: designated? name initial_true_predicates
data World = World Bool String [Predicate]
           deriving (Show, Eq)

--PDDL file: domain problem
data PDDL = CheckPDDL Domain Problem
                deriving (Show, Eq)

class TypedData a where
     getVars :: a -> [String]
     getType :: a -> String 

instance TypedData TypedObjs where
     getVars (TO os _) = os
     getType (TO _ t) = t 

instance TypedData TypedVars where
     getVars (TV vs _) = vs
     getType (TV _ t) = t

instance TypedData TypedTypes where
     getVars (TT ts _) = ts
     getType (TT _ t) = t


-- Whether type is a subtype of another type
subType :: String -- subType
        -> String -- top level type
        -> [TypedTypes] -- Typelist
        -> Bool 
subType _ _ [] = False
subType sub sup ((TT subs topt):types)
  | sub == sup = True
  | sup == topt = sub `elem` subs || any (\t -> subType sub t types) subs
  | otherwise = subType sub sup types

-- Returns the list of all subtypes (including the original type)
getAllSubTypes :: String -> [TypedTypes] -> [String]
getAllSubTypes t [] = [t]
getAllSubTypes "object" types = "object" : concatMap getVars types
getAllSubTypes t ((TT subs topt):types)
  | t == topt = t : concatMap (`getAllSubTypes` types) subs
  | otherwise = getAllSubTypes t types


{-
Vaja välja mõelda kuidas types loogika on vaja kirja panna
kas Agent peab olema alati?
agent teha eriliseks typeks? et "agent" alati subtypeof "agent" nt
-}