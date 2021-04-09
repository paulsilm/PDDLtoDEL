module PDDL where

-- A first order logic formula
data Form = Atom Predicate 
          | And [Form] 
          | Or [Form]
          | Not Form 
          | Imply Form Form
          | Forall VarType Form
          | ForallWhen VarType Form Form
          | Exists VarType Form 
          | Knows String Form
          deriving (Show, Eq)

-- PDDL Domain: name requirements types all_predicates actions
data Domain = Domain String [Req] [String] [Predicate] [Action]
          deriving (Show, Eq)

-- Requirement for PDDL parser (:strips|:typing)
data Req = Strips
         | Typing
          deriving (Show, Eq)

-- Predicate: (atomic_predicate name|definition name typed_variables|specific name object_names)
data Predicate = PredAtom String
               | PredDef String [VarType]
               | PredSpec String [String]
               deriving (Show, Eq)

--Typed variables: variables type
data VarType = VTL [String] String 
          deriving (Show, Eq)

-- PDDL action: name params actor events observabilities
data Action = Action String [VarType] String [Event] [Obs]
          deriving (Show, Eq)

-- Action events: designated? name precondition effect
data Event = Event Bool String Form Form 
          deriving (Show, Eq)

-- Action or world observabilities: (default|specific_per_agent) (full|none|partitioned) [agents]
data Obs = ObsDef ObsType 
         | ObsSpec ObsType [String]
         deriving (Show, Eq)

--Observability type: (full|none|partitioned partition)
data ObsType = Full 
             | None
             | Partition [[String]]
             deriving (Show, Eq)

--PDDL Problem: problem_name domain_name objects initial_true_predicates worlds observabilities goal
data Problem = 
     Problem String String [TypedObjs] [[String]] [World] [Obs] Form
     deriving (Show, Eq)

--Objects with their type: object_names type
data TypedObjs = TO [String] String
             deriving (Show, Eq)
                  
--World: designated? name initial_true_predicates
data World = World Bool String [[String]]
           deriving (Show, Eq)

--PDDL file: domain problem
data PDDL = CheckPDDL Domain Problem
                deriving (Show, Eq)
