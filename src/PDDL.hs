module PDDL where

-- A first order logic formula
data Form = Atom Predicate 
          | And [Form] 
          | Or [Form]
          | Not Form 
          | Imply Form Form
          | Forall [VarType] Form
          | ForallWhen [VarType] Form Form
          | Exists [VarType] Form 
          | Knows String Form
          deriving (Show, Eq)

-- PDDL Domain: name requirements types constants all_predicates actions
data Domain = Domain String [Req] [String] [TypedObjs] [Predicate] [Action]
          deriving (Show, Eq)

-- Requirement for PDDL parser (:strips|:typing)
data Req = Strips
         | Typing
          deriving (Show, Eq)

-- Predicate: Atomic name | Definition name typed_variables | Specific name object_names var?
data Predicate = PredAtom String
               | PredDef String [VarType]
               | PredSpec String [String] Bool
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

-- Action or world observabilities: Default ObsType | Specific_per_agent ObsType agents var?
data Obs = ObsDef ObsType 
         | ObsSpec ObsType [String] Bool
         deriving (Show, Eq)

--Observability type: (Full|None|Partitioned partition)
data ObsType = Full 
             | None
             | Partition [[String]]
             deriving (Show, Eq)

--PDDL Problem: problem_name domain_name objects initial_true_predicates worlds observabilities goal
data Problem = 
     Problem String String [TypedObjs] [Predicate] [World] [Obs] Form
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
