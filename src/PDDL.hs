module PDDL where

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

data CheckDomain = CheckDomain String [Req] [String] [Predicate] [Action]
          deriving (Show, Eq)

data Req = Strips
         | Typing
          deriving (Show, Eq)

data Predicate = PredAtom String
               | PredDef String [VarType]
               | PredSpec String [String]
               deriving (Show, Eq)

data VarType = VTL [String] String 
          deriving (Show, Eq)

data Action = Action String [VarType] String [Event] [Obs]
          deriving (Show, Eq)

data Event = Event Bool String Form Form 
          deriving (Show, Eq)

data Obs = ObsDef ObsType 
         | ObsSpec ObsType [String]
         deriving (Show, Eq)

data ObsType = Full 
             | None
             | Partition [[String]]
             deriving (Show, Eq)

data CheckProblem = 
     CheckProblem String String [ObjType] [[String]] [World] [Obs] Form
     deriving (Show, Eq)

data ObjType = OTL [String] String
             deriving (Show, Eq)
                  

data World = World Bool String [[String]]
           deriving (Show, Eq)

data CheckInput = CheckInput CheckDomain CheckProblem
                deriving (Show, Eq)
