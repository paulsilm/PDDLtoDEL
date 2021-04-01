module PDDL where

data Form = Atom Predicate 
          | And [Form] 
          | Or [Form]
          | Not Form 
          | Imply Form Form
          | Forall VarType Form
          | ForallWhen VarType Form Form
          | Exists VarType Form
data CheckDomain = CheckDomain String [Req] [String] [Predicate] [Action]
data Req = Strips
         | Typing
data Predicate = Pred String [VarType]
data VarType = VTL [String] String 
data Action = Action String [VarType] String [Event] [Obs]
data Event = Event Bool String Form Form 
data Obs = ObsDef ObsType 
         | ObsSpec ObsType [String]
data ObsType = Full 
             | None
             | Partition [[String]]
data CheckProblem = 
     CheckProblem String String [String] [[String]] [World] [Obs] Form
data World = World Bool String [[String]]

data CheckInput = CheckInput CheckDomain CheckProblem