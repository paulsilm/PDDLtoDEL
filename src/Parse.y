{ {-# OPTIONS_GHC -w #-}
module Parse where
import Token
import Lex
}




%name parse CheckInput
%tokentype { Token AlexPosn }
%error { parseError }

%monad { ParseResult } { >>= } { Right }

%token
  TOP          { TokenTop              _ }
  BOT          { TokenBot              _ }
  '('          { TokenOB               _ }
  ')'          { TokenCB               _ }
  '~'          { TokenNeg              _ }
  '-'          { TokenDash             _ }
  '->'         { TokenImpl             _ }
  AND          { TokenCon              _ }
  OR           { TokenDis              _ }
  STR          { TokenStr         $$   _ }
  VAR          { TokenVar         $$   _ }
  'Forall'     { TokenForall           _ }
  'Exists'     { TokenExists           _ }
  'none'       { TokenNone             _ }
  'full'       { TokenFull             _ }
  DEF          { TokenDefine           _ }
  AGENTS       { TokenAgents           _ }
  PREDS        { TokenPredicates       _ }
  EDES         { TokenEventDes         _ }
  ENON         { TokenEventNonDes      _ }
  WDES         { TokenWorldDes         _ }
  WNON         { TokenWorldNonDes      _ }
  INIT         { TokenInit             _ }
  GOAL         { TokenGoal             _ }
  REQS         { TokenRequirements     _ }
  DOM          { TokenDomain           _ }
  ACT          { TokenAction           _ }
  OBS          { TokenObservability    _ }
  STRIPS       { TokenStrips           _ }
  TYPING       { TokenTyping           _ }
  PRECON       { TokenPrecondition     _ }
  EFF          { TokenEffect           _ }
  BYA          { TokenByagent          _ }
  DOMNAME      { TokenDomainName       _ }
  PROBLEMNAME  { TokenProblemName      _ }
  KNOWS        { TokenKnows            _ }
  PARTITION    { TokenPartition        _ }
  PARAMS       { TokenParameters       _ }
  TYPES        { TokenTypes            _ }
  OBJ          { TokenObjects          _ }
  WHEN         { TokenWhen             _ }


%%
CheckInput : CheckDomain CheckProblem { CheckInput $1 $2 }

CheckDomain : '(' DEF 
                 '(' DomainName ')' 
                 '(' REQS RequirementList ')'    
                 '(' TYPES TypeList ')'    
                 '(' PREDS PredicateList ')' 
                 ActionList         
             ')'  { CheckDomain $4 $8 $12 $16 $18 }

RequirementList : Requirement { [$1] }
                | Requirement RequirementList { $1:$2 }

Requirement : STRIPS { Strips }
            | TYPING { Typing }

TypeList : String { [$1] }
         | String StringList { $1:$2 }

PredicateList : '(' Predicate ')' { [$2] }
              | '(' Predicate ')' PredicateList { $2:$4 }

Predicate : String { Pred $1 [] }
          | String VarTypeList { Pred $1 $2 }

ActionList : '(' ACT Action ')'  { [$3] }
           | '(' ACT Action ')' ActionList { $3:$5 }

Action : String Params ByAgent EventList ObsList { Action $1 $2 $3 $4 $5 }
       | String Params ByAgent Event { Action $1 $2 $3 [$4] [] }

Params : PARAMS '(' VarTypeList ')' { $3 }

VarTypeList : VarType { [$1] } 
            | VarType VarTypeList { $1:$2 } 

VarType : Vars '-' String { VTL $1 $3 } 

Vars : VarName { [$1] }
     | VarName Vars { $1:$2 }

ByAgent : BYA VarName { $2 }

Event : Precondition Effect { Event True "" $1 $2 }

EventList : '(' IsEventDesignated String Precondition Effect ')' { [Event $2 $3 $4 $5] }
          | '(' IsEventDesignated String Precondition Effect ')' EventList { (Event $2 $3 $4 $5):$7 }

IsEventDesignated : EDES { True }
                  | ENON { False }

Precondition : PRECON Form { $2 }

Effect : EFF Form { $2 }

DomainName : DOMNAME String { $2 }

CheckProblem : '('
                   '(' PROBLEMNAME String ')'
                   '(' DOM String ')'
                   '(' OBJ ObjList ')'
                   '(' INIT StatementList ')'
                   WorldList
                   '(' ObsList ')' --TODO not sure about this
                   '(' GOAL Form ')'
                ')' { CheckProblem $4 $8 $12 $16 $18 $20 $24 }

ObjList : String '-' String { $1:[$3] }
        | String ObjList { $1:$2 }

WorldList : '(' IsWorldDesignated String StatementList ')' { [World $2 $3 $4] }
          | '(' IsWorldDesignated String StatementList ')' WorldList { (World $2 $3 $4):$6 }

IsWorldDesignated : WDES { True }
                  | WNON { False }

StatementList : '(' Statement ')' { [$2] }
         | '(' Statement ')' StatementList { $2:$4 }

Statement : String { [$1] }
          | String StringList { $1:$2 }

ObsList : ObsType { [$1] }
        | ObsType ObsList { $1:$2 }

ObsType : Obs Vars { ObsSpec $1 $2 }
        | Obs { ObsDef $1 }

Obs : OBS 'full' { Full }
    | OBS 'none' { None }
    | OBS '(' PARTITION PartList ')' { Partition $4 }

PartList : '(' StringList ')' { [$2] }
         | '(' StringList ')' PartList { $2:$4 }

StringList : String { [$1] }
     | String StringList { $1:$2 }


Form : '(' Predicate ')' { Atom $2 }
     | '(' AND ')' { And [] }
     | '(' OR ')' { Or [] }
     | '(' AND FormList ')' { And $3 }
     | '(' OR FormList ')' { Or $3 }
     | '(' '~' Form ')' { Not $3 }
     | '(' '->' Form Form ')' { Imply $3 $4 }
     | '(' 'Forall' '(' VarType ')' Form ')' { Forall $4 $6 }
     | '(' 'Forall' '(' VarType ')' WHEN Form Form ')' { ForallWhen $4 $7 $8 } 
     | '(' 'Exists' '(' VarType ')' Form ')' { Exists $4 $6 }

FormList : Form { [$1] } 
         | Form FormList { $1:$2 }

String : STR { $1 }
VarName : VAR { tail $1 }


{

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

type ParseResult a = Either (Int,Int) a

parseError :: [Token AlexPosn] -> ParseResult a
parseError []     = Left (1,1)
parseError (t:ts) = Left (lin,col)
  where (AlexPn abs lin col) = apn t
}