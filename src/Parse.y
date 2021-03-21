{ //{-# OPTIONS_GHC -w #-}
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

CheckDomain : '(' DEF 
                 '(' DomainName ')' 
                 '(' REQS RequirementList ')'    
                 '(' TYPES TypeList ')'    
                 '(' PREDS PredicateList ')' 
                 ActionList         
             ')'  { CheckDomain $4 $8 $12 $16 $18 }

RequirementList : '-' --TODO

TypeList : String
         | String StringList

PredicateList : '(' Predicate ')'
              | '(' Predicate ')' PredicateList

Predicate : String 
          | String VarTypeList

ActionList : '(' ACT Action ')'  { [$3] }
           | '(' ACT Action ')' ActionList { $3:$5 }

Action : String Params ByAgent EventList ObsList { Action }
       | String Params ByAgent Event 

Params : PARAMS '(' VarTypeList ')'

VarTypeList : Vars { [$1] }
            | Vars VarTypeList { $1:$2 }

Vars : VarName '-' String
     | VarName Vars

ByAgent : BYA VarName { $2 }

Event : Precondition Effect { Event True "" $1 $2 }

EventList : '(' IsEventDesignated String Precondition Effect ')' { [Event $2 $3 $4 $5] }
          | '(' IsEventDesignated String Precondition Effect ')' EventList { (Event $2 $3 $4 $5):$7 }

IsEventDesignated : EDES { True }
                  | ENON { False }

Precondition : PRECON Form 

Effect : EFF Form

DomainName : DOMNAME String { $2 }

CheckProblem : '('
                   '(' PROBLEMNAME String ')'
                   '(' DOM String ')'
                   '(' OBJ ObjList ')'
                   '(' INIT StatementList ')'
                   WorldList
                   '(' ObsList ')' --TODO not sure about this
                   '(' GOAL StatementList ')'
                ')' 

ObjList : String '-' String
        | String ObjList

WorldList : '(' IsWorldDesignated String StatementList ')'
          | '(' IsWorldDesignated String StatementList ')' WorldList

IsWorldDesignated : WDES { True }
                  | WNON { False }

StatementList : '(' Statement ')'
         | '(' Statement ')' StatementList

Statement : String 
          | String StringList 

ObsList : Observability Vars 
        | Observability 

Obs : OBS 'full'
    | OBS 'none' 
    | OBS '(' PARTITION PartList ')'

PartList : '(' StringList ')' 
         | '(' StringList ')' PartList

StringList : String 
     | String StringList 


Form : '(' Predicate ')'
     | '(' AND ')'
     | '(' AND FormList ')'
     | '(' OR FormList ')'
     | '(' '~' Form ')'
     | '(' '->' Form Form ')'
     | '(' 'Forall' '(' Vars ')' Form ')'
     | '(' 'Forall' '(' Vars ')' WHEN Form Form ')' --Use imply thing 
     | '(' 'Exists' '(' Vars ')' Form ')'

FormList : Form { [$1] } 
         | Form FormList { $1:$2 }

String : STR { $1 }
VarName : VAR { $1 }


{

data Event = Event Bool Form Form deriving (Show,Eq,Ord)

data CheckInput = CheckInput [Int] Form [(String,[Int])] JobList deriving (Show,Eq,Ord)
data Job = TrueQ IntList Form | ValidQ Form | WhereQ Form deriving (Show,Eq,Ord)
type JobList = [Job]
type IntList = [Int]
type FormList = [Form]
type ObserveLine = (String,IntList)
type ObserveSpec = [ObserveLine]

type ParseResult a = Either (Int,Int) a

parseError :: [Token AlexPosn] -> ParseResult a
parseError []     = Left (1,1)
parseError (t:ts) = Left (lin,col)
  where (AlexPn abs lin col) = apn t
}