{ {-# OPTIONS_GHC -w #-}
module Parse where
import Token
import Lex
import PDDL
}




%name parse PDDL
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
  WORLDS       { TokenWorlds           _ }
  INIT         { TokenInit             _ }
  GOAL         { TokenGoal             _ }
  REQS         { TokenRequirements     _ }
  CONSS        { TokenConss            _ }
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
PDDL : Domain Problem { CheckPDDL $1 $2 }

opt(p) : p  { $1 }
       |    { [] }

getReqs : REQS RequirementList ')' '(' { $2 }

getConstants : CONSS TypedObjsList ')' '(' { $2 }

Domain : '(' DEF 
                 '(' DomainName ')' 
                 '(' opt(getReqs)
                 TYPES TypeList ')'    
                 '(' opt(getConstants)
                 PREDS PredicateList ')' 
                 ActionList         
             ')'  { Domain $4 $7 $9 $12 $14 $16 }

RequirementList : Requirement { [$1] }
                | Requirement RequirementList { $1:$2 }

Requirement : STRIPS { Strips }
            | TYPING { Typing }

TypeList : String { [$1] }
         | String StringList { $1:$2 }

PredicateList : '(' Predicate ')' { [$2] }
              | '(' Predicate ')' PredicateList { $2:$4 }

Predicate : String { PredAtom $1 }
          | String VarList { PredSpec $1 $2 True }
          | String StringList { PredSpec $1 $2 False }
          | String VarTypeList { PredDef $1 $2 }

ActionList : '(' ACT Action ')'  { [$3] }
           | '(' ACT Action ')' ActionList { $3:$5 }

Action : String Params ByAgent EventList ObsList { Action $1 $2 $3 $4 $5 }
       | String Params ByAgent Event { Action $1 $2 $3 [$4] [] }

Params : PARAMS '(' VarTypeList ')' { $3 }

VarTypeList : VarType { [$1] } 
            | VarType VarTypeList { $1:$2 } 

VarType : VarList '-' String { VTL $1 $3 } 

VarList : VarName { [$1] }
     | VarName VarList { $1:$2 }

ByAgent : BYA VarName { $2 }

Event : Precondition Effect { Event True "" $1 $2 }

EventList : '(' IsEventDesignated String Precondition Effect ')' { [Event $2 $3 $4 $5] }
          | '(' IsEventDesignated String Precondition Effect ')' EventList { (Event $2 $3 $4 $5):$7 }

IsEventDesignated : EDES { True }
                  | ENON { False }

Precondition : PRECON Form { $2 }

Effect : EFF Form { $2 }

DomainName : DOMNAME String { $2 }

Problem : '(' DEF
                   '(' PROBLEMNAME String ')'
                   '(' DOM String ')'
                   '(' OBJ TypedObjsList ')'
                   '(' INIT PredicateList ')'
                   '(' WORLDS WorldList ')'
                   '(' ObsList ')' 
                   '(' GOAL Form ')'
                ')' { Problem $5 $9 $13 $17 $21 $24 $28 }

TypedObjsList : TypedObjs { [$1] }
            | TypedObjs TypedObjsList { $1:$2 }

TypedObjs : StringList '-' String { TO $1 $3 }

WorldList : '(' IsWorldDesignated String PredicateList ')' { [World $2 $3 $4] }
          | '(' IsWorldDesignated String PredicateList ')' WorldList { (World $2 $3 $4):$6 }

IsWorldDesignated : WDES { True }
                  | WNON { False }

ObsList : ObsType { [$1] }
        | ObsType ObsList { $1:$2 }

ObsType : Obs VarList { ObsSpec $1 $2 }
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
     | '(' 'Forall' '(' VarTypeList ')' Form ')' { Forall $4 $6 }
     | '(' 'Forall' '(' VarTypeList ')' WHEN Form Form ')' { ForallWhen $4 $7 $8 } 
     | '(' 'Exists' '(' VarTypeList ')' Form ')' { Exists $4 $6 }
     | '(' KNOWS VarName Form ')' { Knows $3 $4 }

FormList : Form { [$1] } 
         | Form FormList { $1:$2 }

String : STR { $1 }
VarName : VAR { tail $1 }
--TODO same with init predicates
--TODO try and fix the observability requiring ?var instead of OBJ as parameter (or call it a feature)
{
     
type ParseResult a = Either (Int,Int) a

parseError :: [Token AlexPosn] -> ParseResult a
parseError []     = Left (1,1)
parseError (t:ts) = Left (lin,col)
  where (AlexPn abs lin col) = apn t
}