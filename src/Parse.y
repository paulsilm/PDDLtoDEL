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
  '='          { TokenEq               _ }
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
  CONSS        { TokenConss            _ }
  DOM          { TokenDomain           _ }
  ACT          { TokenAction           _ }
  OBS          { TokenObservability    _ }
  STRIPS       { TokenStrips           _ }
  TYPING       { TokenTyping           _ }
  EQUALITY     { TokenEquality         _ }
  ADL          { TokenADL              _ }
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
            | ADL { Adl }
            | EQUALITY { Equality }

TypeList : String { [$1] }
         | String StringList { $1:$2 }

PredicateList : { [] }
              | '(' Predicate ')' { [$2] }
              | '(' Predicate ')' PredicateList { $2:$4 }

Predicate : String { PredAtom $1 }
          | String NameList { PredSpec $1 $2 }
          | String NameList '-' String opt(VarTypeList) { PredDef $1 ((VTL $2 $4):$5) }
          | '=' Name Name { PredEq $2 $3 }

NameList : Name { [$1] }
         | Name NameList { $1:$2 }

Name : VarName { $1 }
     | String { $1 }

ActionList : '(' ACT Action ')'  { [$3] }
           | '(' ACT Action ')' ActionList { $3:$5 }

Action : String Params opt(ByAgent) EventList OptObsList { Action $1 $2 $3 $4 $5 }
       | String Params opt(ByAgent) Event { Action $1 $2 $3 [$4] [] }

OptObsList : { [] }
           | ObsType OptObsList { $1:$2 }

Params : PARAMS '(' OptVarTypeList ')' { $3 }

OptVarTypeList : { [] }
               | VarType OptVarTypeList { $1:$2 } 

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

getInit : INIT PredicateList ')' '(' { $2 }

getObsList : ObsList { $1 }

Problem : '(' DEF
                   '(' PROBLEMNAME String ')'
                   '(' DOM String ')'
                   '(' OBJ TypedObjsList ')'
                   '(' opt(getInit)
                    WorldList 
                    opt(getObsList) 
                    GOAL Form ')'
                ')' { Problem $5 $9 $13 $16 $17 $18 $20 }

TypedObjsList : { [] }
              | TypedObjs { [$1] }
              | TypedObjs TypedObjsList { $1:$2 }

TypedObjs : StringList '-' String { TO $1 $3 }

WorldList : IsWorldDesignated String PredicateList ')' '(' { [World $1 $2 $3] }
          | IsWorldDesignated String PredicateList ')' '(' WorldList { (World $1 $2 $3):$6 }

IsWorldDesignated : WDES { True }
                  | WNON { False }

ObsList : ObsType ')' '(' { [$1] }
        | ObsType ')' '(' ObsList { $1:$4 }

ObsType : Obs VarList { ObsSpec $1 $2}
        | Obs StringList { ObsSpec $1 $2}
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
     | '(' KNOWS Name Form ')' { Knows $3 $4 }

FormList : Form { [$1] } 
         | Form FormList { $1:$2 }

String : STR { $1 }
VarName : VAR { $1 }

{
     
type ParseResult a = Either (Int,Int) a

parseError :: [Token AlexPosn] -> ParseResult a
parseError []     = Left (1,1)
parseError (t:ts) = Left (lin,col)
  where (AlexPn abs lin col) = apn t
}