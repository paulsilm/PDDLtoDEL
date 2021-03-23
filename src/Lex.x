{
{-# OPTIONS_GHC -w #-}
module Lex where
import Token
}

%wrapper "posn"

$dig = 0-9      -- digits
$alf = [a-zA-Z] -- alphabetic characters

tokens :-
  -- ignore whitespace and comments:
  $white+           ;
  ";".*             ;
  -- keywords:
  "define"                { \ p _ -> TokenDefine            p }
  ":predicates"           { \ p _ -> TokenPredicates        p }
  ":event-designated"     { \ p _ -> TokenEventDes          p }
  ":event-nondesignated"  { \ p _ -> TokenEventNonDes       p }
  ":world-designated"     { \ p _ -> TokenWorldDes          p }
  ":world-nondesignated"  { \ p _ -> TokenWorldNonDes       p }
  ":init"                 { \ p _ -> TokenInit              p }
  ":goal"                 { \ p _ -> TokenGoal              p }
  ":requirements"         { \ p _ -> TokenRequirements      p }
  ":domain"               { \ p _ -> TokenDomain            p }
  ":action"               { \ p _ -> TokenAction            p }
  ":observability"        { \ p _ -> TokenObservability     p }
  ":precondition"         { \ p _ -> TokenPrecondition      p }
  ":effect"               { \ p _ -> TokenEffect            p }
  ":byagent"              { \ p _ -> TokenByagent           p }
  ":parameters"           { \ p _ -> TokenParameters        p }
  ":types"                { \ p _ -> TokenTypes             p }
  ":objects"              { \ p _ -> TokenObjects           p }
  ":strips"               { \ p _ -> TokenStrips            p }
  ":typing"               { \ p _ -> TokenTyping            p }
  "domain"                { \ p _ -> TokenDomainName        p }
  "problem"               { \ p _ -> TokenProblemName       p }
  "knows"                 { \ p _ -> TokenKnows             p }
  "partition"             { \ p _ -> TokenPartition         p }
  "none"                  { \ p _ -> TokenNone              p }
  "full"                  { \ p _ -> TokenFull              p }
  "when"                  { \ p _ -> TokenWhen              p }
  -- punctuation:
  "-"                     { \ p _ -> TokenDash              p }
  "("                     { \ p _ -> TokenOB                p }
  ")"                     { \ p _ -> TokenCB                p }
  -- PDDL Formulas:
  "T"                     { \ p _ -> TokenTop               p }
  "F"                     { \ p _ -> TokenBot               p }
  "not"                   { \ p _ -> TokenNeg               p }
  "and"                   { \ p _ -> TokenCon               p }
  "or"                    { \ p _ -> TokenDis               p }
  "forall"                { \ p _ -> TokenForall            p }
  "when"                  { \ p _ -> TokenWhen              p }
  "exists"                { \ p _ -> TokenExists            p }
  "imply"                 { \ p _ -> TokenImpl              p }
  -- Integers and Strings:
  \? $alf [$alf $dig \-]* { \ p s -> TokenVar s             p }
  $alf [$alf $dig]*       { \ p s -> TokenStr s             p }

{
type LexResult a = Either (Int,Int) a

alexScanTokensSafe :: String -> LexResult [Token AlexPosn]
alexScanTokensSafe str = go (alexStartPos,'\n',[],str) where
  go inp@(pos,_,_,str) =
    case (alexScan inp 0) of
      AlexEOF -> Right []
      AlexError ((AlexPn _ line column),_,_,_) -> Left (line,column)
      AlexSkip  inp' len     -> go inp'
      AlexToken inp' len act -> case (act pos (take len str), go inp') of
        (_, Left lc) -> Left lc
        (x, Right y) -> Right (x : y)
}