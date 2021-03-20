module Token where
data Token a -- == AlexPn
  = TokenDefine            {apn :: a}
  | TokenAgents            {apn :: a}
  | TokenPredicates        {apn :: a}
  | TokenEventDes          {apn :: a}
  | TokenEventNonDes       {apn :: a}
  | TokenWorldDes          {apn :: a}
  | TokenWorldNonDes       {apn :: a}
  | TokenInit              {apn :: a}
  | TokenGoal              {apn :: a}
  | TokenRequirements      {apn :: a}
  | TokenDomain            {apn :: a}
  | TokenAction            {apn :: a}
  | TokenObservability     {apn :: a}
  | TokenPrecondition      {apn :: a}
  | TokenEffect            {apn :: a}
  | TokenByagent           {apn :: a}
  | TokenDomainName        {apn :: a}
  | TokenProblemName       {apn :: a}
  | TokenKnows             {apn :: a}
  | TokenPartition         {apn :: a}
  | TokenNone              {apn :: a}
  | TokenFull              {apn :: a}
  | TokenDash              {apn :: a}
  | TokenExists            {apn :: a}
  | TokenVar {fooV::String, apn :: a}
  | TokenStr {fooS::String, apn :: a}
  | TokenInt {fooI::Int,    apn :: a}
  | TokenTop               {apn :: a}
  | TokenBot               {apn :: a}
  | TokenNeg               {apn :: a}
  | TokenOB                {apn :: a}
  | TokenCB                {apn :: a}
  | TokenBinCon            {apn :: a}
  | TokenBinDis            {apn :: a}
  | TokenCon               {apn :: a}
  | TokenDis               {apn :: a}
  | TokenImpl              {apn :: a}
  | TokenForall            {apn :: a}
  | TokenExists            {apn :: a}
  deriving (Eq,Show)