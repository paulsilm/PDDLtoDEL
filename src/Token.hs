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
  | TokenConss             {apn :: a}
  | TokenDomain            {apn :: a}
  | TokenAction            {apn :: a}
  | TokenObservability     {apn :: a}
  | TokenPrecondition      {apn :: a}
  | TokenEffect            {apn :: a}
  | TokenByagent           {apn :: a}
  | TokenDomainName        {apn :: a}
  | TokenProblemName       {apn :: a}
  | TokenKnows             {apn :: a}
  | TokenStrips            {apn :: a}
  | TokenTyping            {apn :: a}
  | TokenPartition         {apn :: a}
  | TokenNone              {apn :: a}
  | TokenFull              {apn :: a}
  | TokenDash              {apn :: a}
  | TokenVar {fooV::String, apn :: a}
  | TokenStr {fooS::String, apn :: a}
  | TokenParameters        {apn :: a}
  | TokenTypes             {apn :: a}
  | TokenObjects           {apn :: a}
  | TokenTop               {apn :: a}
  | TokenBot               {apn :: a}
  | TokenNeg               {apn :: a}
  | TokenOB                {apn :: a}
  | TokenCB                {apn :: a}
  | TokenCon               {apn :: a}
  | TokenDis               {apn :: a}
  | TokenImpl              {apn :: a}
  | TokenForall            {apn :: a}
  | TokenExists            {apn :: a}
  | TokenWhen              {apn :: a}
  deriving (Eq,Show)