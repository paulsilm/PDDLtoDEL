module Input where


import System.Exit (exitFailure)
import System.Environment (getArgs,getProgName)
import System.IO (hPutStrLn, stderr)


--Arguments (-print, --debug, --nosemantics, -d, -ic)
type Arguments = (String, Bool,Bool,Int,Bool)
type FileNames = Either String (String,String)
type InputFormat = (Arguments,FileNames)
--Returns 

getInput :: IO (InputFormat)
getInput = do
  args <- getArgs
  let input = inputFromArgs (("", False, False, 0, False), Left "") args
  case input of
    Just contents -> do
      return contents
    Nothing -> do
      name <- getProgName
      mapM_ (hPutStrLn stderr)
        [ infoline
        , "usage: " ++ name ++ " <filename> | <options>\n"
        --, "  (use filename - for STDIN)\n"
        , "  -dom <filename>   parse the domain file\n"
        , "  -prb <filename>   parse the problem file\n" 
        , "  -print <filename> print the output to file. Use - for stdout\n" 
        , "  -d   <int>        the maximum depth of solution\n" 
        , "  -ic               constrain actions to those where the agent knows it will lead to the goal\n" 
        , "  --debug           print the states being searched through\n" 
        , "  --nosemantics     ignore the semantic checker (in case it's buggy)\n" 
        ]
      exitFailure

{- TODO add:
    '-' for stdin 
    -tex to generate a tex file with initial state diagram (non-essential)
    -pdf to generate a pdf file with initial state diagram (non-essential)
-}
--Returns command line arguments parsed
inputFromArgs :: InputFormat -> [String] -> Maybe InputFormat
--End of arguments, check if input was valid
inputFromArgs i@(_, Left fn) [] 
  | fn /= "" = Just i
  | otherwise = Nothing
inputFromArgs i@(_, Right (dom, prb)) [] 
  | dom /= "" && prb /= "" = Just i
  | otherwise = Nothing
--read implicit coordination flag
inputFromArgs ((p,db,nos,d,_), fs) ("-ic":args) = inputFromArgs ((p,db,nos,d,True), fs) args
--Read maximum depth
inputFromArgs ((p,db,nos,_,ic), fs) ("-d":i:args) = 
  inputFromArgs ((p,db,nos, read i::Int, ic), fs) args
--read ignore semantic checker flag
inputFromArgs ((p,db,_,d,ic), fs) ("--nosemantics":args) = inputFromArgs ((p,db,True,d,ic), fs)args
--read debug flag
inputFromArgs ((p,_,nos,d,ic), fs) ("--debug":args) = inputFromArgs ((p,True,nos,d,ic), fs) args
--read print output filename
inputFromArgs ((_,db,nos,d,ic), fs) ("-print":filename:args) = inputFromArgs ((filename,db,nos,d,ic), fs) args
--read domain filename
inputFromArgs (a, Left fn) ("-dom":dom:args) 
  | fn /= "" = Nothing 
  | otherwise = inputFromArgs (a, Right (dom,"")) args
inputFromArgs (a, Right (fn,prb)) ("-dom":dom:args) 
  | fn /= "" = Nothing
  | otherwise = inputFromArgs (a, Right (dom, prb)) args
--read problem filename
inputFromArgs (a, Left fn) ("-prb":prb:args)
  | fn /= "" = Nothing 
  | otherwise = inputFromArgs (a, Right ("", prb)) args
inputFromArgs (a, Right (dom,fn)) ("-prb":prb:args) 
  | fn /= "" = Nothing
  | otherwise = inputFromArgs (a, Right (dom, prb)) args
--Read regular filename
inputFromArgs (a, Left fo) (fn:args) 
  | fo /= "" = Nothing
  | otherwise = inputFromArgs (a, Left fn) args
inputFromArgs (_, Right _) (_:_) = Nothing
--inputFromArgs _ _ = Nothing

infoline :: String
infoline = "PDDLtoDEL " ++ "1.0.0" ++ " -- https://github.com/paulsilm/PDDLtoDEL\n"
