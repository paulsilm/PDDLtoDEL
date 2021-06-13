module Input where


import System.Exit (exitFailure)
import System.Environment (getArgs,getProgName)
import System.IO (hPutStrLn, stderr)


--Arguments (-pdf, -tex, -print, --debug, --nosemantics, -d, -ic)
type Arguments = (String,String,String,Bool,Bool,Int,Bool)
type FileNames = Either String (String,String)
type InputFormat = (Arguments,FileNames)
--Returns 

getInput :: IO (InputFormat)
getInput = do
  args <- getArgs
  let input = inputFromArgs (("", "", "", False, False, 0, False), Left "") args
  case input of
    Just contents -> do
      return contents
    Nothing -> do
      name <- getProgName
      mapM_ (hPutStrLn stderr)
        [ infoline
        , "usage: " ++ name ++ " <filename> | <options>"
        , "  (use filename - for STDIN)"
        , "  -dom <filename>      parse the domain file"
        , "  -prb <filename>      parse the problem file" 
        , "  -print [<filename>]  print the output to file. Use - for stdout." 
        , "  -tex [<filename>]    print the tex file of the initial model and action models in DEL. Use - for stdout."
        , "  -d   <int>           the maximum depth of solution" 
        , "  -ic                  constrain actions to those where the agent knows it will lead to the goal" 
        , "  --debug              print the states being searched through" 
        , "  --nosemantics        ignore the semantic checker (in case it's buggy)" 
        ]
      exitFailure

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
inputFromArgs ((pdf,tex,p,db,nos,d,_), fs) ("-ic":args) = inputFromArgs ((pdf,tex,p,db,nos,d,True), fs) args
--Read maximum depth
inputFromArgs ((pdf,tex,p,db,nos,_,ic), fs) ("-d":i:args) = 
  inputFromArgs ((pdf,tex,p,db,nos, read i::Int, ic), fs) args
--read ignore semantic checker flag
inputFromArgs ((pdf,tex,p,db,_,d,ic), fs) ("--nosemantics":args) = inputFromArgs ((pdf,tex,p,db,True,d,ic), fs) args
--read debug flag
inputFromArgs ((pdf,tex,p,_,nos,d,ic), fs) ("--debug":args) = inputFromArgs ((pdf,tex,p,True,nos,d,ic), fs) args
--read print output filename
inputFromArgs ((pdf,tex,_,db,nos,d,ic), fs) ("-print":filename:args) = inputFromArgs ((pdf,tex,filename,db,nos,d,ic), fs) args
--read model print filename
inputFromArgs ((pdf,_,p,db,nos,d,ic), fs) ("-tex":filename:args) = inputFromArgs ((pdf,filename,p,db,nos,d,ic), fs) args
--read pdf output filename
inputFromArgs ((_,tex,p,db,nos,d,ic), fs) ("-pdf":filename:args) = inputFromArgs ((filename,tex,p,db,nos,d,ic), fs) args
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
