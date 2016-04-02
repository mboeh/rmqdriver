module Main where

import AMQPTap
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Data.List.Split (splitOneOf)
import qualified System.Console.Readline as RL

data Mode = Interactive
          | ExecuteFile String
          | ExecuteScript String
          | Help

data Options = Options { amqpUri :: String
                       , mode    :: Mode
                       }

mkOptions :: Options
mkOptions = Options "amqp://guest:guest@localhost" Interactive

parseOptions :: [String] -> Options
parseOptions = (flip go) mkOptions
  where go ("-u" : uri : rest) opts      = go rest opts { amqpUri = uri }
        go ("-e" : script : rest) opts   = go rest opts { mode = ExecuteScript script }
        go ("-h" : rest) opts            = go rest opts { mode = Help }
        go ("-b" : filename : rest) opts = go rest opts { mode = ExecuteFile filename }
        go [] opts                       = opts
        go _ opts                      = opts { mode = Help }

-- Command interpreter

repl :: String -> EngineResult -> IO ()
repl prompt state = do
  input <- RL.readline prompt
  case input of
    Nothing     -> exit 
    Just "exit" -> exit
    Just line   -> do RL.addHistory line
                      newState <- execCommand state $ parseCommand $ words line
                      putStrLn $ show $ engineStatus newState
                      repl prompt newState 
  where exit    = exitSuccess 

execScript :: EngineResult -> String -> IO ()
execScript state text = do
  let commands = (fmap parseCommand) . (fmap words) . (splitOneOf "\n;") $ text
  execCommands state commands
  where execCommands _ []           = return ()
        execCommands s (cmd : rest) = do newState <- execCommand s cmd
                                         execCommands newState rest
  
execFile :: EngineResult -> String -> IO ()
execFile state "-" = getContents >>= execScript state
execFile state filename = readFile filename >>= execScript state

usage :: IO ()
usage = do
  putStrLn "Usage: amqptap [-b FILENAME|-e SCRIPT|-h]"
  putStrLn "  -b FILENAME\tExecute contents of FILENAME as a script."
  putStrLn "  -e SCRIPT  \tExecute the string SCRIPT."
  putStrLn "  -h         \tRead this edifying help message."

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  engine <- connectEngine $ amqpUri opts 
  case mode opts of
    Interactive      -> repl "> " $ engineStartState engine
    ExecuteFile fn   -> execFile (engineStartState engine) fn
    ExecuteScript sc -> execScript (engineStartState engine) sc
    Help             -> usage
