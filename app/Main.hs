module Main where

import AMQPTap
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Data.List.Split (splitOneOf)
import qualified System.Console.Readline as RL

data Options = Options { amqpUri       :: String
                       , executeFile   :: Maybe String
                       , executeScript :: Maybe String
                       }

mkOptions :: Options
mkOptions = Options "amqp://guest:guest@localhost" Nothing Nothing

parseOptions :: [String] -> Options
parseOptions = (flip go) mkOptions
  where go ("-u" : uri : rest) opts    = go rest opts { amqpUri = uri }
        go ("-e" : script : rest) opts = go rest opts { executeScript = Just script }
        go (filename : rest) opts      = go rest opts { executeFile = Just filename }
        go _ opts                      = opts

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

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  engine <- connectEngine $ amqpUri opts 
  case (executeFile opts, executeScript opts) of
    (Nothing, Nothing) -> repl "> " $ engineStartState engine
    (Just fn, Nothing) -> execFile (engineStartState engine) fn
    (Nothing, Just sc) -> execScript (engineStartState engine) sc
    _                  -> fail "Only one of -e or filename can be provided"
