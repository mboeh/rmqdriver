module Main where

import AMQPTap
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Control.Monad (liftM, foldM_)
import qualified System.Console.Readline as RL

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

execFile :: EngineResult -> String -> IO ()
execFile state filename = do
  commands <- (liftM $ (fmap parseCommand) . (fmap words) . lines) $ readFile filename
  execCommands state commands
  where execCommands _ []           = return ()
        execCommands s (cmd : rest) = do newState <- execCommand s cmd
                                         execCommands newState rest

main :: IO ()
main = do
  engine <- connectEngine "amqp://guest:guest@localhost"
  args <- getArgs
  case args of
    []         -> repl "> " $ engineStartState engine
    [filename] -> execFile (engineStartState engine) filename
    _          -> fail "bad arguments"
