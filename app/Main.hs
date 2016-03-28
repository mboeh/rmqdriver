module Main where

import AMQPTap
import qualified System.Console.Readline as RL

-- Basic entities to operate on

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
  where exit    = fail "Exiting."

main :: IO ()
main = do
  engine <- connectEngine "amqp://guest:guest@localhost"
  repl "> " $ engineStartState engine
