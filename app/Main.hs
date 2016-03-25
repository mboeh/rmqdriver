module Main where

import Control.Monad (forever)
import Data.List (intercalate)
import qualified System.Console.Readline as RL
import qualified Network.AMQP as AM

newtype Queue    = Queue String deriving (Show)
newtype Exchange = Exchange String deriving (Show)
newtype Topic    = Topic String deriving (Show)

data Sink    = SinkHandler ((AM.Message, AM.Envelope) -> IO ())

defaultSink :: Sink
defaultSink = SinkHandler handler
  where handler (_msg, env) = AM.ackEnv env

instance Show Sink where
  show (SinkHandler _) = "default"

mkSink :: String -> Sink
mkSink _ = defaultSink

data Command = SourceAdd
             | SourceDrop Queue
             | SourceTap Queue Exchange Topic
             | SourceUntap Queue Exchange Topic
             | SourceDrain Queue Sink

instance Show Command where
  show (SourceAdd) = "add"
  show (SourceDrop (Queue queue)) = intercalate " " ["drop", queue] 
  show (SourceTap (Queue queue) (Exchange exchange) (Topic topic)) = intercalate " " ["tap", queue, exchange, topic]
  show (SourceUntap (Queue queue) (Exchange exchange) (Topic topic)) = intercalate " " ["untap", queue, exchange, topic]
  show (SourceDrain (Queue queue) sink) = intercalate " " ["drain", queue, show sink]
  
parseCommand :: [String] -> Maybe Command
parseCommand ["add"] =
  Just SourceAdd
parseCommand ["drop", queue] =
  Just $ SourceDrop (Queue queue)
parseCommand ["tap", queue, exchange, topic] = 
  Just $ SourceTap (Queue queue) (Exchange exchange) (Topic topic)
parseCommand ["untap", queue, exchange, topic] = 
  Just $ SourceUntap (Queue queue) (Exchange exchange) (Topic topic)
parseCommand ["drain", queue, sink] =
  Just $ SourceDrain (Queue queue) (mkSink sink)
parseCommand _ = Nothing

repl :: String -> ([String] -> IO String) -> IO ()
repl prompt act = forever $ do
  input <- RL.readline prompt
  case input of
    Nothing     -> exit 
    Just "exit" -> exit
    Just line   -> do RL.addHistory line
                      result <- act $ words line
                      putStrLn result
  where exit    = fail "Exiting."

data Result = ResultOK | ResultUnknown deriving (Show)

execCommand :: Maybe Command -> IO Result
execCommand (Just cmd) = return ResultOK
execCommand Nothing    = return ResultUnknown

actions :: [String] -> IO String
actions cmd = show <$> (execCommand . parseCommand) cmd

main :: IO ()
main = repl "> " actions
