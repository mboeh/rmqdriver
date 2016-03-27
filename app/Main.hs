{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Main where

import Data.List (intercalate)
import qualified System.Console.Readline as RL
import qualified Network.AMQP as AM
import qualified Data.Set as Set
import qualified Data.Text as T

-- Basic entities to operate on

newtype Queue    = Queue String deriving (Show, Eq, Ord)

amQueueName :: Queue -> T.Text
amQueueName (Queue name) = T.pack $ "amqptap-" ++ name

newtype Exchange = Exchange String deriving (Show, Eq, Ord)
newtype Topic    = Topic String deriving (Show, Eq, Ord)

-- Sinks: recipients of tapped messages

data Sink    = SinkHandler ((AM.Message, AM.Envelope) -> IO ())

defaultSink :: Sink
defaultSink = SinkHandler handler
  where handler (_msg, env) = AM.ackEnv env

instance Show Sink where
  show (SinkHandler _) = "default"

mkSink :: String -> Sink
mkSink _ = defaultSink

-- Commands

data Command = SourceAdd Queue
             | SourceList
             | SourceDrop Queue
             | SourceTap Queue Exchange Topic
             | SourceUntap Queue Exchange Topic
             | SourceDrain Queue Sink
             | NoCommand

instance Show Command where
  show (SourceAdd (Queue queue)) = intercalate " " ["add", queue]
  show (SourceList) = "list"
  show (SourceDrop (Queue queue)) = intercalate " " ["drop", queue] 
  show (SourceTap (Queue queue) (Exchange exchange) (Topic topic)) = intercalate " " ["tap", queue, exchange, topic]
  show (SourceUntap (Queue queue) (Exchange exchange) (Topic topic)) = intercalate " " ["untap", queue, exchange, topic]
  show (SourceDrain (Queue queue) sink) = intercalate " " ["drain", queue, show sink]
  show (NoCommand) = "pass"

data Engine = Engine { connection :: AM.Connection
                     , channel    :: AM.Channel
                     , queues     :: Set.Set Queue 
                     }

data Status = OK String
            | Failure String
            | Unknown 

instance Show Status where
  show (OK msg)      = msg
  show (Failure msg) = "Failure: " ++ msg
  show (Unknown)     = "Unknown command"

data EngineResult = EngineResult { engine :: Engine
                                 , command :: Command
                                 , status :: Status
                                 }

connectEngine :: String -> IO Engine
connectEngine uri = do
  connection <- AM.openConnection'' (AM.fromURI uri)
  channel <- AM.openChannel connection
  let queues = Set.empty
  return $ Engine{..}

sourceAdd :: Engine -> Queue -> IO (Engine, Status)
sourceAdd engine queue = do
  let engine' = engine { queues = Set.insert queue $ queues engine }
      queueOpts = AM.newQueue { AM.queueName       = amQueueName queue
                              , AM.queueAutoDelete = True
                              , AM.queueExclusive  = True 
                              }
  (_, _, _) <- AM.declareQueue (channel engine) queueOpts
  return (engine', OK "queue added")

sourceList :: Engine -> IO (Engine, Status)
sourceList engine = do
  return (engine, OK (intercalate "\n" $ fmap show $ Set.toList $ queues engine))

sourceDrop :: Engine -> Queue -> IO (Engine, Status)
sourceDrop engine queue = do
  let status = if Set.member queue (queues engine) then OK "deleted" else Failure "no such queue"
      engine' = engine { queues = Set.delete queue $ queues engine }
  _ <- AM.deleteQueue (channel engine) (amQueueName queue)
  return (engine', status)

execCommand :: EngineResult -> Maybe Command -> IO EngineResult
execCommand (EngineResult e c _) Nothing    = return $ EngineResult e c Unknown
execCommand (EngineResult eng _ _) (Just command) = do
  (engine, status) <- doExec command
  return $ EngineResult{..}
  where doExec (SourceAdd q)  = sourceAdd eng q
        doExec (SourceList)   = sourceList eng
        doExec (SourceDrop q) = sourceDrop eng q
        doExec _              = return (eng, Failure "not implemented")
  
parseCommand :: [String] -> Maybe Command
parseCommand ["add", queue] =
  Just $ SourceAdd (Queue queue)
parseCommand ["list"] =
  Just SourceList
parseCommand ["drop", queue] =
  Just $ SourceDrop (Queue queue)
parseCommand ["tap", queue, exchange, topic] = 
  Just $ SourceTap (Queue queue) (Exchange exchange) (Topic topic)
parseCommand ["untap", queue, exchange, topic] = 
  Just $ SourceUntap (Queue queue) (Exchange exchange) (Topic topic)
parseCommand ["drain", queue, sink] =
  Just $ SourceDrain (Queue queue) (mkSink sink)
parseCommand _ = Nothing

-- Command interpreter

repl :: String -> EngineResult -> IO ()
repl prompt state = do
  input <- RL.readline prompt
  case input of
    Nothing     -> exit 
    Just "exit" -> exit
    Just line   -> do RL.addHistory line
                      newState <- execCommand state $ parseCommand $ words line
                      putStrLn $ show $ status $ newState
                      repl prompt newState 
  where exit    = fail "Exiting."

main :: IO ()
main = do
  engine <- connectEngine "amqp://guest:guest@localhost"
  repl "> " (EngineResult engine NoCommand (OK "ready"))
