module RMQDriver.Commands ( Command(..)
                        , parseCommand
                        ) where

import RMQDriver.Types
import RMQDriver.Sinks
import Data.List (intercalate)
import Data.Monoid

data Command = SourceAdd Queue
             | SourceList
             | SourceDrop Queue
             | SourceBind Queue Exchange Topic
             | SourceUnbind Queue Exchange Topic
             | SourceDrain Queue Sink
             | SourceTail Queue Sink
             | MultiCommand Command Command
             | NoCommand

instance Monoid Command where
  mempty = NoCommand
  l `mappend` r = MultiCommand l r

cmd :: [String] -> String
cmd = intercalate " "

instance Show Command where
  show (SourceAdd (Queue queue)) = 
    cmd ["add", queue]
  show (SourceList) = 
    cmd ["list"]
  show (SourceDrop (Queue queue)) = 
    cmd ["drop", queue] 
  show (SourceBind (Queue queue) (Exchange exchange) (Topic topic)) =
    cmd ["bind", queue, exchange, topic]
  show (SourceUnbind (Queue queue) (Exchange exchange) (Topic topic)) = 
    cmd ["unbind", queue, exchange, topic]
  show (SourceDrain (Queue queue) sink) = 
    cmd ["drain", queue, show sink]
  show (SourceTail (Queue queue) sink) = 
    cmd ["tail", queue, show sink]
  show (MultiCommand l r) = show l ++ " ; " ++ show r
  show (NoCommand) = "pass"

parseCommand :: [String] -> Maybe Command
parseCommand ["add", queue] =
  Just $ SourceAdd (Queue queue)
parseCommand ["list"] =
  Just SourceList
parseCommand ["drop", queue] =
  Just $ SourceDrop (Queue queue)
parseCommand ["bind", queue, exchange, topic] = 
  Just $ SourceBind (Queue queue) (Exchange exchange) (Topic topic)
parseCommand ["unbind", queue, exchange, topic] = 
  Just $ SourceUnbind (Queue queue) (Exchange exchange) (Topic topic)
parseCommand ["drain", queue, sink] =
  Just $ SourceDrain (Queue queue) (mkSink sink)
parseCommand ["tail", queue, sink] =
  Just $ SourceTail (Queue queue) (mkSink sink)
parseCommand ["tap", queue, exchange, topic, sink] =
  let q   = Queue queue
      ex  = Exchange exchange
      top = Topic topic
      sk  = mkSink sink
  in Just $ SourceAdd q <> SourceBind q ex top <> SourceTail q sk 

parseCommand _ = Nothing
