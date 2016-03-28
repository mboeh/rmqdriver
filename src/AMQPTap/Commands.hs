module AMQPTap.Commands ( Command(..)
                        , parseCommand
                        ) where

import AMQPTap.Types
import AMQPTap.Sinks
import Data.List (intercalate)

data Command = SourceAdd Queue
             | SourceList
             | SourceDrop Queue
             | SourceBind Queue Exchange Topic
             | SourceUnbind Queue Exchange Topic
             | SourceDrain Queue Sink
             | NoCommand

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
parseCommand _ = Nothing
