module AMQPTap.Commands where

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

instance Show Command where
  show (SourceAdd (Queue queue)) = intercalate " " ["add", queue]
  show (SourceList) = "list"
  show (SourceDrop (Queue queue)) = intercalate " " ["drop", queue] 
  show (SourceBind (Queue queue) (Exchange exchange) (Topic topic)) = intercalate " " ["bind", queue, exchange, topic]
  show (SourceUnbind (Queue queue) (Exchange exchange) (Topic topic)) = intercalate " " ["unbind", queue, exchange, topic]
  show (SourceDrain (Queue queue) sink) = intercalate " " ["drain", queue, show sink]
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
