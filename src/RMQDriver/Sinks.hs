module RMQDriver.Sinks where

import RMQDriver.Types
import RMQDriver.JSON (messageWithEnvelope)
import qualified Text.JSON as JSON
import Text.Printf (printf)
import System.IO
import qualified Network.AMQP as AM
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

jsonSink :: Sink
jsonSink = SinkHandler handler
  where handler (msg, env) = do putStrLn . JSON.encodeStrict . messageWithEnvelope $ (msg, env)
                                hFlush stdout
textSink :: Sink
textSink = SinkHandler handler
  where handler (msg, env) = 
          printf "[%s] %s\n" routingKey body
          where body       = BL.unpack $ AM.msgBody msg
                routingKey = T.unpack $ AM.envRoutingKey env

defaultSink :: Sink
defaultSink = textSink

mkSink :: String -> Sink
mkSink "json" = jsonSink
mkSink "text" = textSink
mkSink _      = defaultSink
