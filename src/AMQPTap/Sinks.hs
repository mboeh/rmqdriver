module AMQPTap.Sinks where

import AMQPTap.Types
import Text.Printf (printf)
import qualified Network.AMQP as AM
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

defaultSink :: Sink
defaultSink = SinkHandler handler
  where handler (msg, env) = 
          printf "[%s] %s\n" routingKey body
          where body       = BL.unpack $ AM.msgBody msg
                routingKey = T.unpack $ AM.envRoutingKey env

mkSink :: String -> Sink
mkSink _ = defaultSink
