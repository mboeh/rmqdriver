module AMQPTap.Sinks where

import AMQPTap.Types
import Text.Printf (printf)
import qualified Network.AMQP as AM
import qualified Data.ByteString.Lazy.Char8 as BL

defaultSink :: Sink
defaultSink = SinkHandler handler
  where handler (msg, env) = do printf "%s" $ BL.unpack $ AM.msgBody msg
                                AM.ackEnv env

mkSink :: String -> Sink
mkSink _ = defaultSink
