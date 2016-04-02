module AMQPTap.Types where

import qualified Network.AMQP as AM
import qualified Data.Set as Set

newtype Queue    = Queue String deriving (Show, Eq, Ord)
newtype Exchange = Exchange String deriving (Show, Eq, Ord)
newtype Topic    = Topic String deriving (Show, Eq, Ord)

data Sink = SinkHandler ((AM.Message, AM.Envelope) -> IO ())

instance Show Sink where
  show (SinkHandler _) = "default"

data Engine = Engine { amqpUri     :: String
                     , connection  :: AM.Connection
                     , channel     :: AM.Channel
                     , queuePrefix :: String
                     , queues      :: Set.Set Queue 
                     }

data Status = OK String
            | Failure String
            | NetworkError String
            | Unknown 

instance Show Status where
  show (OK msg)      = msg
  show (Failure msg) = "Failure: " ++ msg
  show (NetworkError msg) = "Network error: " ++ msg
  show (Unknown)     = "Unknown command"
