module AMQPTap.JSON ( message
                    , envelope 
                    , messageWithEnvelope
                    ) where

import Text.JSON
import Network.AMQP 

data MaybeNull a = NotNull a | Null

nullable :: Maybe a -> MaybeNull a
nullable (Just x) = NotNull x
nullable Nothing  = Null

instance (JSON a) => JSON (MaybeNull a) where
  readJSON o = nullable <$> (readJSON o :: JSON a => Result (Maybe a))
  showJSON (NotNull x) = showJSON x
  showJSON Null        = JSNull

optional :: JSON a => Maybe a -> JSValue
optional = showJSON . nullable

message :: Message -> JSValue
message m = encJSDict [ ( "body", showJSON $ msgBody m )
                      , ( "id", optional $ msgID m )
                      , ( "type", optional $ msgType m )
                      , ( "user_id", optional $ msgUserID m )
                      , ( "application_id", optional $ msgApplicationID m )
                      , ( "cluster_id", optional $ msgClusterID m )
                      , ( "content_type", optional $ msgContentType m )
                      , ( "content_encoding", optional $ msgContentEncoding m )
                      , ( "reply_to", optional $ msgReplyTo m )
                      , ( "correlation_id", optional $ msgCorrelationID m )
                      , ( "expiration", optional $ msgExpiration m )
                      ]

envelope :: Envelope -> JSValue
envelope e = encJSDict [ ( "routing_key", showJSON $ envRoutingKey e )
                       , ( "exchange_name", showJSON $ envExchangeName e )
                       , ( "redelivered", showJSON $ envRedelivered e )
                       ]

messageWithEnvelope :: (Message, Envelope) -> JSValue
messageWithEnvelope (m, e) = encJSDict [ ( "message", message m )
                                       , ( "envelope", envelope e )
                                       ]
