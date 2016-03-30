module AMQPTap.JSON ( message
                    , envelope 
                    , messageWithEnvelope
                    ) where

import Text.JSON
import Network.AMQP 
import qualified Network.AMQP.Types as AMT

data MaybeNull a = NotNull a | Null

nullable :: Maybe a -> MaybeNull a
nullable (Just x) = NotNull x
nullable Nothing  = Null

instance (JSON a) => JSON (MaybeNull a) where
  readJSON o = nullable <$> readJSON o
  showJSON (NotNull x) = showJSON x
  showJSON Null        = JSNull

instance JSON AMT.FieldTable where
  readJSON o = AMT.FieldTable <$> readJSON o
  showJSON (AMT.FieldTable tbl) = showJSON tbl

instance JSON AMT.FieldValue where
  readJSON o = AMT.FVString <$> readJSON o -- FIXME
  showJSON (AMT.FVBool b) = showJSON b
  showJSON (AMT.FVInt8 i) = showJSON i
  showJSON (AMT.FVInt16 i) = showJSON i
  showJSON (AMT.FVInt32 i) = showJSON i
  showJSON (AMT.FVInt64 i) = showJSON i
  showJSON (AMT.FVFloat f) = showJSON f
  showJSON (AMT.FVDouble f) = showJSON f
  showJSON (AMT.FVDecimal _) = JSNull -- FIXME
  showJSON (AMT.FVString t) = showJSON t
  showJSON (AMT.FVFieldArray a) = showJSON a
  showJSON (AMT.FVFieldTable tbl) = showJSON tbl
  showJSON (AMT.FVVoid) = JSNull
  showJSON (AMT.FVByteArray bytes) = showJSON bytes
  showJSON (AMT.FVTimestamp ts) = showJSON ts

-- FIXME in general
instance JSON DeliveryMode where
  readJSON o = Ok NonPersistent -- FIXME
  showJSON = showJSON . show

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
                      , ( "timestamp", optional $ msgTimestamp m )
                      , ( "delivery_mode", optional $ msgDeliveryMode m )
                      , ( "headers", optional $ msgHeaders m )
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
