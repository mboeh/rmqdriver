module AMQPTap.JSON ( message
                    , envelope 
                    , messageWithEnvelope
                    ) where

import Text.JSON
import Network.AMQP 

message :: Message -> JSValue
message m = encJSDict [ ( "body", showJSON $ msgBody m )
                      ]

envelope :: Envelope -> JSValue
envelope e = encJSDict [ ( "routingKey", envRoutingKey e )
                       , ( "exchangeName", envExchangeName e )
                       ]

messageWithEnvelope :: (Message, Envelope) -> JSValue
messageWithEnvelope (m, e) = encJSDict [ ( "message", message m )
                                       , ( "envelope", envelope e )
                                       ]
