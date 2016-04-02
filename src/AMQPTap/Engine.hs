{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module AMQPTap.Engine where

import AMQPTap.Types
import AMQPTap.Commands
import Data.List (intercalate)
import Control.Exception.Base (catch)
import Control.Concurrent (threadDelay)
import System.Posix.Signals (installHandler, Handler(CatchOnce), sigINT, sigTERM)
import Control.Concurrent.MVar (modifyMVar_, newMVar, withMVar, MVar)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Network.AMQP as AM

data EngineResult = EngineResult { engine :: Engine
                                 , command :: Command
                                 , status :: Status
                                 }

amQueueName :: Queue -> T.Text
amQueueName (Queue name) = T.pack $ "amqptap-" ++ name

amExchangeName :: Exchange -> T.Text
amExchangeName (Exchange name) = T.pack $ name

amTopicName :: Topic -> T.Text
amTopicName (Topic name) = T.pack $ name

connectEngine :: String -> IO Engine
connectEngine amqpUri = do
  connection <- AM.openConnection'' (AM.fromURI amqpUri)
  channel    <- AM.openChannel connection
  let queues = Set.empty
  return $ Engine{..}

sourceAdd :: Engine -> Queue -> IO (Engine, Status)
sourceAdd engine queue = do
  let engine'   = engine { queues = Set.insert queue $ queues engine }
      queueOpts = AM.newQueue { AM.queueName       = amQueueName queue
                              , AM.queueAutoDelete = True
                              , AM.queueExclusive  = True 
                              }
  (_, _, _) <- AM.declareQueue (channel engine) queueOpts
  return (engine', OK "queue added")

sourceList :: Engine -> IO (Engine, Status)
sourceList engine = do
  return (engine, OK (intercalate "\n" $ fmap show $ Set.toList $ queues engine))

sourceDrop :: Engine -> Queue -> IO (Engine, Status)
sourceDrop engine queue = do
  let status  = if Set.member queue (queues engine) then OK "deleted" else Failure "no such queue"
      engine' = engine { queues = Set.delete queue $ queues engine }
  _ <- AM.deleteQueue (channel engine) (amQueueName queue)
  return (engine', status)

sourceBind :: Engine -> Queue -> Exchange -> Topic -> IO (Engine, Status)
sourceBind engine queue exchange topic = do
  AM.bindQueue (channel engine) (amQueueName queue) (amExchangeName exchange) (amTopicName topic)
  return (engine, OK "bound")

sourceUnbind :: Engine -> Queue -> Exchange -> Topic -> IO (Engine, Status)
sourceUnbind engine queue exchange topic = do
  AM.unbindQueue (channel engine) (amQueueName queue) (amExchangeName exchange) (amTopicName topic)
  return (engine, OK "unbound")

sourceDrain :: Engine -> Queue -> Sink -> IO (Engine, Status)
sourceDrain engine queue (SinkHandler handler) = do
  msgTuple <- AM.getMsg (channel engine) AM.NoAck (amQueueName queue)
  case msgTuple of
    Just (msg, env) -> do handler (msg, env)
                          sourceDrain engine queue (SinkHandler handler)
    Nothing         -> return (engine, OK "drained")

sourceTail :: Engine -> Queue -> Sink -> IO (Engine, Status)
sourceTail engine queue (SinkHandler handler) = do
  intFlag <- newMVar 0
  installSignalHandlers intFlag
  go intFlag
  where go :: MVar Int -> IO (Engine, Status)
        go intFlag = do msgTuple <- AM.getMsg (channel engine) AM.NoAck (amQueueName queue)
                        case msgTuple of
                          Just (msg, env) -> handler (msg, env)
                          Nothing         -> threadDelay 200000 -- 200ms
                        tryInterrupt intFlag $ sourceTail engine queue (SinkHandler handler)
        
        handleInterrupt :: MVar Int -> IO ()
        handleInterrupt intFlag       = modifyMVar_ intFlag (return . (+1))

        installSignalHandlers :: MVar Int -> IO () 
        installSignalHandlers intFlag = do _ <- installHandler sigINT (CatchOnce $ handleInterrupt intFlag) Nothing
                                           _ <- installHandler sigTERM (CatchOnce $ handleInterrupt intFlag) Nothing
                                           return ()

        tryInterrupt :: MVar Int -> IO (Engine, Status) -> IO (Engine, Status)
        tryInterrupt intFlag nextAct  = withMVar intFlag (testInterrupt nextAct)
       
        testInterrupt :: IO (Engine, Status) -> Int -> IO (Engine, Status)
        testInterrupt nextAct 0 = nextAct
        testInterrupt _       _ = return (engine, OK "interrupted")

handleAMError :: Engine -> AM.AMQPException -> IO (Engine, Status)
handleAMError engine err = do
  AM.closeConnection $ connection engine
  newEngine <- connectEngine $ amqpUri engine
  return (newEngine, NetworkError $ show err)

execCommand :: EngineResult -> Maybe Command -> IO EngineResult
execCommand (EngineResult e c _) Nothing          = return $ EngineResult e c Unknown
execCommand result (Just (MultiCommand l r)) =
  execCommand result (Just l) >>= (flip execCommand) (Just r)
execCommand (EngineResult eng _ _) (Just command) = do
  (engine, status) <- catch (doExec command) (handleAMError eng)
  return $ EngineResult{..}
  where doExec (SourceAdd q)        = sourceAdd eng q
        doExec (SourceList)         = sourceList eng
        doExec (SourceDrop q)       = sourceDrop eng q
        doExec (SourceBind q e t)   = sourceBind eng q e t
        doExec (SourceUnbind q e t) = sourceUnbind eng q e t
        doExec (SourceDrain q s)    = sourceDrain eng q s
        doExec (SourceTail q s)     = sourceTail eng q s
        doExec _                    = return (eng, Failure "not implemented")

engineStartState :: Engine -> EngineResult
engineStartState engine = EngineResult engine NoCommand (OK "ready")
