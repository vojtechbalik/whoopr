{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Whoopr.TaskQueue where

import Protolude

class TaskQueueProducer q a where
    tqSend :: q -> a -> IO ()

class TaskQueueConsumer q a where
    tqRecv ::  q -> IO a

instance TaskQueueProducer (Chan a) a where
  tqSend = writeChan

instance TaskQueueConsumer (Chan a) a where
  tqRecv = readChan