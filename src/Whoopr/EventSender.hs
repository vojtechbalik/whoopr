{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Whoopr.EventSender where

import Protolude
import Whoopr.TaskQueue
import Network.URI (URI)
import Data.Time (UTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Whoopr.ToByteString
import Network.HTTP.Simple (Header, getResponseStatusCode, httpLBS, setRequestMethod, setRequestBody)
import qualified Data.ByteString as SBS
import Whoopr.Subscribers
import Network.HTTP.Client.Conduit (HttpException, requestFromURI_)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toASCIIBytes)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Conduit (RequestBody(..))
import Data.ByteString.Char8 (singleton)

taskRecvHelper :: (TaskQueueConsumer cq (Task fd)) => Proxy fd -> cq -> IO (Task fd)
taskRecvHelper _ = tqRecv

taskSendHelper :: (TaskQueueProducer cq (Task fd)) => Proxy fd -> cq -> Task fd -> IO ()
taskSendHelper _ = tqSend

consumeTaskQueue :: (
        TaskQueueConsumer cq (Task fd),
        TaskQueueProducer pq (Task fd),
        HmacSigner s, HasEndpointURI s,
        MonadSubscriptionsFilter s m fd,
        MonadIO m
    ) => Proxy fd -> (m () -> IO ()) -> cq -> pq -> IO ()
consumeTaskQueue proxy runner cq pq = do
    task <- taskRecvHelper proxy cq
    liftIO . async $ case task of
        NotificationDeliveryTask nd -> handleNotificationDelivery proxy pq nd
        NotificationInitializationTask ni -> runner $ handleNotificationInitialization proxy pq ni
    return ()

data NotificationDelivery =
    NotificationDelivery {
        eventId :: ByteString,
        endpointURI :: URI,
        sharedSecret :: ByteString,
        eventData :: ByteString,
        createdAt :: UTCTime,
        retrySchedule :: [Int],
        attemptsSoFar :: Int
    }

data DynEventData where
    DynEventData :: ToByteString a => a -> DynEventData

data NotificationInitialization fd =
    NotificationInitialization {
        eventData :: DynEventData,
        filterDescriptor :: fd
    }

data Task fd = NotificationDeliveryTask NotificationDelivery
            |  NotificationInitializationTask (NotificationInitialization fd)

geometricProgression init quot = init:geometricProgression (init*quot) quot

defaultRetrySchedule :: [Int]
defaultRetrySchedule = take 10 $ geometricProgression 30 2

handleNotificationDelivery :: TaskQueueProducer tq (Task fd) => Proxy fd -> tq -> NotificationDelivery -> IO ()
handleNotificationDelivery proxy tq nd@NotificationDelivery{..} = do
    ok <- attemptEventDelivery nd
    unless (ok || null retrySchedule) $ do
        let (delay:remRetries) = retrySchedule
            nd' = nd{attemptsSoFar = attemptsSoFar + 1, retrySchedule = remRetries}
        async $ threadDelay (delay*1000) >> taskSendHelper proxy tq (NotificationDeliveryTask nd')
        return ()

handleNotificationInitialization :: (
        TaskQueueProducer pq (Task fd),
        HmacSigner s, HasEndpointURI s,
        MonadSubscriptionsFilter s m fd, 
        MonadIO m) => Proxy fd -> pq -> NotificationInitialization fd -> m ()
handleNotificationInitialization proxy pq NotificationInitialization{..} = do
    let eventDataByteString = case eventData of
                DynEventData e -> convertToByteString e
    ss <- filterSubscriptions filterDescriptor
    ss' <- liftIO $ mapM (\s -> makeNotificationDelivery s eventDataByteString) ss
    forM_ ss' $ liftIO . taskSendHelper proxy pq . NotificationDeliveryTask

makeNotificationDelivery :: (HmacSigner s, HasEndpointURI s) =>
        s              -- subscriber
    ->  SBS.ByteString -- event data
    ->  IO NotificationDelivery
makeNotificationDelivery s ed = do
    uuid <- nextRandom
    currentTime <- getCurrentTime
    return NotificationDelivery {
        eventId = toASCIIBytes uuid,
        endpointURI = getEndpointURI s,
        sharedSecret = getSharedSecret s,
        eventData = ed,
        createdAt = currentTime,
        retrySchedule = defaultRetrySchedule,
        attemptsSoFar = 0
    }

attemptEventDelivery :: NotificationDelivery -> IO Bool
attemptEventDelivery nd@NotificationDelivery{..} = do
    let req = setRequestMethod "POST"
                $ setRequestBody (RequestBodyBS eventData)
                -- not good, impure exceptions
                $ requestFromURI_ endpointURI
    -- careful, what about the headers that are already in the request by default?
    headers <- generateAttemptHeaders nd

    let handler e = do
            print e
            return False
    catch
        (do statusCode <- getResponseStatusCode <$> httpLBS req
            return (200 <= statusCode && statusCode < 300))
        (\e -> do print (e :: HttpException)
                  return False)

getSecondsSinceEpoch :: IO Int64
getSecondsSinceEpoch = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> getCurrentTime

generateAttemptHeaders :: NotificationDelivery -> IO [Header]
generateAttemptHeaders NotificationDelivery{..} = do
    let res = []
    timestamp <- show <$> getSecondsSinceEpoch

    let res = ("webhook-timestamp", timestamp):("webhook-id", eventId):res
    let toSign = SBS.intercalate (singleton '.') [eventId, timestamp, eventData]
    -- TODO run it through hashing algo using crypton, encode it in base64?
    return $ ("webhook-signature", toSign):res