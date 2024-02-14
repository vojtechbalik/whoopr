{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Whoopr.Basic.SubscriptionAPI where

import Protolude
import Servant.API
import Servant
import Network.URI
import Control.Monad.Base
import Data.Aeson.TH (deriveToJSON, defaultOptions, deriveFromJSON)
import qualified Data.Text as T
import Whoopr.Basic.Subscription
import Whoopr.Subscribers
import Data.Aeson (ToJSON)
import Data.Aeson.Types ( ToJSON(..), object, (.=) )

type WebhookAPI = ReqBody '[JSON] ExtSubscription :> PostNoContent
            :<|>                                       Get '[JSON] [BasicSubscription]
            :<|>  Capture "userid" Int              :> Get '[JSON] BasicSubscription
            :<|>  Capture "userid" Int              :> DeleteNoContent

-- Encoding ByteStrings to JSON is not straightforward;
--   JSON strings should be UTF8, but we know our values are ASCII.
instance ToJSON BasicSubscription where
    toJSON BasicSubscription{..} = object [
        "id" .= id,
        "endpointURI" .= endpointURI,
        "eventType" .= decodeUtf8 eventType,
        "sharedSecret" .= decodeUtf8 sharedSecret]

data ExtSubscription = ExtSubscription {
    endpointUrl :: Text,
    sharedSecret :: Text,
    eventType :: Text
} deriving (Show, Generic)

deriveFromJSON defaultOptions 'ExtSubscription

webhookApiProxy :: Proxy WebhookAPI
webhookApiProxy = Proxy

webhookApiServer :: (MonadSubscriptions BasicSubscription m, MonadError ServerError m) => (forall a . m a -> Servant.Handler a) -> Server WebhookAPI
webhookApiServer f = hoistServer webhookApiProxy f webhookApiServerImpl

webhookApiServerImpl :: (MonadSubscriptions BasicSubscription m, MonadError ServerError m) => ServerT WebhookAPI m
webhookApiServerImpl = createSubscr :<|> getSubscrs :<|> getSubscr :<|> deleteSubscr
    where
        createSubscr ExtSubscription{..} = do
            uri <- case parseEndpointUrl endpointUrl of
                        Left msg -> throwError $ err400 { errReasonPhrase = T.unpack msg }
                        Right u -> return u
            sharedSecretBS <- convertToAsciiBSOrThrow sharedSecret
            eventTypeBS <- convertToAsciiBSOrThrow eventType
            createSubscription BasicSubscription {
                id = 0,
                endpointURI = uri,
                eventType = eventTypeBS,
                sharedSecret = sharedSecretBS
            }
            return NoContent
        getSubscrs = toList <$> getAllSubscriptions
        getSubscr id = do
            maybeSubscr <- getSubscription id
            case maybeSubscr of
                Just s -> return s
                Nothing -> throwError err404
        deleteSubscr id = deleteSubscription id >> return NoContent


        convertToAsciiBSOrThrow txt = if T.all isAscii txt
                                        then return $ encodeUtf8 txt
                                        else throwError $ err400 { errReasonPhrase = "Non-ascii characters in secret"}



-- subscriptionsApp :: (HasWebhooks env) => env -> Application
-- subscriptionsApp env = serve webhookApiProxy (webhookApiServer env)

subscriptionsApp :: (MonadSubscriptions BasicSubscription m, MonadError ServerError m) => (forall a. m a -> Servant.Handler a) -> Application
subscriptionsApp f = serve webhookApiProxy (webhookApiServer f)

parseEndpointUrl :: Text -> Either Text URI
parseEndpointUrl t = case parseURI (T.unpack t) of
    Nothing  -> Left "Could not parse URI"
    Just uri -> Right uri

-- validateSubscription :: Subscription -> m Bool
-- validateSubscription s = _ -- TODO
