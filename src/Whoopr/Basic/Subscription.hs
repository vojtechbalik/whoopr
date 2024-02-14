{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Whoopr.Basic.Subscription where

import Protolude
import Network.URI
import Whoopr.Subscribers

data BasicSubscription = BasicSubscription {
    id :: Int,
    endpointURI :: URI,
    eventType :: ByteString,
    sharedSecret :: ByteString
}

instance MonadSubscriptions BasicSubscription m => MonadSubscriptionsFilter BasicSubscription m ByteString where
    filterSubscriptions :: ByteString -> m [BasicSubscription]
    filterSubscriptions needle = do
        ss <- getAllSubscriptions
        let filtered = foldr 
                (\s@BasicSubscription{eventType=hay} acc -> if needle == hay then s:acc else acc)
                []
                ss
        return filtered

instance HmacSigner BasicSubscription where
    getSharedSecret = sharedSecret

instance HasEndpointURI BasicSubscription where
    getEndpointURI = endpointURI

instance HasId BasicSubscription where
    type IdType BasicSubscription = Int
    getId = id  
    setId s id = s{id=id}