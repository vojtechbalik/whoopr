{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Whoopr.Subscribers where

import Protolude
import Network.URI (URI)


class (Monad m, Traversable (TraversableHolder m), HasId s) => MonadSubscriptions s m | m -> s where
    type TraversableHolder m :: * -> *

    getAllSubscriptions :: m (TraversableHolder m s)
    getSubscription :: IdType s -> m (Maybe s)
    createSubscription :: s -> m s
    deleteSubscription :: IdType s -> m (Maybe s)

class (MonadSubscriptions s m) => MonadSubscriptionsFilter s m fd where
    filterSubscriptions :: fd -> m [s]

class HmacSigner s where
    getSharedSecret :: s -> ByteString

class HasEndpointURI s where
    getEndpointURI :: s -> URI

class HasId a where
    type IdType a :: *
    getId :: a -> IdType a
    setId :: a -> IdType a -> a