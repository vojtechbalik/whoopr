{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
module Whoopr.Basic.RIOMonadListSubscriptions where

import Protolude
import Whoopr.Subscribers

class HasSubscriptionsList env s | env -> s where
    hslGetAllSubscriptions :: env -> MVar [s]

-- UndecidableInstances is needed here.
instance (HasSubscriptionsList env s, MonadReader env m, MonadIO m, HasId s, Integral (IdType s)) => MonadSubscriptions s m where
    type TraversableHolder m = []

    getAllSubscriptions :: m [s]
    getAllSubscriptions = do
        e <- ask
        liftIO . readMVar . hslGetAllSubscriptions $ e

    getSubscription :: IdType s -> m (Maybe s)
    getSubscription id = do
        ss <- liftIO . readMVar . hslGetAllSubscriptions =<< ask
        return $ find (\s -> getId s == id) ss

    createSubscription :: s -> m s
    createSubscription s = do
        ss <- liftIO . takeMVar . hslGetAllSubscriptions =<< ask
        let freeId = 1 + getId (maximumBy (compare `on` getId) ss)
            s' = setId s freeId
            ss' = s':ss
        liftIO . (`putMVar` ss') .hslGetAllSubscriptions =<< ask
        return s'

    deleteSubscription :: IdType s -> m (Maybe s)
    deleteSubscription id = do
        ss <- liftIO . takeMVar . hslGetAllSubscriptions =<< ask
        let (removed, ss') = foldr
                (\s (r, acc) -> if getId s == id then (Just s, acc) else (Nothing, s:acc))
                (Nothing, [])
                ss
        return removed