{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
import Protolude
import Core


class HasSubscriptionsList env s | env -> s where
    hslGetAllSubscriptions :: env -> IO [s]

instance (HasSubscriptionsList env s, MonadReader env m, MonadIO m) => MonadSubscriptions env s m where
    type TraversableHolder m = []
    getAllSubscriptions :: m [s]
    getAllSubscriptions = do
        e <- ask
        liftIO $ hslGetAllSubscriptions e