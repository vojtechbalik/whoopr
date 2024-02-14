{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Whoopr.ToByteString where

import qualified Data.ByteString as SBS
import Data.Aeson (ToJSON, encode)
import Protolude

class ToByteString a where
    convertToByteString :: a -> SBS.ByteString

instance ToByteString SBS.ByteString where
    convertToByteString = identity

-- UndecidableInstances needed here. Here it is OK as long as there isn't a circular reference, 
--  e.g. `instance ToByteString a => ToJSON a` must not exist.
instance ToJSON a => ToByteString a where
    convertToByteString = SBS.toStrict . encode