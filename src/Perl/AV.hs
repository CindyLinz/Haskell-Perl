{-# LANGUAGE FlexibleInstances #-}
module Perl.AV
  where

import Control.Monad.IO.Class
import Data.Array.Storable

import Perl.Type
import Perl.Monad
import Perl.MonadGlue
import Perl.AsSV
import Perl.ToSV
import Perl.FromSV

class ToAV a where
  toAV :: MonadIO m => a -> PerlT s m AV

asSVToAV :: (AsSV a, MonadIO m) => [a] -> PerlT s m AV
asSVToAV as = do
  listSV <- mapM asSV as
  arr <- liftIO $ newListArray (1, length as) listSV
  newAV arr

toSVToAV :: (ToSV a, MonadIO m) => [a] -> PerlT s m AV
toSVToAV as = do
  listSV <- mapM toSV as
  arr <- liftIO $ newListArray (1, length as) listSV
  newAV arr

instance ToAV (StorableArray Int SV) where
  toAV = newAV

instance ToAV [AsSVObj] where toAV = asSVToAV
instance ToAV [SV] where toAV = asSVToAV
instance ToAV [CV] where toAV = asSVToAV
instance ToAV [RefSV] where toAV = asSVToAV
instance ToAV [RefAV] where toAV = asSVToAV
instance ToAV [RefHV] where toAV = asSVToAV
instance ToAV [RefCV] where toAV = asSVToAV

instance ToAV [Int] where toAV = toSVToAV
instance ToAV [Double] where toAV = toSVToAV
instance ToAV [String] where toAV = toSVToAV

class FromAV a where
  fromAV :: MonadIO m => AV -> PerlT s m a
