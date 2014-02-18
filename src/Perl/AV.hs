{-# LANGUAGE FlexibleInstances #-}
module Perl.AV
  ( ToAV (toAV)
  , FromAV (fromAV)
  , readAV
  , writeAV
  , pushAV
  , popAV
  , shiftAV
  , unshiftAV
  -- from MonadGlue
  , G.peekAV
  , G.fetchAV
  , G.storeAV
  , G.lengthAV
  , G.existsAV
  , G.clearAV
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Array.Storable
import Foreign.C.Types

import Perl.Type
import Perl.Monad
import qualified Perl.MonadGlue as G
import Perl.AsSV
import Perl.ToSV
import Perl.FromSV

class ToAV a where
  toAV :: MonadIO m => a -> PerlT s m AV

asSVToAV :: (AsSV a, MonadIO m) => [a] -> PerlT s m AV
asSVToAV as = do
  listSV <- mapM asSV as
  arr <- liftIO $ newListArray (1, length as) listSV
  G.newAV arr

toSVToAV :: (ToSV a, MonadIO m) => [a] -> PerlT s m AV
toSVToAV as = do
  listSV <- mapM toSV as
  arr <- liftIO $ newListArray (1, length as) listSV
  G.newAV arr

instance ToAV (StorableArray Int SV) where
  toAV = G.newAV

instance ToAV [AsSVObj] where toAV = asSVToAV
instance ToAV [SV] where toAV = asSVToAV
instance ToAV [RefSV] where toAV = asSVToAV
instance ToAV [RefAV] where toAV = asSVToAV
instance ToAV [RefHV] where toAV = asSVToAV
instance ToAV [RefCV] where toAV = asSVToAV

instance ToAV [Int] where toAV = toSVToAV
instance ToAV [Double] where toAV = toSVToAV
instance ToAV [String] where toAV = toSVToAV

class FromAV a where
  fromAV :: MonadIO m => AV -> PerlT s m a

fromSVFromAV :: (FromSV a, MonadIO m) => AV -> PerlT s m [a]
fromSVFromAV av = do
  len <- G.lengthAV av
  forM (take (fromIntegral len) [0..]) $ \i -> do
    res <- G.peekAV av i
    case res of
      Nothing -> fromSVNon
      Just a -> fromSV a

instance FromAV [SV] where fromAV = fromSVFromAV
instance FromAV [RefSV] where fromAV = fromSVFromAV
instance FromAV [RefAV] where fromAV = fromSVFromAV
instance FromAV [RefHV] where fromAV = fromSVFromAV
instance FromAV [RefCV] where fromAV = fromSVFromAV

instance FromAV [Int] where fromAV = fromSVFromAV
instance FromAV [Double] where fromAV = fromSVFromAV
instance FromAV [String] where fromAV = fromSVFromAV

readAV :: (FromSV a, MonadIO m) => AV -> CInt -> PerlT s m a
readAV av i = do
  res <- G.peekAV av i
  case res of
    Nothing -> fromSVNon
    Just a -> fromSV a

writeAV :: (ToSV a, MonadIO m) => AV -> CInt -> a -> PerlT s m ()
writeAV av i v = do
  sv <- toSV v
  G.storeAV av i sv

pushAV :: (ToSV a, MonadIO m) => AV -> a -> PerlT s m ()
pushAV av v = do
  sv <- toSV v
  G.pushAV av sv

unshiftAV :: (ToSV a, MonadIO m) => AV -> a -> PerlT s m ()
unshiftAV av v = do
  sv <- toSV v
  G.unshiftAV av sv

popAV :: (FromSV a, MonadIO m) => AV -> PerlT s m a
popAV av = do
  sv <- G.popAV av
  fromSV sv

shiftAV :: (FromSV a, MonadIO m) => AV -> PerlT s m a
shiftAV av = do
  sv <- G.shiftAV av
  fromSV sv
