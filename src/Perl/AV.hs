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
  , globalAV
  , peekGlobalAV
  -- from MonadGlue
  , G.peekAV
  , G.fetchAV
  , G.storeAV
  , G.lengthAV
  , G.existsAV
  , G.clearAV
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Array.IArray

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import Perl.Type
import Perl.Monad
import qualified Perl.Internal.MonadGlue as G
import Perl.SV

peekGlobalAV :: (MonadCatch m, MonadIO m) => String -> PerlT s m (Maybe AV)
peekGlobalAV name = do
  av <- perlWithAnyIO (withCStringLen name) (flip G.getAV 0)
  return $ if av == nullPtr
    then Nothing
    else Just av

globalAV :: (MonadCatch m, MonadIO m) => String -> PerlT s m AV
globalAV name = perlWithAnyIO (withCStringLen name) (flip G.getAV 1)

class ToAV a where
  toAV :: (MonadCatch m, MonadIO m) => a -> PerlT s m AV

instance ToAV SVArray where
  toAV = G.newAV

instance ToSV a => ToAV [a] where
  toAV as = do
    listSV <- mapM toSV as
    G.newAV (listArray (1, length as) listSV)

class FromAV a where
  fromAV :: (MonadCatch m, MonadIO m) => AV -> PerlT s m a

fromSVFromAV :: (FromSV a, MonadCatch m, MonadIO m) => AV -> PerlT s m [a]
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

readAV :: (FromSV a, MonadCatch m, MonadIO m) => AV -> CInt -> PerlT s m a
readAV av i = do
  res <- G.peekAV av i
  case res of
    Nothing -> fromSVNon
    Just a -> fromSV a

writeAV :: (ToSV a, MonadCatch m, MonadIO m) => AV -> CInt -> a -> PerlT s m ()
writeAV av i v = do
  sv <- toSV v
  G.storeAV av i sv

pushAV :: (ToSV a, MonadCatch m, MonadIO m) => AV -> a -> PerlT s m ()
pushAV av v = do
  sv <- toSV v
  G.pushAV av sv

unshiftAV :: (ToSV a, MonadCatch m, MonadIO m) => AV -> a -> PerlT s m ()
unshiftAV av v = do
  sv <- toSV v
  G.unshiftAV av sv

popAV :: (FromSV a, MonadCatch m, MonadIO m) => AV -> PerlT s m a
popAV av = do
  sv <- G.popAV av
  fromSV sv

shiftAV :: (FromSV a, MonadCatch m, MonadIO m) => AV -> PerlT s m a
shiftAV av = do
  sv <- G.shiftAV av
  fromSV sv
