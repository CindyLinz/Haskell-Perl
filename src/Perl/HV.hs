{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, Rank2Types #-}
module Perl.HV
  ( readHV
  , writeHV
  , deleteHV
  , deleteHV_
  , HashKey (..)
  , fetchHV
  , globalHV
  , peekGlobalHV
  -- from Perl.MonadGlue
  , G.newHVEmpty
  , G.clearHV
  , G.peekHV
  , G.existsHV
  , G.storeHV
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class

import Foreign.Ptr
import Foreign.C.String

import Perl.Type
import Perl.Monad
import qualified Perl.Internal.MonadGlue as G
import Perl.SV

peekGlobalHV :: (MonadCatch m, MonadIO m) => String -> PerlT s m (Maybe HV)
peekGlobalHV name = do
  hv <- perlWithAnyIO (withCStringLen name) (flip G.getHV 0)
  return $ if hv == nullPtr
    then Nothing
    else Just hv

globalHV :: (MonadCatch m, MonadIO m) => String -> PerlT s m HV
globalHV name = perlWithAnyIO (withCStringLen name) (flip G.getHV 1)

class HashKey k where
  withHashKey :: MonadIO m => k -> (forall s0. CStringLen -> PerlT s0 IO a) -> PerlT s m a

instance HashKey CStringLen where
  withHashKey key act = perlWithAnyIO ($ key) act

instance HashKey String where
  withHashKey key act = perlWithAnyIO (withCStringLen key) act

readHV :: (HashKey k, FromSV a, MonadIO m) => HV -> k -> PerlT s m a
readHV hv key = withHashKey key $ \k -> do
  res <- G.peekHV hv k
  case res of
    Nothing -> fromSVNon
    Just a -> fromSV a

writeHV :: (HashKey k, ToSV a, MonadIO m) => HV -> k -> a -> PerlT s m ()
writeHV hv key val = withHashKey key $ \k -> do
  sv <- toSV val
  G.storeHV hv k sv

deleteHV :: (HashKey k, FromSV a, MonadIO m) => HV -> k -> PerlT s m a
deleteHV hv key = withHashKey key $ \k -> do
  res <- G.deleteHV hv k
  case res of
    Nothing -> fromSVNon
    Just a -> fromSV a

deleteHV_ :: (HashKey k, MonadIO m) => HV -> k -> PerlT s m ()
deleteHV_ hv key = withHashKey key $ \k -> do
  G.deleteHV hv k
  return ()

fetchHV :: (HashKey k, MonadIO m) => HV -> k -> PerlT s m (Maybe SV)
fetchHV hv key = withHashKey key $ \k -> G.fetchHV hv k
