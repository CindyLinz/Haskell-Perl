{-# LANGUAGE Rank2Types, TypeSynonymInstances, FlexibleInstances #-}
module Perl.Embed
  ( readFindSV
  , writeFindSV
  , findSV
  , safeFindSV
  , findAV
  , safeFindAV
  , findHV
  , safeFindHV
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Foreign.C.String
import Foreign.Ptr

import Perl.Type
import Perl.Monad
import Perl.SV
import qualified Perl.Internal.MonadGlue as G
import Perl.Internal.Util

class VarName name where
  withVarName :: MonadIO m => name -> (forall s0. CStringLen -> PerlT s0 IO a) -> PerlT s m a

instance VarName CStringLen where
  withVarName name act = perlWithAnyIO ($ name) act

instance VarName String where
  withVarName name act = perlWithAnyIO (withCStringLen name) act

readFindSV :: (VarName name, FromSV a, MonadCatch m, MonadIO m) => name -> PerlT s m a
readFindSV name = do
  sv <- findSV name
  if sv == nullPtr
    then fromSVNon
    else fromSV sv

writeFindSV :: (VarName name, ToSV a, MonadCatch m, MonadIO m) => name -> a -> PerlT s m ()
writeFindSV name val = do
  sv <- findSV name
  when (sv /= nullPtr) $ setSV sv val

findSV :: (VarName name, MonadIO m) => name -> PerlT s m SV
findSV name = withVarName name $ \cName@(cNamePtr , cNameLen) -> do
  sv <- G.findSV cName
  if sv == nullPtr
    then G.getSV (plusPtr cNamePtr 1, cNameLen - 1) 0
    else return sv

safeFindSV :: (VarName name, MonadCatch m, MonadIO m) => name -> PerlT s m (Maybe SV)
safeFindSV name = findSV name >>= return . liftNull

findAV :: (VarName name, MonadIO m) => name -> PerlT s m AV
findAV name = withVarName name $ \cName@(cNamePtr, cNameLen) -> do
  av <- G.findAV cName
  if av == nullPtr
    then G.getAV (plusPtr cNamePtr 1, cNameLen - 1) 0
    else return av

safeFindAV :: (VarName name, MonadCatch m, MonadIO m) => name -> PerlT s m (Maybe AV)
safeFindAV name = findAV name >>= return . liftNull

findHV :: (VarName name, MonadIO m) => name -> PerlT s m HV
findHV name = withVarName name $ \cName@(cNamePtr, cNameLen) -> do
  hv <- G.findHV cName
  if hv == nullPtr
    then G.getHV (plusPtr cNamePtr 1, cNameLen - 1) 0
    else return hv

safeFindHV :: (VarName name, MonadCatch m, MonadIO m) => name -> PerlT s m (Maybe HV)
safeFindHV name = findHV name >>= return . liftNull

findCV :: (VarName name, MonadIO m) => name -> PerlT s m CV
findCV name = withVarName name $ \cName@(cNamePtr, cNameLen) ->
  G.getCV (plusPtr cNamePtr 1, cNameLen - 1) 0

safeFindCV :: (VarName name, MonadCatch m, MonadIO m) => name -> PerlT s m (Maybe CV)
safeFindCV name = findCV name >>= return . liftNull
