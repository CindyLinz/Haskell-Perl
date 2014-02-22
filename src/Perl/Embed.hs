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
import Control.Monad.IO.Class
import Foreign.C.String
import Foreign.Ptr

import Perl.Type
import Perl.Monad
import Perl.FromSV
import Perl.ToSV
import qualified Perl.MonadGlue as G

class VarName name where
  withVarName :: MonadIO m => name -> (forall s0. CStringLen -> PerlT s0 IO a) -> PerlT s m a

instance VarName CStringLen where
  withVarName name act = perlWithAnyIO ($ name) act

instance VarName String where
  withVarName name act = perlWithAnyIO (withCStringLen name) act

readFindSV :: (VarName name, FromSV a, MonadIO m) => name -> PerlT s m a
readFindSV name = withVarName name $ \cName -> do
  sv <- G.findSV cName
  if sv == nullPtr
    then fromSVNon
    else fromSV sv

writeFindSV :: (VarName name, ToSV a, MonadIO m) => name -> a -> PerlT s m ()
writeFindSV name val = withVarName name $ \cName -> do
  sv <- G.findSV cName
  when (sv /= nullPtr) $ setSV sv val

findSV :: (VarName name, MonadIO m) => name -> PerlT s m SV
findSV name = withVarName name $ \cName -> G.findSV cName

safeFindSV :: (VarName name, MonadIO m) => name -> PerlT s m (Maybe SV)
safeFindSV name = withVarName name $ \cName -> G.safeFindSV cName

findAV :: (VarName name, MonadIO m) => name -> PerlT s m AV
findAV name = withVarName name $ \cName -> G.findAV cName

safeFindAV :: (VarName name, MonadIO m) => name -> PerlT s m (Maybe AV)
safeFindAV name = withVarName name $ \cName -> G.safeFindAV cName

findHV :: (VarName name, MonadIO m) => name -> PerlT s m HV
findHV name = withVarName name $ \cName -> G.findHV cName

safeFindHV :: (VarName name, MonadIO m) => name -> PerlT s m (Maybe HV)
safeFindHV name = withVarName name $ \cName -> G.safeFindHV cName
