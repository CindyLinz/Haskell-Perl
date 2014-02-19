{-# LANGUAGE Rank2Types, TypeSynonymInstances, FlexibleInstances #-}
module Perl.Embed
  ( readMySV
  , writeMySV
  , mySV
  , safeMySV
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

readMySV :: (VarName name, FromSV a, MonadIO m) => name -> PerlT s m a
readMySV name = withVarName name $ \cName -> do
  sv <- G.mySV cName
  if sv == nullPtr
    then fromSVNon
    else fromSV sv

writeMySV :: (VarName name, ToSV a, MonadIO m) => name -> a -> PerlT s m ()
writeMySV name val = withVarName name $ \cName -> do
  sv <- G.mySV cName
  when (sv /= nullPtr) $ setSV sv val

mySV :: (VarName name, MonadIO m) => name -> PerlT s m SV
mySV name = withVarName name $ \cName -> G.mySV cName

safeMySV :: (VarName name, MonadIO m) => name -> PerlT s m (Maybe SV)
safeMySV name = withVarName name $ \cName -> G.safeMySV cName
