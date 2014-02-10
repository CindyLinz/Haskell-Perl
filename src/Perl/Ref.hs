{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Perl.Ref
  where

import Control.Monad.IO.Class

import Perl.Type
import Perl.Monad
import Perl.MonadGlue

class Refable a b where
  newRef :: MonadIO m => a -> PerlT s m b
  deRef :: MonadIO m => b -> PerlT s m a

instance Refable PtrSV RefSV where
  newRef = newSVRef
  deRef = deRefSV

instance Refable PtrAV RefAV where
  newRef = newAVRef
  deRef = deRefAV

instance Refable PtrHV RefHV where
  newRef = newHVRef
  deRef = deRefHV
