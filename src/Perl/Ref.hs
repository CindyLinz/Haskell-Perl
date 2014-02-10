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

instance Refable SV RefSV where
  newRef = newSVRef
  deRef = deRefSV

instance Refable AV RefAV where
  newRef = newAVRef
  deRef = deRefAV

instance Refable HV RefHV where
  newRef = newHVRef
  deRef = deRefHV
