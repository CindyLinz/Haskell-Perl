{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Perl.AsSV
  where

import Control.Monad.IO.Class
import Foreign.Ptr

import Perl.Type
import Perl.Monad

class AsSV a where
  asSV :: MonadIO m => a -> PerlT s m SV

instance AsSV SV where asSV = return
instance AsSV CV where asSV = return . castPtr
instance AsSV RefSV where asSV = return . castPtr
instance AsSV RefAV where asSV = return . castPtr
instance AsSV RefHV where asSV = return . castPtr
instance AsSV RefCV where asSV = return . castPtr
