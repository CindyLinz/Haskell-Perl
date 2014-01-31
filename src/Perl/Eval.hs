{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
module Perl.Eval where

import Foreign.C.String
import Control.Monad.IO.Class

import Perl.Type
import Perl.Monad
import qualified Perl.MonadGlue as G

class PerlEvalable m a where
  eval :: a -> PerlT s m PtrSV

instance MonadIO m => PerlEvalable m CString where
  eval = G.eval

instance MonadIO m => PerlEvalable m String where
  eval str = PerlT $ \perl frames ->
    liftIO $ withCString str $ \cstr ->
      unPerlT (G.eval cstr) perl frames
