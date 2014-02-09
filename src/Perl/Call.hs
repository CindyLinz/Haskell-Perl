{-# LANGUAGE RankNTypes, FlexibleInstances, ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
module Perl.Call
  where

import Control.Monad
import Control.Monad.IO.Class
import Data.Array.Storable
import Foreign.C.String

import Perl.Type
import Perl.Monad
import qualified Perl.MonadGlue as G
import Perl.ToSV

--class ToArgs a where
--  toArgs :: a -> StorableArray Int PtrSV
--
--data ArgList m = forall s. ToSV m s => ArgList [s]
--
--instance MonadIO m => ToArgs (ArgList m) where
--  toArgs = undefined

--class CallType m r | r -> m where
--  collect :: ([ToSVObj m] -> [ToSVObj m]) -> String -> r
--
--instance MonadIO m => CallType m (PerlT s m (StorableArray Int PtrSV)) where
--  collect args name = undefined
--
--instance (MonadIO m, CallType m r) => CallType m (ToSVObj m -> r) where
--  collect args name sv = collect (\later -> args (sv : later)) name
--
--call :: (MonadIO m, CallType m r) => String -> r
--call name = collect id name

class CallType r where
  collect :: (forall s m. MonadIO m => [PtrSV] -> PerlT s m [PtrSV]) -> String -> r

instance MonadIO m => CallType (PerlT s m (StorableArray Int PtrSV)) where
  collect args name = scope $ do
    argList <- args []
    res <- PerlT $ \perl frames -> liftIO $ withCString name $ \cName -> do
      argArray <- newListArray (1, length argList) argList
      unPerlT (G.callName cName 0 argArray) perl frames
    return res

instance (ToSV svObj, CallType r) => CallType (svObj -> r) where
  collect args name svObj = collect
    ( \later -> do
      sv <- toSV svObj
      args $ sv : later
    ) name

call :: CallType r => String -> r
call name = collect return name

noRet :: (Functor m, MonadIO m) => PerlT s m (StorableArray Int PtrSV) -> PerlT s m ()
noRet x = fmap (undefined $!) x >> return ()

