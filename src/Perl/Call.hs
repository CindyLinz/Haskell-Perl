{-# LANGUAGE Rank2Types, FlexibleInstances #-}
module Perl.Call
  where

import Control.Monad
import Control.Monad.IO.Class
import Data.Array.Storable
import Foreign.C.Types
import Foreign.C.String

import Perl.Type
import Perl.Constant
import Perl.Monad
import qualified Perl.MonadGlue as G
import Perl.SV

--class ToArgs a where
--  toArgs :: a -> StorableArray Int SV
--
--data ArgList m = forall s. ToSV m s => ArgList [s]
--
--instance MonadIO m => ToArgs (ArgList m) where
--  toArgs = undefined

--class CallType m r | r -> m where
--  collect :: ([ToSVObj m] -> [ToSVObj m]) -> String -> r
--
--instance MonadIO m => CallType m (PerlT s m (StorableArray Int SV)) where
--  collect args name = undefined
--
--instance (MonadIO m, CallType m r) => CallType m (ToSVObj m -> r) where
--  collect args name sv = collect (\later -> args (sv : later)) name
--
--call :: (MonadIO m, CallType m r) => String -> r
--call name = collect id name

class CallType r where
  collect :: (forall s m. MonadIO m => [SV] -> PerlT s m [SV]) -> String -> r

callCommon :: MonadIO m => CInt -> (forall s1 m1. MonadIO m1 => [SV] -> PerlT s1 m1 [SV]) -> String -> PerlT s m (StorableArray Int SV)
callCommon flag args name = scope $ do
  argList <- args []
  res <- PerlT $ \perl frames -> liftIO $ withCStringLen name $ \cName -> do
    argArray <- newListArray (1, length argList) argList
    unPerlT (G.callName cName flag argArray) perl frames
  return res

callScalarCommon :: (FromSV a, MonadIO m) => (forall s1 m1. MonadIO m1 => [SV] -> PerlT s1 m1 [SV]) -> String -> PerlT s m a
callScalarCommon args name = do
  resArray <- callCommon const_G_SCALAR args name
  resSV <- liftIO $ readArray resArray 1
  fromSV resSV

callListCommon :: (FromSV a, MonadIO m) => (forall s1 m1. MonadIO m1 => [SV] -> PerlT s1 m1 [SV]) -> String -> PerlT s m [a]
callListCommon args name = do
  resArray <- callCommon const_G_ARRAY args name
  resSVList <- liftIO $ getElems resArray
  mapM fromSV resSVList

instance MonadIO m => CallType (PerlT s m (StorableArray Int SV)) where
  collect = callCommon const_G_ARRAY

instance MonadIO m => CallType (PerlT s m ()) where
  collect args name = callCommon const_G_VOID args name >> return ()

instance MonadIO m => CallType (PerlT s m SV) where collect = callScalarCommon
instance MonadIO m => CallType (PerlT s m Int) where collect = callScalarCommon
instance MonadIO m => CallType (PerlT s m Double) where collect = callScalarCommon
instance MonadIO m => CallType (PerlT s m String) where collect = callScalarCommon

instance MonadIO m => CallType (PerlT s m [SV]) where collect = callListCommon
instance MonadIO m => CallType (PerlT s m [Int]) where collect = callListCommon
instance MonadIO m => CallType (PerlT s m [Double]) where collect = callListCommon
instance MonadIO m => CallType (PerlT s m [String]) where collect = callListCommon

instance (ToSV svObj, CallType r) => CallType (svObj -> r) where
  collect args name svObj = collect
    ( \later -> do
      sv <- toSV svObj
      args $ sv : later
    ) name

call :: CallType r => String -> r
call name = collect return name

noRet :: MonadIO m => PerlT s m () -> PerlT s m ()
noRet = id

