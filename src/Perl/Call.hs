{-# LANGUAGE Rank2Types, FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts #-}
module Perl.Call
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Data.Array.IArray
import Data.Bits

import Foreign.C.Types
import Foreign.C.String

import Perl.Type
import Perl.Class
import Perl.Constant
import Perl.Monad
import Perl.SV
import Perl.SVArray
import qualified Perl.Internal.MonadGlue as G

data Context
  = VoidContext
  | ScalarContext
  | ArrayContext

contextConstant :: Integral n => Context -> n
contextConstant VoidContext = const_G_VOID
contextConstant ScalarContext = const_G_SCALAR
contextConstant ArrayContext = const_G_ARRAY

class Retrievable a where
  context
    :: a -- ^ this value will not be accessed, fine to put an undefined here
    -> Context
  retrieve :: (MonadCatch m, MonadIO m) => SVArray -> PerlT s m a

instance Retrievable () where
  context _ = VoidContext
  retrieve _ = return ()

instance Retrievable SV where
  context _ = ScalarContext
  retrieve = fromSVArray

retrieveFromSV :: (FromSV a, MonadCatch m, MonadIO m) => SVArray -> PerlT s m a
retrieveFromSV svArray = fromSV =<< retrieve svArray
instance Retrievable Int where
  context _ = ScalarContext
  retrieve = retrieveFromSV
instance Retrievable Double where
  context _ = ScalarContext
  retrieve = retrieveFromSV
instance Retrievable String where
  context _ = ScalarContext
  retrieve = retrieveFromSV
instance Retrievable RefSV where
  context _ = ScalarContext
  retrieve = retrieveFromSV
instance Retrievable RefAV where
  context _ = ScalarContext
  retrieve = retrieveFromSV
instance Retrievable RefHV where
  context _ = ScalarContext
  retrieve = retrieveFromSV
instance Retrievable RefCV where
  context _ = ScalarContext
  retrieve = retrieveFromSV

instance Retrievable SVArray where
  context _ = ArrayContext
  retrieve = return
instance Retrievable [SV] where
  context _ = ArrayContext
  retrieve = return . elems

retrieveFromSVList :: (FromSV a, MonadCatch m, MonadIO m) => SVArray -> PerlT s m [a]
retrieveFromSVList svArray = mapM fromSV =<< retrieve svArray
instance Retrievable [Int] where
  context _ = ArrayContext
  retrieve = retrieveFromSVList
instance Retrievable [Double] where
  context _ = ArrayContext
  retrieve = retrieveFromSVList
instance Retrievable [String] where
  context _ = ArrayContext
  retrieve = retrieveFromSVList
instance Retrievable [RefSV] where
  context _ = ArrayContext
  retrieve = retrieveFromSVList
instance Retrievable [RefAV] where
  context _ = ArrayContext
  retrieve = retrieveFromSVList
instance Retrievable [RefHV] where
  context _ = ArrayContext
  retrieve = retrieveFromSVList
instance Retrievable [RefCV] where
  context _ = ArrayContext
  retrieve = retrieveFromSVList

class Retrievable b => PerlEvalable a b where
  eval :: (MonadCatch m, MonadIO m) => a -> PerlT s m b
voidEval :: (MonadCatch m, MonadIO m, PerlEvalable a ()) => a -> PerlT s m ()
voidEval = eval

evalCore :: forall s m b. (MonadCatch m, MonadIO m, Retrievable b) => CStringLen -> PerlT s m b
evalCore code = do
  res <- G.eval code (const_G_EVAL .|. (contextConstant $ context (undefined :: b)))
  err <- G.getEvalError
  case err of
    Just errSV -> fromSV errSV >>= throwM . flip PerlException errSV
    _ -> retrieve res

instance Retrievable b => PerlEvalable CStringLen b where
  eval = evalCore

instance Retrievable b => PerlEvalable String b where
  eval str = PerlT $ \perl cv ->
    liftIO $ withCStringLen str $ \cstrlen ->
      unPerlT (evalCore cstrlen) perl cv

class CallType r where
  collect
    :: (forall s m. (MonadCatch m, MonadIO m) => [SV] -> PerlT s m [SV])
    -> String
    -> (forall s m. (MonadCatch m, MonadIO m) => CStringLen -> CInt -> SVArray -> PerlT s m SVArray)
    -> r

instance (Retrievable r, MonadCatch m, MonadIO m) => CallType (PerlT s m r) where
  collect args name act = do
    scope $ do
      argList <- args []
      res <- PerlT $ \perl cv -> liftIO $ withCStringLen name $ \cName -> do
        argArray <- newListArray (1, length argList) argList
        unPerlT (act cName (const_G_EVAL .|. (contextConstant $ context (undefined :: r))) argArray) perl cv
      err <- G.getEvalError
      case err of
        Just errSV -> do
          msg <- fromSV errSV
          throwM $ PerlException msg errSV
        _ -> lift $ retrieve res

instance CallType r => CallType (SV -> r) where
  collect args name act sv = collect (args . (sv :)) name act

collectToSV
  :: (ToSV svObj, CallType r)
  => (forall s m. (MonadCatch m, MonadIO m) => [SV] -> PerlT s m [SV])
  -> String
  -> (forall s m. (MonadCatch m, MonadIO m) => CStringLen -> CInt -> SVArray -> PerlT s m SVArray)
  -> svObj
  -> r
collectToSV args name act svObj = collect
  ( \later -> do
    sv <- toSV svObj
    args $ sv : later
  ) name act
instance CallType r => CallType (Int -> r) where
  collect = collectToSV
instance CallType r => CallType (Double -> r) where
  collect = collectToSV
instance CallType r => CallType (String -> r) where
  collect = collectToSV
instance CallType r => CallType (RefSV -> r) where
  collect = collectToSV
instance CallType r => CallType (RefAV -> r) where
  collect = collectToSV
instance CallType r => CallType (RefHV -> r) where
  collect = collectToSV
instance CallType r => CallType (RefCV -> r) where
  collect = collectToSV

instance CallType r => CallType ([SV] -> r) where
  collect args name act svs = collect (args . (svs ++)) name act

collectToSVList
  :: (ToSV svObj, CallType r)
  => (forall s m. (MonadCatch m, MonadIO m) => [SV] -> PerlT s m [SV])
  -> String
  -> (forall s m. (MonadCatch m, MonadIO m) => CStringLen -> CInt -> SVArray -> PerlT s m SVArray)
  -> [svObj]
  -> r
collectToSVList args name act svObjs = collect
  ( \later -> do
    svs <- mapM toSV svObjs
    args $ svs ++ later
  ) name act
instance CallType r => CallType ([Int] -> r) where
  collect = collectToSVList
instance CallType r => CallType ([Double] -> r) where
  collect = collectToSVList
instance CallType r => CallType ([String] -> r) where
  collect = collectToSVList
instance CallType r => CallType ([RefSV] -> r) where
  collect = collectToSVList
instance CallType r => CallType ([RefAV] -> r) where
  collect = collectToSVList
instance CallType r => CallType ([RefHV] -> r) where
  collect = collectToSVList
instance CallType r => CallType ([RefCV] -> r) where
  collect = collectToSVList

-- | Call a method
call :: CallType r => String -> r
call name = collect return name G.callName

-- | Call an object method
callMethod
  :: CallType r
  => SV -- ^ object
  -> String -- ^ method name
  -> r
callMethod obj name = collect (\later -> return (obj : later)) name G.callNameMethod

-- | Call a class method
callClass
  :: CallType r
  => String -- ^ class name (package name)
  -> String -- ^ method name
  -> r
callClass klass name = collect
  ( \later -> do
    klassSV <- toSV klass
    return (klassSV : later)
  ) name G.callNameMethod

noRet :: MonadIO m => PerlT s m () -> PerlT s m ()
noRet = id
