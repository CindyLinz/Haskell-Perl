{-# LANGUAGE Rank2Types, FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts #-}
module Perl.Call
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Array.Storable
import Data.Bits

import Foreign.C.Types
import Foreign.C.String

import Perl.Type
import Perl.Constant
import Perl.Monad
import Perl.SV
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
  retrieve svArray = do
    lastIndex <- liftIO $ snd <$> getBounds svArray
    if lastIndex > 0
      then fromSV =<< liftIO (readArray svArray lastIndex)
      else fromSVNon

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
  retrieve = liftIO . getElems

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
  eval :: (MonadCatch m, MonadIO m) => a -> PerlT s m (Either String b)
voidEval :: (MonadCatch m, MonadIO m, PerlEvalable a ()) => a -> PerlT s m (Either String ())
voidEval = eval

evalCore :: forall s m b. (MonadCatch m, MonadIO m, Retrievable b) => CStringLen -> PerlT s m (Either String b)
evalCore code = do
  res <- G.eval code (const_G_EVAL .|. (contextConstant $ context (undefined :: b)))
  err <- G.getEvalError
  case err of
    Just errSV -> fromSV errSV >>= return . Left
    _ -> retrieve res >>= return . Right

instance Retrievable b => PerlEvalable CStringLen b where
  eval = evalCore

instance Retrievable b => PerlEvalable String b where
  eval str = PerlT $ \perl cv ->
    liftIO $ withCStringLen str $ \cstrlen ->
      unPerlT (evalCore cstrlen) perl cv

class CallType r where
  collect :: (forall s m. (MonadCatch m, MonadIO m) => [SV] -> PerlT s m [SV]) -> String -> r

instance (Retrievable r, MonadCatch m, MonadIO m) => CallType (PerlT s m r) where
  collect args name = do
    scope $ do
      argList <- args []
      res <- PerlT $ \perl cv -> liftIO $ withCStringLen name $ \cName -> do
        argArray <- newListArray (1, length argList) argList
        unPerlT (G.callName cName (const_G_EVAL .|. (contextConstant $ context (undefined :: r))) argArray) perl cv
      err <- G.getEvalError
      case err of
        Just errSV -> do
          msg <- fromSV errSV
          throwM $ PerlException msg errSV
        _ -> retrieve res

instance CallType r => CallType (SV -> r) where
  collect args name sv = collect (args . (sv :)) name

collectToSV :: (ToSV svObj, CallType r) => (forall s m. (MonadCatch m, MonadIO m) => [SV] -> PerlT s m [SV]) -> String -> svObj -> r
collectToSV args name svObj = collect
  ( \later -> do
    sv <- toSV svObj
    args $ sv : later
  ) name
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
  collect args name svs = collect (args . (svs ++)) name

collectToSVList :: (ToSV svObj, CallType r) => (forall s m. (MonadCatch m, MonadIO m) => [SV] -> PerlT s m [SV]) -> String -> [svObj] -> r
collectToSVList args name svObjs = collect
  ( \later -> do
    svs <- mapM toSV svObjs
    args $ svs ++ later
  ) name
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

call :: CallType r => String -> r
call name = collect return name

noRet :: MonadIO m => PerlT s m () -> PerlT s m ()
noRet = id
