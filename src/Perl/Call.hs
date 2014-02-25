{-# LANGUAGE Rank2Types, FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts #-}
module Perl.Call
  where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Data.Array.Storable

import Foreign.C.Types
import Foreign.C.String

import Perl.Type
import Perl.Constant
import Perl.Monad
import Perl.SV
import qualified Perl.MonadGlue as G

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
  retrieve :: MonadIO m => SVArray -> PerlT s m a

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

retrieveFromSV :: (FromSV a, MonadIO m) => SVArray -> PerlT s m a
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

retrieveFromSVList :: (FromSV a, MonadIO m) => SVArray -> PerlT s m [a]
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
  eval :: MonadIO m => a -> PerlT s m b
voidEval :: (MonadIO m, PerlEvalable a ()) => a -> PerlT s m ()
voidEval = eval

instance Retrievable b => PerlEvalable CStringLen b where
  eval code = retrieve =<< G.eval code (contextConstant $ context (undefined :: b))

instance Retrievable b => PerlEvalable String b where
  eval str = PerlT $ \perl frames ->
    liftIO $ withCStringLen str $ \cstrlen ->
      unPerlT (retrieve =<< G.eval cstrlen (contextConstant $ context (undefined :: b))) perl frames

class CallType r where
  collect :: (forall s m. MonadIO m => [SV] -> PerlT s m [SV]) -> String -> r

instance (Retrievable r, MonadIO m) => CallType (PerlT s m r) where
  collect args name = scope $ do
    argList <- args []
    res <- PerlT $ \perl frames -> liftIO $ withCStringLen name $ \cName -> do
      argArray <- newListArray (1, length argList) argList
      unPerlT (G.callName cName (contextConstant $ context (undefined :: r)) argArray) perl frames
    retrieve res

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

