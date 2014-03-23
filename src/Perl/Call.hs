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
import Perl.SVList
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

callCommon
  :: forall ret s m. (Retrievable ret, MonadCatch m, MonadIO m)
  => (forall s m. (MonadCatch m, MonadIO m) => CStringLen -> CInt -> SVArray -> PerlT s (PerlT s m) SVArray)
  -> String
  -> [SV]
  -> PerlT s (PerlT s m) ret
callCommon act name args = PerlT $ \perl1 cv1 -> PerlT $ \perl2 cv2 ->
  liftIO $ withCStringLen name $ \cName -> unPerlT (unPerlT (do
    argArray <- asSVArray args
    res <- act cName (const_G_EVAL .|. (contextConstant $ context (undefined :: ret))) argArray
    err <- G.getEvalError
    case err of
      Just errSV -> do
        msg <- fromSV errSV
        throwM $ PerlException msg errSV
      _ -> lift $ retrieve res
  ) perl2 cv2) perl1 cv1

-- | Call a method
call :: (ToSVList args, Retrievable ret, MonadCatch m, MonadIO m) => String -> args -> PerlT s m ret
call method args = scope $ asSVList args >>= callCommon G.callName method

-- | Call an object method
callMethod
  :: (ToSVList args, Retrievable ret, MonadCatch m, MonadIO m)
  => SV -- ^ the object
  -> String -- ^ method name
  -> args
  -> PerlT s m ret
callMethod obj method args = scope $ liftM (obj :) (asSVList args) >>= callCommon G.callNameMethod method

-- | Call a class method
callClass
  :: (ToSVList args, Retrievable ret, MonadCatch m, MonadIO m)
  => String -- ^ class name (package name)
  -> String -- ^ method name
  -> args
  -> PerlT s m ret
callClass klass method args = scope $ do
  klassSV <- asSV klass
  svList <- asSVList args
  callCommon G.callNameMethod method (klassSV : svList)

noRet :: MonadIO m => PerlT s m () -> PerlT s m ()
noRet = id
