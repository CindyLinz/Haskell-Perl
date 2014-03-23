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

class FromSVArray a => Retrievable a where
  context
    :: a -- ^ this value will not be accessed, fine to put an undefined here
    -> Context

instance Retrievable () where
  context _ = VoidContext

instance Retrievable SV where
  context _ = ScalarContext

instance Retrievable Int where
  context _ = ScalarContext
instance Retrievable Double where
  context _ = ScalarContext
instance Retrievable String where
  context _ = ScalarContext
instance Retrievable RefSV where
  context _ = ScalarContext
instance Retrievable RefAV where
  context _ = ScalarContext
instance Retrievable RefHV where
  context _ = ScalarContext
instance Retrievable RefCV where
  context _ = ScalarContext

instance Retrievable SVArray where
  context _ = ArrayContext
instance FromSV a => Retrievable [a] where
  context _ = ArrayContext

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
    _ -> fromSVArray res

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
  => (forall s m. (MonadCatch m, MonadIO m) => CInt -> SVArray -> PerlT s (PerlT s m) SVArray)
  -> [SV]
  -> PerlT s (PerlT s m) ret
callCommon act args = do
  argArray <- asSVArray args
  res <- act (const_G_EVAL .|. (contextConstant $ context (undefined :: ret))) argArray
  err <- G.getEvalError
  case err of
    Just errSV -> do
      msg <- fromSV errSV
      throwM $ PerlException msg errSV
    _ -> lift $ fromSVArray res

-- | Call a function
call :: (ToSVList args, Retrievable ret, MonadCatch m, MonadIO m) => String -> args -> PerlT s m ret
call method args = perlWithAnyIO (withCStringLen method) $ \cName ->
  scope $ asSVList args >>= callCommon (G.callName cName)

-- | Call a code (sub) reference
callVar :: (ToSVList args, Retrievable ret, MonadCatch m, MonadIO m) => RefCV -> args -> PerlT s m ret
callVar sub args = scope $ asSVList args >>= callCommon (G.callVar sub)

-- | Call an object method
callMethod
  :: (ToSVList args, Retrievable ret, MonadCatch m, MonadIO m)
  => SV -- ^ the object
  -> String -- ^ method name
  -> args
  -> PerlT s m ret
callMethod obj method args = perlWithAnyIO (withCStringLen method) $ \cName ->
  scope $ liftM (obj :) (asSVList args) >>= callCommon (G.callNameMethod cName)

-- | Call a class method
callClass
  :: (ToSVList args, Retrievable ret, MonadCatch m, MonadIO m)
  => String -- ^ class name (package name)
  -> String -- ^ method name
  -> args
  -> PerlT s m ret
callClass klass method args = perlWithAnyIO (withCStringLen method) $ \cName ->
  scope $ do
    klassSV <- asSV klass
    svList <- asSVList args
    callCommon (G.callNameMethod cName) (klassSV : svList)

noRet :: MonadIO m => PerlT s m () -> PerlT s m ()
noRet = id
