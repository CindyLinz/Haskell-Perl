{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, Rank2Types, FlexibleContexts, ExistentialQuantification, OverlappingInstances, FunctionalDependencies, UndecidableInstances #-}
module Perl.Sub
  ( G.getSubContext
  , retSub
  , Subable (..)
  , sub
  , subDo
  , defSub
  , die
  ) where

import Data.Array.MArray

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Foreign.C.String
import Foreign.Ptr

import Perl.Type
import Perl.Monad
import Perl.SV
import Perl.SVArray
import qualified Perl.Internal.MonadGlue as G

retSub :: ToSVArray a => a -> Perl s ToSVArrayObj
retSub = return . ToSVArrayObj

class ToSVArray ret => Subable a ret | a -> ret where
  subBody :: [SV] -> a -> Perl s ret

instance ToSVArray ret => Subable (Perl s ret) ret where
  subBody _ body = PerlT $ \perl cv ->
    unPerlT body perl cv

instance (FromSV a, ToSVArray ret) => Subable ([a] -> Perl s ret) ret where
  subBody args lambda = do
    a <- mapM fromSV args
    subBody undefined (lambda a)

currySub :: (ToSVArray ret, FromSV a, Subable others ret) => [SV] -> (a -> others) -> Perl s ret
currySub args lambda = do
  (a, others) <- case args of
    [] -> do
      a' <- fromSVNon
      return (a', [])
    (a:as) -> do
      a' <- fromSV a
      return (a', as)
  subBody others (lambda a)

instance Subable others ret => Subable (SV -> others) ret where subBody = currySub
instance Subable others ret => Subable (Int -> others) ret where subBody = currySub
instance Subable others ret => Subable (Double -> others) ret where subBody = currySub
instance Subable others ret => Subable (String -> others) ret where subBody = currySub
instance Subable others ret => Subable (RefSV -> others) ret where subBody = currySub
instance Subable others ret => Subable (RefAV -> others) ret where subBody = currySub
instance Subable others ret => Subable (RefHV -> others) ret where subBody = currySub
instance Subable others ret => Subable (RefCV -> others) ret where subBody = currySub
instance ToSVArray ret => Subable (String -> Perl s ret) ret where subBody = currySub

subCommon :: (ToSVArray ret, Subable a ret) => a -> Perl s ret
subCommon body = do
  args <- G.getSubArgs
  argsList <- liftIO $ getElems args
  subBody argsList body

sub :: (ToSVArray ret, MonadCatch m, MonadIO m, Subable a ret) => a -> PerlT s m RefCV
sub body = G.makeSub $ subCommon body

subDo :: ToSVArray ret => Perl s ret -> Perl s ret
subDo = id

defSub :: (ToSVArray ret, MonadCatch m, MonadIO m, Subable a ret) => String -> a -> PerlT s m ()
defSub name body = PerlT $ \perl cv -> do
  liftIO $ withCString name $ \cName ->
    unPerlT (G.defineSub cName $ subCommon body) perl cv

-- | Throwe a Perl exception
die :: (MonadCatch m, MonadIO m) => String -> PerlT s m a
die msg = do
  errSV <- toSVMortal msg
  throwM $ PerlException msg errSV
