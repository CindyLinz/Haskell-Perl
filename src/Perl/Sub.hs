{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, Rank2Types, FlexibleContexts, ExistentialQuantification, OverlappingInstances #-}
module Perl.Sub
  ( G.getSubContext
  , SubReturn (..)
  , SubReturnObj
  , retSub
  , Subable (..)
  , sub
  , subDo
  , defSub
  ) where

import Data.Array.MArray

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Foreign.C.String

import Perl.Type
import Perl.Monad
import Perl.SV
import qualified Perl.Internal.MonadGlue as G

class SubReturn a where
  returnSub :: a -> PerlSub s ()

data SubReturnObj = forall a. SubReturn a => SubReturnObj a
retSub :: SubReturn a => a -> PerlSub s SubReturnObj
retSub = return . SubReturnObj

returnSubToSV :: ToSV a => a -> PerlSub s ()
returnSubToSV a = do
  aMortal <- lift $ toSVMortal a
  rets <- liftIO $ newArray (1,1) aMortal
  G.setSubReturns rets

returnSubToSVList :: ToSV a => [a] -> PerlSub s ()
returnSubToSVList retList = do
  retSVList <- lift $ forM retList (\a -> toSVMortal a)
  let len = length retSVList
  rets <- liftIO $ newListArray (1,len) retSVList
  G.setSubReturns rets

instance SubReturn SubReturnObj where returnSub (SubReturnObj a) = returnSub a

instance SubReturn () where
  returnSub _ = do
    rets <- liftIO $ newArray (1,0) undefined
    G.setSubReturns rets

instance SubReturn ToSVObj where returnSub = returnSubToSV
instance SubReturn SV where returnSub = returnSubToSV
instance SubReturn Int where returnSub = returnSubToSV
instance SubReturn Double where returnSub = returnSubToSV
instance SubReturn String where returnSub = returnSubToSV
instance SubReturn RefSV where returnSub = returnSubToSV
instance SubReturn RefAV where returnSub = returnSubToSV
instance SubReturn RefHV where returnSub = returnSubToSV
instance SubReturn RefCV where returnSub = returnSubToSV
instance ToSV a => SubReturn [a] where returnSub = returnSubToSVList
instance (ToSV a, ToSV b) => SubReturn (a, b) where returnSub (a, b) = returnSub [ToSVObj a, ToSVObj b]
instance (ToSV a, ToSV b, ToSV c) => SubReturn (a, b, c) where returnSub (a, b, c) = returnSub [ToSVObj a, ToSVObj b, ToSVObj c]
instance (ToSV a, ToSV b, ToSV c, ToSV d) => SubReturn (a, b, c, d) where returnSub (a, b, c, d) = returnSub [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d]
instance (ToSV a, ToSV b, ToSV c, ToSV d, ToSV e) => SubReturn (a, b, c, d, e) where returnSub (a, b, c, d, e) = returnSub [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e]
instance (ToSV a, ToSV b, ToSV c, ToSV d, ToSV e, ToSV f) => SubReturn (a, b, c, d, e, f) where returnSub (a, b, c, d, e, f) = returnSub [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e, ToSVObj f]
instance (ToSV a, ToSV b, ToSV c, ToSV d, ToSV e, ToSV f, ToSV g) => SubReturn (a, b, c, d, e, f, g) where returnSub (a, b, c, d, e, f, g) = returnSub [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e, ToSVObj f, ToSVObj g]

class Subable a where
  subBody :: [SV] -> a -> PerlSub s ()

instance SubReturn ret => Subable (PerlSub s ret) where
  subBody _ body = PerlSubT $ \perl cv -> PerlT $ \_ frames -> do
    (frames', ret) <- unPerlT (unPerlSubT body perl cv) perl frames
    unPerlT (unPerlSubT (returnSub ret) perl cv) perl frames'

instance (FromSV a, SubReturn ret) => Subable ([a] -> PerlSub s ret) where
  subBody args lambda = do
    a <- lift $ mapM fromSV args
    subBody undefined (lambda a)

currySub :: (FromSV a, Subable others) => [SV] -> (a -> others) -> PerlSub s ()
currySub args lambda = do
  (a, others) <- case args of
    [] -> do
      a' <- lift fromSVNon
      return (a', [])
    (a:as) -> do
      a' <- lift $ fromSV a
      return (a', as)
  subBody others (lambda a)

instance Subable others => Subable (SV -> others) where subBody = currySub
instance Subable others => Subable (Int -> others) where subBody = currySub
instance Subable others => Subable (Double -> others) where subBody = currySub
instance Subable others => Subable (String -> others) where subBody = currySub
instance Subable others => Subable (RefSV -> others) where subBody = currySub
instance Subable others => Subable (RefAV -> others) where subBody = currySub
instance Subable others => Subable (RefHV -> others) where subBody = currySub
instance Subable others => Subable (RefCV -> others) where subBody = currySub
instance SubReturn ret => Subable (String -> PerlSub s ret) where subBody = currySub

subCommon :: Subable a => a -> PerlSub s ()
subCommon body = do
  args <- G.getSubArgs
  argsList <- liftIO $ getElems args
  subBody argsList body

sub :: (MonadIO m, Subable a) => a -> PerlT s m RefCV
sub body = makeSub $ subCommon body

subDo :: SubReturn ret => PerlSub s ret -> PerlSub s ret
subDo = id

defSub :: (MonadIO m, Subable a) => String -> a -> PerlT s m ()
defSub name body = PerlT $ \perl frames -> do
  liftIO $ withCString name $ \cName ->
    unPerlT (defineSub cName $ subCommon body) perl frames
