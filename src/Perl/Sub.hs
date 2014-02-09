{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, Rank2Types, FlexibleContexts #-}
module Perl.Sub
  where

import Control.Monad
import Control.Monad.IO.Class
import Data.Array.MArray

import Perl.Type
import Perl.Monad
import Perl.ToSV
import Perl.FromSV
import qualified Perl.MonadGlue as G

class SubReturn a where
  returnSub :: a -> PerlSubT s IO ()

instance SubReturn [ToSVObj] where
  returnSub retList = do
    retSVList <- liftPerl $ forM retList (\(ToSVObj a) -> toSVMortal a)
    liftPerl $ forM_ retSVList G.incRefCnt
    let len = length retSVList
    rets <- liftIO $ newListArray (1,len) retSVList
    G.setSubReturns rets

class Subable a where
  subBody :: [PtrSV] -> a -> PerlSubT s IO ()

instance SubReturn ret => Subable (PerlSubT s IO ret) where
  subBody _ body = PerlSubT $ \perl cv -> do
    ret <- unPerlSubT body perl cv
    unPerlSubT (returnSub ret) perl cv
--  subBody _ body = do
--    ret <- body :: PerlSubT s m ret
--    returnSub ret :: PerlSubT s m ()

instance (FromSV a, Subable others) => Subable (a -> others) where
  subBody args lambda = do
    (a, others) <- case args of
      [] -> do
        a' <- liftPerl fromSVNon
        return (a', [])
      (a:as) -> do
        a' <- liftPerl $ fromSV a
        return (a', as)
    subBody others (lambda a)

sub :: (MonadIO m, Subable a) => a -> PerlT s m PtrSV
sub body = makeSub $ do
  args <- G.getSubArgs
  argsList <- liftIO $ getElems args
  subBody argsList body

subDo :: SubReturn ret => PerlSubT s IO ret -> PerlSubT s IO ret
subDo = id
