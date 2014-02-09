{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, Rank2Types, FlexibleContexts, ExistentialQuantification #-}
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

data SubReturnObj = forall a. SubReturn a => SubReturnObj a
retSub :: SubReturn a => a -> PerlSubT s IO SubReturnObj
retSub = return . SubReturnObj

instance SubReturn [ToSVObj] where
  returnSub retList = do
    retSVList <- liftPerl $ forM retList (\(ToSVObj a) -> toSVMortal a)
    liftPerl $ forM_ retSVList G.incRefCnt
    let len = length retSVList
    rets <- liftIO $ newListArray (1,len) retSVList
    G.setSubReturns rets

instance SubReturn SubReturnObj where returnSub (SubReturnObj a) = returnSub a
instance SubReturn () where returnSub _ = returnSub ([] :: [ToSVObj])
instance SubReturn ToSVObj where returnSub a = returnSub [a]
instance SubReturn PtrSV where returnSub a = returnSub [ToSVObj a]
instance SubReturn Int where returnSub a = returnSub [ToSVObj a]
instance SubReturn Double where returnSub a = returnSub [ToSVObj a]
instance SubReturn String where returnSub a = returnSub [ToSVObj a]
instance (ToSV a, ToSV b) => SubReturn (a, b) where returnSub (a, b) = returnSub [ToSVObj a, ToSVObj b]
instance (ToSV a, ToSV b, ToSV c) => SubReturn (a, b, c) where returnSub (a, b, c) = returnSub [ToSVObj a, ToSVObj b, ToSVObj c]
instance (ToSV a, ToSV b, ToSV c, ToSV d) => SubReturn (a, b, c, d) where returnSub (a, b, c, d) = returnSub [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d]
instance (ToSV a, ToSV b, ToSV c, ToSV d, ToSV e) => SubReturn (a, b, c, d, e) where returnSub (a, b, c, d, e) = returnSub [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e]
instance (ToSV a, ToSV b, ToSV c, ToSV d, ToSV e, ToSV f) => SubReturn (a, b, c, d, e, f) where returnSub (a, b, c, d, e, f) = returnSub [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e, ToSVObj f]
instance (ToSV a, ToSV b, ToSV c, ToSV d, ToSV e, ToSV f, ToSV g) => SubReturn (a, b, c, d, e, f, g) where returnSub (a, b, c, d, e, f, g) = returnSub [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e, ToSVObj f, ToSVObj g]

class Subable a where
  subBody :: [PtrSV] -> a -> PerlSubT s IO ()

instance SubReturn ret => Subable (PerlSubT s IO ret) where
  subBody _ body = PerlSubT $ \perl cv -> do
    ret <- unPerlSubT body perl cv
    unPerlSubT (returnSub ret) perl cv

instance (FromSV a, SubReturn ret) => Subable ([a] -> PerlSubT s IO ret) where
  subBody args lambda = do
    a <- liftPerl $ mapM fromSV args
    subBody undefined (lambda a)

currySub :: (FromSV a, Subable others) => [PtrSV] -> (a -> others) -> PerlSubT s IO ()
currySub args lambda = do
  (a, others) <- case args of
    [] -> do
      a' <- liftPerl fromSVNon
      return (a', [])
    (a:as) -> do
      a' <- liftPerl $ fromSV a
      return (a', as)
  subBody others (lambda a)

instance Subable others => Subable (PtrSV -> others) where subBody = currySub
instance Subable others => Subable (Int -> others) where subBody = currySub
instance Subable others => Subable (Double -> others) where subBody = currySub
instance Subable others => Subable (String -> others) where subBody = currySub

sub :: (MonadIO m, Subable a) => a -> PerlT s m PtrSV
sub body = makeSub $ do
  args <- G.getSubArgs
  argsList <- liftIO $ getElems args
  subBody argsList body

subDo :: SubReturn ret => PerlSubT s IO ret -> PerlSubT s IO ret
subDo = id
