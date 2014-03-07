{-# LANGUAGE Rank2Types, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification #-}
module Perl.SVArray
  ( SVArray
  , SVArrayBuilder
  , SVArrayBreaker
  , addToSV
  , addFromSV
  , buildSVArray
  , breakSVArray
  , ToSVArray (..)
  , ToSVArrayObj (..)
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Monoid
import Data.Array.MArray
import Data.Array.Storable

import Perl.Type
import Perl.Class
import Perl.Monad
import Perl.SV

newtype SVArrayBuilder = SVArrayBuilder { unSVArrayBuilder :: [ToSVObj] -> [ToSVObj] }
newtype SVArrayBreaker s m = SVArrayBreaker { unSVArrayBreaker :: [[SV] -> PerlT s m [SV]] -> [[SV] -> PerlT s m [SV]] }

instance Monoid SVArrayBuilder where
  mempty = SVArrayBuilder id
  mappend former later = SVArrayBuilder $ unSVArrayBuilder former . unSVArrayBuilder later

instance Monoid (SVArrayBreaker s m) where
  mempty = SVArrayBreaker id
  mappend former later = SVArrayBreaker $ unSVArrayBreaker former . unSVArrayBreaker later

addToSV :: ToSV a => a -> SVArrayBuilder
addToSV a = SVArrayBuilder (ToSVObj a :)

addFromSV :: ([SV] -> PerlT s m [SV]) -> SVArrayBreaker s m
addFromSV r = SVArrayBreaker (r :)

buildSVArray :: (MonadCatch m, MonadIO m) => SVArrayBuilder -> PerlT s m SVArray
buildSVArray builder = do
  svList <- forM (unSVArrayBuilder builder []) toSV
  liftIO $ newListArray (1, length svList) svList

-- return the unconsumed SVs
breakSVArray :: (MonadCatch m, MonadIO m) => SVArrayBreaker s m -> SVArray -> PerlT s m [SV]
breakSVArray breakers svArray = do
  svList <- liftIO $ getElems svArray
  foldM (\svs br -> br svs) svList (unSVArrayBreaker breakers [])

data ToSVArrayObj = forall a. ToSVArray a => ToSVArrayObj a
instance ToSVArray ToSVArrayObj where
  toSVArray (ToSVArrayObj a) = toSVArray a
  toSVMortalArray (ToSVArrayObj a) = toSVMortalArray a

instance ToSVArray () where
  toSVArray _ = liftIO $ newArray_ (1,0)
  toSVMortalArray _ = liftIO $ newArray_ (1,0)

toSVToSVArray :: (ToSV a, MonadCatch m, MonadIO m) => a -> PerlT s m SVArray
toSVToSVArray a = do
  sv <- toSV a
  liftIO $ newArray (1,1) sv

toSVToSVMortalArray :: (ToSV a, MonadCatch m, MonadIO m) => a -> PerlT s m SVArray
toSVToSVMortalArray a = do
  sv <- toSVMortal a
  liftIO $ newArray (1,1) sv

instance ToSVArray ToSVObj where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
instance ToSVArray SV where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
instance ToSVArray Int where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
instance ToSVArray Double where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
instance ToSVArray String where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
instance ToSVArray RefSV where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
instance ToSVArray RefAV where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
instance ToSVArray RefHV where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
instance ToSVArray RefCV where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
instance ToSV a => ToSVArray [a] where
  toSVArray as = do
    retSVList <- forM as toSV
    liftIO $ newListArray (1,length as) retSVList
  toSVMortalArray as = do
    retSVMortalList <- forM as toSVMortal
    liftIO $ newListArray (1,length as) retSVMortalList
instance (ToSV a, ToSV b) => ToSVArray (a, b) where
  toSVArray (a, b) = toSVArray [ToSVObj a, ToSVObj b]
  toSVMortalArray (a, b) = toSVMortalArray [ToSVObj a, ToSVObj b]
instance (ToSV a, ToSV b, ToSV c) => ToSVArray (a, b, c) where
  toSVArray (a, b, c) = toSVArray [ToSVObj a, ToSVObj b, ToSVObj c]
  toSVMortalArray (a, b, c) = toSVMortalArray [ToSVObj a, ToSVObj b, ToSVObj c]
instance (ToSV a, ToSV b, ToSV c, ToSV d) => ToSVArray (a, b, c, d) where
  toSVArray (a, b, c, d) = toSVArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d]
  toSVMortalArray (a, b, c, d) = toSVMortalArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d]
instance (ToSV a, ToSV b, ToSV c, ToSV d, ToSV e) => ToSVArray (a, b, c, d, e) where
  toSVArray (a, b, c, d, e) = toSVArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e]
  toSVMortalArray (a, b, c, d, e) = toSVMortalArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e]
instance (ToSV a, ToSV b, ToSV c, ToSV d, ToSV e, ToSV f) => ToSVArray (a, b, c, d, e, f) where
  toSVArray (a, b, c, d, e, f) = toSVArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e, ToSVObj f]
  toSVMortalArray (a, b, c, d, e, f) = toSVMortalArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e, ToSVObj f]
instance (ToSV a, ToSV b, ToSV c, ToSV d, ToSV e, ToSV f, ToSV g) => ToSVArray (a, b, c, d, e, f, g) where
  toSVArray (a, b, c, d, e, f, g) = toSVArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e, ToSVObj f, ToSVObj g]
  toSVMortalArray (a, b, c, d, e, f, g) = toSVMortalArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e, ToSVObj f, ToSVObj g]
