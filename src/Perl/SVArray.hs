{-# LANGUAGE Rank2Types, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, OverlappingInstances #-}
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
import Data.Array.IArray
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
  return $ listArray (1, length svList) svList

-- return the unconsumed SVs
breakSVArray :: (MonadCatch m, MonadIO m) => SVArrayBreaker s m -> SVArray -> PerlT s m [SV]
breakSVArray breakers svArray = do
  foldM (\svs br -> br svs) (elems svArray) (unSVArrayBreaker breakers [])

duplicateSVArray :: (MonadCatch m, MonadIO m) => (SV -> PerlT s m SV) -> SVArray -> PerlT s m SVArray
duplicateSVArray morph orig = do
  mapM morph (elems orig) >>= return . listArray (bounds orig)
instance ToSVArray SVArray where
  toSVArray = duplicateSVArray toSV
  toSVMortalArray = duplicateSVArray toSVMortal
  asSVArray = duplicateSVArray asSV

data ToSVArrayObj = forall a. ToSVArray a => ToSVArrayObj a
instance ToSVArray ToSVArrayObj where
  toSVArray (ToSVArrayObj a) = toSVArray a
  toSVMortalArray (ToSVArrayObj a) = toSVMortalArray a
  asSVArray (ToSVArrayObj a) = asSVArray a

instance ToSVArray () where
  toSVArray _ = return $ array (1,0) []
  toSVMortalArray _ = return $ array (1,0) []
  asSVArray _ = return $ array (1,0) []

toSVToSVArray :: (ToSV a, MonadCatch m, MonadIO m) => a -> PerlT s m SVArray
toSVToSVArray a = do
  sv <- toSV a
  return $ listArray (1,1) [sv]

toSVToSVMortalArray :: (ToSV a, MonadCatch m, MonadIO m) => a -> PerlT s m SVArray
toSVToSVMortalArray a = do
  sv <- toSVMortal a
  return $ listArray (1,1) [sv]

toSVAsSVArray :: (ToSV a, MonadCatch m, MonadIO m) => a -> PerlT s m SVArray
toSVAsSVArray a = do
  sv <- asSV a
  return $ listArray (1,1) [sv]

instance ToSVArray ToSVObj where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
  asSVArray = toSVAsSVArray
instance ToSVArray SV where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
  asSVArray = toSVAsSVArray
instance ToSVArray Int where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
  asSVArray = toSVAsSVArray
instance ToSVArray Double where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
  asSVArray = toSVAsSVArray
instance ToSVArray String where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
  asSVArray = toSVAsSVArray
instance ToSVArray RefSV where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
  asSVArray = toSVAsSVArray
instance ToSVArray RefAV where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
  asSVArray = toSVAsSVArray
instance ToSVArray RefHV where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
  asSVArray = toSVAsSVArray
instance ToSVArray RefCV where
  toSVArray = toSVToSVArray
  toSVMortalArray = toSVToSVMortalArray
  asSVArray = toSVAsSVArray
instance ToSV a => ToSVArray [a] where
  toSVArray as = do
    retSVList <- forM as toSV
    return $ listArray (1,length as) retSVList
  toSVMortalArray as = do
    retSVMortalList <- forM as toSVMortal
    return $ listArray (1,length as) retSVMortalList
  asSVArray as = do
    retSVList <- forM as asSV
    return $ listArray (1,length as) retSVList
instance (ToSV a, ToSV b) => ToSVArray (a, b) where
  toSVArray (a, b) = toSVArray [ToSVObj a, ToSVObj b]
  toSVMortalArray (a, b) = toSVMortalArray [ToSVObj a, ToSVObj b]
  asSVArray (a, b) = asSVArray [ToSVObj a, ToSVObj b]
instance (ToSV a, ToSV b, ToSV c) => ToSVArray (a, b, c) where
  toSVArray (a, b, c) = toSVArray [ToSVObj a, ToSVObj b, ToSVObj c]
  toSVMortalArray (a, b, c) = toSVMortalArray [ToSVObj a, ToSVObj b, ToSVObj c]
  asSVArray (a, b, c) = asSVArray [ToSVObj a, ToSVObj b, ToSVObj c]
instance (ToSV a, ToSV b, ToSV c, ToSV d) => ToSVArray (a, b, c, d) where
  toSVArray (a, b, c, d) = toSVArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d]
  toSVMortalArray (a, b, c, d) = toSVMortalArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d]
  asSVArray (a, b, c, d) = asSVArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d]
instance (ToSV a, ToSV b, ToSV c, ToSV d, ToSV e) => ToSVArray (a, b, c, d, e) where
  toSVArray (a, b, c, d, e) = toSVArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e]
  toSVMortalArray (a, b, c, d, e) = toSVMortalArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e]
  asSVArray (a, b, c, d, e) = asSVArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e]
instance (ToSV a, ToSV b, ToSV c, ToSV d, ToSV e, ToSV f) => ToSVArray (a, b, c, d, e, f) where
  toSVArray (a, b, c, d, e, f) = toSVArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e, ToSVObj f]
  toSVMortalArray (a, b, c, d, e, f) = toSVMortalArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e, ToSVObj f]
  asSVArray (a, b, c, d, e, f) = asSVArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e, ToSVObj f]
instance (ToSV a, ToSV b, ToSV c, ToSV d, ToSV e, ToSV f, ToSV g) => ToSVArray (a, b, c, d, e, f, g) where
  toSVArray (a, b, c, d, e, f, g) = toSVArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e, ToSVObj f, ToSVObj g]
  toSVMortalArray (a, b, c, d, e, f, g) = toSVMortalArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e, ToSVObj f, ToSVObj g]
  asSVArray (a, b, c, d, e, f, g) = asSVArray [ToSVObj a, ToSVObj b, ToSVObj c, ToSVObj d, ToSVObj e, ToSVObj f, ToSVObj g]

fromSVFromSVArray :: (FromSV a, MonadCatch m, MonadIO m) => SVArray -> PerlT s m a
fromSVFromSVArray svArray = case elems svArray of
  [] -> fromSVNon
  _ -> fromSV (svArray ! snd (bounds svArray))

instance FromSVArray () where fromSVArray _ = return ()
instance FromSVArray SV where fromSVArray = fromSVFromSVArray
instance FromSVArray Int where fromSVArray = fromSVFromSVArray
instance FromSVArray Double where fromSVArray = fromSVFromSVArray
instance FromSVArray String where fromSVArray = fromSVFromSVArray
instance FromSVArray RefSV where fromSVArray = fromSVFromSVArray
instance FromSVArray RefAV where fromSVArray = fromSVFromSVArray
instance FromSVArray RefHV where fromSVArray = fromSVFromSVArray
instance FromSVArray RefCV where fromSVArray = fromSVFromSVArray
instance FromSVArray SVArray where fromSVArray = duplicateSVArray asSV

instance FromSV a => FromSVArray [a] where
  fromSVArray = mapM fromSV . elems
