{-# LANGUAGE Rank2Types #-}
module Perl.SVArray
  ( SVArray
  , SVArrayBuilder
  , SVArrayBreaker
  , addToSV
  , addFromSV
  , buildSVArray
  , breakSVArray
  ) where

import Control.Monad
import Control.Monad.IO.Class

import Data.Monoid
import Data.Array.MArray
import Data.Array.Storable

import Perl.Type
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

buildSVArray :: MonadIO m => SVArrayBuilder -> PerlT s m SVArray
buildSVArray builder = do
  svList <- forM (unSVArrayBuilder builder []) toSV
  liftIO $ newListArray (1, length svList) svList

-- return the unconsumed SVs
breakSVArray :: MonadIO m => SVArrayBreaker s m -> SVArray -> PerlT s m [SV]
breakSVArray breakers svArray = do
  svList <- liftIO $ getElems svArray
  foldM (\svs br -> br svs) svList (unSVArrayBreaker breakers [])
