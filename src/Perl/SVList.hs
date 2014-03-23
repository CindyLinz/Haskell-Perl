{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
module Perl.SVList
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Perl.Type
import Perl.Class
import Perl.Monad
import Perl.SV

instance ToSV a => ToSVList [a] where
  toSVList = mapM toSV
  toSVMortalList = mapM toSVMortal
  asSVList = mapM asSV

instance ToSVList () where
  toSVList _ = return []
  toSVMortalList _ = return []
  asSVList _ = return []

singletonToSVList :: (ToSV a, MonadCatch m, MonadIO m) => a -> PerlT s m [SV]
singletonToSVList a = toSV a >>= return . pure

singletonToSVMortalList :: (ToSV a, MonadCatch m, MonadIO m) => a -> PerlT s m [SV]
singletonToSVMortalList a = toSVMortal a >>= return . pure

singletonAsSVList :: (ToSV a, MonadCatch m, MonadIO m) => a -> PerlT s m [SV]
singletonAsSVList a = asSV a >>= return . pure

instance ToSVList SV where
  toSVList = singletonToSVList
  toSVMortalList = singletonToSVMortalList
  asSVList = singletonAsSVList
instance ToSVList ToSVObj where
  toSVList = singletonToSVList
  toSVMortalList = singletonToSVMortalList
  asSVList = singletonAsSVList
instance ToSVList Int where
  toSVList = singletonToSVList
  toSVMortalList = singletonToSVMortalList
  asSVList = singletonAsSVList
instance ToSVList Double where
  toSVList = singletonToSVList
  toSVMortalList = singletonToSVMortalList
  asSVList = singletonAsSVList
instance ToSVList String where
  toSVList = singletonToSVList
  toSVMortalList = singletonToSVMortalList
  asSVList = singletonAsSVList
instance ToSVList RefSV where
  toSVList = singletonToSVList
  toSVMortalList = singletonToSVMortalList
  asSVList = singletonAsSVList
instance ToSVList RefAV where
  toSVList = singletonToSVList
  toSVMortalList = singletonToSVMortalList
  asSVList = singletonAsSVList
instance ToSVList RefHV where
  toSVList = singletonToSVList
  toSVMortalList = singletonToSVMortalList
  asSVList = singletonAsSVList
instance ToSVList RefCV where
  toSVList = singletonToSVList
  toSVMortalList = singletonToSVMortalList
  asSVList = singletonAsSVList

instance (ToSV a1, ToSV a2) => ToSVList (a1, a2) where
  toSVList (a1, a2) = do
    a1' <- toSV a1
    a2' <- toSV a2
    return [a1', a2']
  toSVMortalList (a1, a2) = do
    a1' <- toSVMortal a1
    a2' <- toSVMortal a2
    return [a1', a2']
  asSVList (a1, a2) = do
    a1' <- asSV a1
    a2' <- asSV a2
    return [a1', a2']
instance (ToSV a1, ToSV a2, ToSV a3) => ToSVList (a1, a2, a3) where
  toSVList (a1, a2, a3) = do
    a1' <- toSV a1
    a2' <- toSV a2
    a3' <- toSV a3
    return [a1', a2', a3']
  toSVMortalList (a1, a2, a3) = do
    a1' <- toSVMortal a1
    a2' <- toSVMortal a2
    a3' <- toSVMortal a3
    return [a1', a2', a3']
  asSVList (a1, a2, a3) = do
    a1' <- asSV a1
    a2' <- asSV a2
    a3' <- asSV a3
    return [a1', a2', a3']
instance (ToSV a1, ToSV a2, ToSV a3, ToSV a4) => ToSVList (a1, a2, a3, a4) where
  toSVList (a1, a2, a3, a4) = do
    a1' <- toSV a1
    a2' <- toSV a2
    a3' <- toSV a3
    a4' <- toSV a4
    return [a1', a2', a3', a4']
  toSVMortalList (a1, a2, a3, a4) = do
    a1' <- toSVMortal a1
    a2' <- toSVMortal a2
    a3' <- toSVMortal a3
    a4' <- toSVMortal a4
    return [a1', a2', a3', a4']
  asSVList (a1, a2, a3, a4) = do
    a1' <- asSV a1
    a2' <- asSV a2
    a3' <- asSV a3
    a4' <- asSV a4
    return [a1', a2', a3', a4']
instance (ToSV a1, ToSV a2, ToSV a3, ToSV a4, ToSV a5) => ToSVList (a1, a2, a3, a4, a5) where
  toSVList (a1, a2, a3, a4, a5) = do
    a1' <- toSV a1
    a2' <- toSV a2
    a3' <- toSV a3
    a4' <- toSV a4
    a5' <- toSV a5
    return [a1', a2', a3', a4', a5']
  toSVMortalList (a1, a2, a3, a4, a5) = do
    a1' <- toSVMortal a1
    a2' <- toSVMortal a2
    a3' <- toSVMortal a3
    a4' <- toSVMortal a4
    a5' <- toSVMortal a5
    return [a1', a2', a3', a4', a5']
  asSVList (a1, a2, a3, a4, a5) = do
    a1' <- asSV a1
    a2' <- asSV a2
    a3' <- asSV a3
    a4' <- asSV a4
    a5' <- asSV a5
    return [a1', a2', a3', a4', a5']
instance (ToSV a1, ToSV a2, ToSV a3, ToSV a4, ToSV a5, ToSV a6) => ToSVList (a1, a2, a3, a4, a5, a6) where
  toSVList (a1, a2, a3, a4, a5, a6) = do
    a1' <- toSV a1
    a2' <- toSV a2
    a3' <- toSV a3
    a4' <- toSV a4
    a5' <- toSV a5
    a6' <- toSV a6
    return [a1', a2', a3', a4', a5', a6']
  toSVMortalList (a1, a2, a3, a4, a5, a6) = do
    a1' <- toSVMortal a1
    a2' <- toSVMortal a2
    a3' <- toSVMortal a3
    a4' <- toSVMortal a4
    a5' <- toSVMortal a5
    a6' <- toSVMortal a6
    return [a1', a2', a3', a4', a5', a6']
  asSVList (a1, a2, a3, a4, a5, a6) = do
    a1' <- asSV a1
    a2' <- asSV a2
    a3' <- asSV a3
    a4' <- asSV a4
    a5' <- asSV a5
    a6' <- asSV a6
    return [a1', a2', a3', a4', a5', a6']
instance (ToSV a1, ToSV a2, ToSV a3, ToSV a4, ToSV a5, ToSV a6, ToSV a7) => ToSVList (a1, a2, a3, a4, a5, a6, a7) where
  toSVList (a1, a2, a3, a4, a5, a6, a7) = do
    a1' <- toSV a1
    a2' <- toSV a2
    a3' <- toSV a3
    a4' <- toSV a4
    a5' <- toSV a5
    a6' <- toSV a6
    a7' <- toSV a7
    return [a1', a2', a3', a4', a5', a6', a7']
  toSVMortalList (a1, a2, a3, a4, a5, a6, a7) = do
    a1' <- toSVMortal a1
    a2' <- toSVMortal a2
    a3' <- toSVMortal a3
    a4' <- toSVMortal a4
    a5' <- toSVMortal a5
    a6' <- toSVMortal a6
    a7' <- toSVMortal a7
    return [a1', a2', a3', a4', a5', a6', a7']
  asSVList (a1, a2, a3, a4, a5, a6, a7) = do
    a1' <- asSV a1
    a2' <- asSV a2
    a3' <- asSV a3
    a4' <- asSV a4
    a5' <- asSV a5
    a6' <- asSV a6
    a7' <- asSV a7
    return [a1', a2', a3', a4', a5', a6', a7']
instance (ToSV a1, ToSV a2, ToSV a3, ToSV a4, ToSV a5, ToSV a6, ToSV a7, ToSV a8) => ToSVList (a1, a2, a3, a4, a5, a6, a7, a8) where
  toSVList (a1, a2, a3, a4, a5, a6, a7, a8) = do
    a1' <- toSV a1
    a2' <- toSV a2
    a3' <- toSV a3
    a4' <- toSV a4
    a5' <- toSV a5
    a6' <- toSV a6
    a7' <- toSV a7
    a8' <- toSV a8
    return [a1', a2', a3', a4', a5', a6', a7', a8']
  toSVMortalList (a1, a2, a3, a4, a5, a6, a7, a8) = do
    a1' <- toSVMortal a1
    a2' <- toSVMortal a2
    a3' <- toSVMortal a3
    a4' <- toSVMortal a4
    a5' <- toSVMortal a5
    a6' <- toSVMortal a6
    a7' <- toSVMortal a7
    a8' <- toSVMortal a8
    return [a1', a2', a3', a4', a5', a6', a7', a8']
  asSVList (a1, a2, a3, a4, a5, a6, a7, a8) = do
    a1' <- asSV a1
    a2' <- asSV a2
    a3' <- asSV a3
    a4' <- asSV a4
    a5' <- asSV a5
    a6' <- asSV a6
    a7' <- asSV a7
    a8' <- asSV a8
    return [a1', a2', a3', a4', a5', a6', a7', a8']
instance (ToSV a1, ToSV a2, ToSV a3, ToSV a4, ToSV a5, ToSV a6, ToSV a7, ToSV a8, ToSV a9) => ToSVList (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  toSVList (a1, a2, a3, a4, a5, a6, a7, a8, a9) = do
    a1' <- toSV a1
    a2' <- toSV a2
    a3' <- toSV a3
    a4' <- toSV a4
    a5' <- toSV a5
    a6' <- toSV a6
    a7' <- toSV a7
    a8' <- toSV a8
    a9' <- toSV a9
    return [a1', a2', a3', a4', a5', a6', a7', a8', a9']
  toSVMortalList (a1, a2, a3, a4, a5, a6, a7, a8, a9) = do
    a1' <- toSVMortal a1
    a2' <- toSVMortal a2
    a3' <- toSVMortal a3
    a4' <- toSVMortal a4
    a5' <- toSVMortal a5
    a6' <- toSVMortal a6
    a7' <- toSVMortal a7
    a8' <- toSVMortal a8
    a9' <- toSVMortal a9
    return [a1', a2', a3', a4', a5', a6', a7', a8', a9']
  asSVList (a1, a2, a3, a4, a5, a6, a7, a8, a9) = do
    a1' <- asSV a1
    a2' <- asSV a2
    a3' <- asSV a3
    a4' <- asSV a4
    a5' <- asSV a5
    a6' <- asSV a6
    a7' <- asSV a7
    a8' <- asSV a8
    a9' <- asSV a9
    return [a1', a2', a3', a4', a5', a6', a7', a8', a9']
instance (ToSV a1, ToSV a2, ToSV a3, ToSV a4, ToSV a5, ToSV a6, ToSV a7, ToSV a8, ToSV a9, ToSV a10) => ToSVList (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  toSVList (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = do
    a1' <- toSV a1
    a2' <- toSV a2
    a3' <- toSV a3
    a4' <- toSV a4
    a5' <- toSV a5
    a6' <- toSV a6
    a7' <- toSV a7
    a8' <- toSV a8
    a9' <- toSV a9
    a10' <- toSV a10
    return [a1', a2', a3', a4', a5', a6', a7', a8', a9', a10']
  toSVMortalList (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = do
    a1' <- toSVMortal a1
    a2' <- toSVMortal a2
    a3' <- toSVMortal a3
    a4' <- toSVMortal a4
    a5' <- toSVMortal a5
    a6' <- toSVMortal a6
    a7' <- toSVMortal a7
    a8' <- toSVMortal a8
    a9' <- toSVMortal a9
    a10' <- toSVMortal a10
    return [a1', a2', a3', a4', a5', a6', a7', a8', a9', a10']
  asSVList (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = do
    a1' <- asSV a1
    a2' <- asSV a2
    a3' <- asSV a3
    a4' <- asSV a4
    a5' <- asSV a5
    a6' <- asSV a6
    a7' <- asSV a7
    a8' <- asSV a8
    a9' <- asSV a9
    a10' <- asSV a10
    return [a1', a2', a3', a4', a5', a6', a7', a8', a9', a10']

instance FromSVList [SV] where fromSVList = mapM fromSV
instance FromSVList [Int] where fromSVList = mapM fromSV
instance FromSVList [Double] where fromSVList = mapM fromSV
instance FromSVList [String] where fromSVList = mapM fromSV
