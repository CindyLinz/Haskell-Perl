{-# LANGUAGE EmptyDataDecls, ExistentialQuantification, DeriveDataTypeable #-}
module Perl.Type
  where

import Data.Typeable
import Data.Array.Unboxed

import Foreign
import Foreign.C.Types
import Foreign.C.String

type StrLen = CSize
type IV = CIntMax
type NV = CDouble

data PerlInterpreter
type PtrPerl = Ptr PerlInterpreter

data SV_
type SV = Ptr SV_

data AV_
type AV = Ptr AV_

data HV_
type HV = Ptr HV_

data CV_
type CV = Ptr CV_

data RSV_
type RefSV = Ptr RSV_

data RAV_
type RefAV = Ptr RAV_

data RHV_
type RefHV = Ptr RHV_

data RCV_
type RefCV = Ptr RCV_

type SVArray = UArray Int SV

newtype PerlT s m a = PerlT
  { unPerlT :: PtrPerl -> CV -> m ([SV], a)
  }
type Perl s = PerlT s IO

data PerlException = PerlException String SV deriving Typeable
instance Show PerlException where
  show (PerlException msg _) = "PerlException " ++ msg
