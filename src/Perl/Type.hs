{-# LANGUAGE EmptyDataDecls #-}
module Perl.Type
  where

import Foreign
import Foreign.C.Types
import Foreign.C.String

type StrLen = CSize
type IV = CIntMax
type NV = CDouble

data PerlInterpreter
type PtrPerl = Ptr PerlInterpreter

data SV
type PtrSV = Ptr SV

data AV
type PtrAV = Ptr AV

data HV
type PtrHV = Ptr HV
