{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Perl.Glue
  where

import Foreign
import Foreign.C.String

type StrLen = Int

data PerlInterpreter
type PtrPerl = Ptr PerlInterpreter

data SV
type PtrSV = Ptr SV

data AV
type PtrAV = Ptr AV

data HV
type PtrHV = Ptr HV

------
-- init / exit

foreign import ccall unsafe
  init_perl :: IO PtrPerl

foreign import ccall unsafe
  exit_perl :: PtrPerl -> IO ()

------
-- new var

foreign import ccall unsafe
  "Perl_newSV" perl_newSV :: PtrPerl -> StrLen -> IO PtrSV

------
-- ref count

foreign import ccall unsafe
  "Perl_sv_free" perl_sv_free :: PtrPerl -> PtrSV -> IO ()

foreign import ccall unsafe
  svREFCNT_inc :: PtrSV -> IO PtrSV

foreign import ccall unsafe
  svREFCNT_inc_NN :: PtrSV -> IO PtrSV

foreign import ccall unsafe
  svREFCNT_inc_void :: PtrSV -> IO ()

------
-- eval

foreign import ccall safe
  "Perl_eval_pv" perl_eval_pv :: PtrPerl -> CString -> Int32 -> IO PtrSV
