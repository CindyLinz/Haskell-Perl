{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Perl.Glue
  where

import Foreign

type StrLen = Int

data PerlInterpreter
type PtrPerl = Ptr PerlInterpreter

data SV
type PtrSV = Ptr SV

data AV
type PtrAV = Ptr AV

data HV
type PtrHV = Ptr HV

foreign import ccall unsafe
  init_perl :: IO PtrPerl

foreign import ccall unsafe
  exit_perl :: PtrPerl -> IO ()


foreign import ccall unsafe
  "Perl_newSV" perl_newSV :: PtrPerl -> StrLen -> IO PtrSV


foreign import ccall unsafe
  "S_SvREFCNT_dec" s_SvREFCNT_dec :: PtrPerl -> PtrSV -> IO ()

foreign import ccall unsafe
  "S_SvREFCNT_dec_NN" s_SvREFCNT_dec_NN :: PtrPerl -> PtrSV -> IO ()

foreign import ccall unsafe
  "S_SvREFCNT_inc" s_SvREFCNT_inc :: PtrSV -> IO PtrSV

foreign import ccall unsafe
  "S_SvREFCNT_inc_NN" s_SvREFCNT_inc_NN :: PtrSV -> IO PtrSV

foreign import ccall unsafe
  "S_SvREFCNT_inc_void" s_SvREFCNT_inc_void :: PtrSV -> IO ()
