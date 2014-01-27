{-# LANGUAGE ForeignFunctionInterface #-}
module Perl.Glue
  where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Perl.Type

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

foreign import ccall safe
  "Perl_sv_free" perl_sv_free :: PtrPerl -> PtrSV -> IO ()

foreign import ccall unsafe
  svREFCNT_inc :: PtrSV -> IO PtrSV

foreign import ccall unsafe
  svREFCNT_inc_NN :: PtrSV -> IO PtrSV

foreign import ccall unsafe
  svREFCNT_inc_void :: PtrSV -> IO ()

------
-- read SV

foreign import ccall safe
  svIVx :: PtrPerl -> PtrSV -> IO IV

foreign import ccall safe
  svNVx :: PtrPerl -> PtrSV -> IO NV

foreign import ccall safe
  svPVbytex :: PtrPerl -> PtrSV -> Ptr StrLen -> IO (Ptr CChar)

------
-- eval

foreign import ccall safe
  "Perl_eval_pv" perl_eval_pv :: PtrPerl -> CString -> Int32 -> IO PtrSV
