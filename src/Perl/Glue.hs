{-# LANGUAGE ForeignFunctionInterface #-}
module Perl.Glue
  where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Perl.Type

------
-- stdlib

foreign import ccall unsafe
  "&free" p_free :: FunPtr (Ptr a -> IO ())

------
-- init / exit

foreign import ccall unsafe
  init_perl :: IO PtrPerl

foreign import ccall unsafe
  exit_perl :: PtrPerl -> IO ()

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

foreign import ccall unsafe
  svREFCNT_inc_void_NN :: PtrSV -> IO ()

------
-- new SV

foreign import ccall unsafe
  "Perl_newSV" perl_newSV :: PtrPerl -> StrLen -> IO PtrSV

foreign import ccall unsafe
  "Perl_newSViv" perl_newSViv :: PtrPerl -> IV -> IO PtrSV

foreign import ccall unsafe
  "Perl_newSVnv" perl_newSVnv :: PtrPerl -> NV -> IO PtrSV

foreign import ccall unsafe
  "Perl_newSVpvn_flags" perl_newSVpvn_flags :: PtrPerl -> CString -> StrLen -> CUInt -> IO PtrSV

------
-- write SV

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
  "Perl_eval_pv" perl_eval_pv :: PtrPerl -> CString -> CInt -> IO PtrSV

------
-- call

foreign import ccall safe
  "Perl_call_sv" perl_call_sv :: PtrPerl -> PtrSV -> CInt -> IO CInt

foreign import ccall safe
  "Perl_call_pv" perl_call_pv :: PtrPerl -> CString -> CInt -> IO CInt

foreign import ccall safe
  glue_call_pv :: PtrPerl -> CString -> CInt -> CInt -> Ptr PtrSV -> Ptr (Ptr PtrSV) -> IO CInt

------
-- sub wrapper

foreign import ccall unsafe
  "wrapper" wrap_sub_wrapper :: (PtrPerl -> PtrCV -> IO ()) -> IO (FunPtr (PtrPerl -> PtrCV -> IO ()))

foreign import ccall unsafe
  wrap_sub :: PtrPerl -> (FunPtr (PtrPerl -> PtrCV -> IO ())) -> IO PtrCV
