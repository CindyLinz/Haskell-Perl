{-# LANGUAGE ForeignFunctionInterface #-}
module Perl.Glue
  where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Perl.Type

foreign export ccall
  freeHaskellFunPtr :: FunPtr a -> IO ()

------
-- stdlib

foreign import ccall unsafe
  "&free" p_free :: FunPtr (Ptr a -> IO ())

------
-- init / exit

foreign import ccall safe
  init_perl :: IO PtrPerl

foreign import ccall safe
  exit_perl :: PtrPerl -> IO ()

------
-- ref count

foreign import ccall safe
  svREFCNT_dec :: PtrPerl -> PtrSV -> IO PtrSV

--foreign import ccall unsafe
--  svREFCNT_dec_NN :: PtrPerl -> PtrSV -> IO PtrSV

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

foreign import ccall unsafe
  "Perl_sv_newmortal" perl_sv_newmortal :: PtrPerl -> IO PtrSV

foreign import ccall unsafe
  newSViv_mortal :: PtrPerl -> IV -> IO PtrSV

foreign import ccall unsafe
  newSVnv_mortal :: PtrPerl -> NV -> IO PtrSV

foreign import ccall unsafe
  "Perl_sv_mortalcopy" perl_sv_mortalcopy :: PtrPerl -> PtrSV -> IO PtrSV

------
-- write SV

foreign import ccall safe
  "Perl_sv_setiv_mg" perl_sv_setiv_mg :: PtrPerl -> PtrSV -> IV -> IO ()

foreign import ccall safe
  "Perl_sv_setnv_mg" perl_sv_setnv_mg :: PtrPerl -> PtrSV -> NV -> IO ()

foreign import ccall safe
  "Perl_sv_setpvn_mg" perl_sv_setpvn_mg :: PtrPerl -> PtrSV -> CString -> StrLen -> IO ()

foreign import ccall safe
  "Perl_sv_setsv_mg" perl_sv_setsv_mg :: PtrPerl -> PtrSV -> PtrSV -> IO ()

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
  wrap_sub :: PtrPerl -> FunPtr (PtrPerl -> PtrCV -> IO ()) -> IO PtrSV

foreign import ccall unsafe
  get_sub_arg_num :: PtrPerl -> IO CInt

foreign import ccall unsafe
  get_sub_args :: PtrPerl -> Ptr PtrSV -> CInt -> IO ()

foreign import ccall unsafe
  set_sub_returns :: PtrPerl -> Ptr PtrSV -> CInt -> IO()
