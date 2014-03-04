{-# LANGUAGE ForeignFunctionInterface #-}
module Perl.Internal.Glue
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

foreign import ccall unsafe
  svREFCNT :: SV -> IO CUInt

foreign import ccall safe
  svREFCNT_dec :: PtrPerl -> SV -> IO SV

--foreign import ccall unsafe
--  svREFCNT_dec_NN :: PtrPerl -> SV -> IO SV

foreign import ccall unsafe
  svREFCNT_inc :: SV -> IO SV

foreign import ccall unsafe
  svREFCNT_inc_NN :: SV -> IO SV

foreign import ccall unsafe
  svREFCNT_inc_void :: SV -> IO ()

foreign import ccall unsafe
  svREFCNT_inc_void_NN :: SV -> IO ()

------
-- new SV

foreign import ccall unsafe
  "Perl_newSV" perl_newSV :: PtrPerl -> StrLen -> IO SV

foreign import ccall safe
  "Perl_newSVsv" perl_newSVsv :: PtrPerl -> SV -> IO SV

foreign import ccall unsafe
  "Perl_newSViv" perl_newSViv :: PtrPerl -> IV -> IO SV

foreign import ccall unsafe
  "Perl_newSVnv" perl_newSVnv :: PtrPerl -> NV -> IO SV

foreign import ccall unsafe
  "Perl_newSVpvn_flags" perl_newSVpvn_flags :: PtrPerl -> CString -> StrLen -> CUInt -> IO SV

foreign import ccall unsafe
  "Perl_sv_newmortal" perl_sv_newmortal :: PtrPerl -> IO SV

foreign import ccall unsafe
  newSViv_mortal :: PtrPerl -> IV -> IO SV

foreign import ccall unsafe
  newSVnv_mortal :: PtrPerl -> NV -> IO SV

foreign import ccall unsafe
  "Perl_sv_mortalcopy" perl_sv_mortalcopy :: PtrPerl -> SV -> IO SV

------
-- write SV

foreign import ccall safe
  perl_sv_setundef :: PtrPerl -> SV -> IO ()

foreign import ccall safe
  "Perl_sv_setiv_mg" perl_sv_setiv_mg :: PtrPerl -> SV -> IV -> IO ()

foreign import ccall safe
  "Perl_sv_setnv_mg" perl_sv_setnv_mg :: PtrPerl -> SV -> NV -> IO ()

foreign import ccall safe
  "Perl_sv_setpvn_mg" perl_sv_setpvn_mg :: PtrPerl -> SV -> CString -> StrLen -> IO ()

foreign import ccall safe
  "Perl_sv_setsv_mg" perl_sv_setsv_mg :: PtrPerl -> SV -> SV -> IO ()

------
-- read SV

foreign import ccall safe
  svIVx :: PtrPerl -> SV -> IO IV

foreign import ccall safe
  svNVx :: PtrPerl -> SV -> IO NV

foreign import ccall safe
  svPVbytex :: PtrPerl -> SV -> Ptr StrLen -> IO (Ptr CChar)

------
-- ref

foreign import ccall unsafe
  "Perl_newRV" perl_newSVRV :: PtrPerl -> SV -> IO RefSV

foreign import ccall unsafe
  "Perl_newRV" perl_newAVRV :: PtrPerl -> AV -> IO RefAV

foreign import ccall unsafe
  "Perl_newRV" perl_newHVRV :: PtrPerl -> HV -> IO RefHV

foreign import ccall unsafe
  "svRV" svRV :: RefSV -> IO SV

foreign import ccall unsafe
  "svRV" avRV :: RefAV -> IO AV

foreign import ccall unsafe
  "svRV" hvRV :: RefHV -> IO HV

foreign import ccall unsafe
  rvTYPE :: SV -> IO CInt

------
-- AV

foreign import ccall unsafe
  "Perl_newAV" perl_newAV :: PtrPerl -> IO AV

foreign import ccall safe
  "Perl_av_make" perl_av_make :: PtrPerl -> CInt -> Ptr SV -> IO AV

foreign import ccall safe
  "Perl_av_clear" perl_av_clear :: PtrPerl -> AV -> IO ()

foreign import ccall safe
  "Perl_av_fetch" perl_av_fetch :: PtrPerl -> AV -> CInt -> CUInt -> IO (Ptr SV)

foreign import ccall safe
  "Perl_av_store" perl_av_store :: PtrPerl -> AV -> CInt -> SV -> IO (Ptr SV)

foreign import ccall safe
  "Perl_av_exists" perl_av_exists :: PtrPerl -> AV -> CInt -> IO Bool

foreign import ccall safe
  "Perl_av_len" perl_av_len :: PtrPerl -> AV -> IO CInt

foreign import ccall safe
  "Perl_av_push" perl_av_push :: PtrPerl -> AV -> SV -> IO ()

foreign import ccall safe
  perl_av_unshift :: PtrPerl -> AV -> SV -> IO ()

foreign import ccall safe
  "Perl_av_pop" perl_av_pop :: PtrPerl -> AV -> IO SV

foreign import ccall safe
  "Perl_av_shift" perl_av_shift :: PtrPerl -> AV -> IO SV

------
-- HV

foreign import ccall unsafe
  perl_newHV :: PtrPerl -> IO HV

foreign import ccall safe
  "Perl_hv_clear" perl_hv_clear :: PtrPerl -> HV -> IO ()

foreign import ccall safe
  perl_hv_peek :: PtrPerl -> HV -> CString -> StrLen -> IO (Ptr SV)

foreign import ccall safe
  perl_hv_fetch :: PtrPerl -> HV -> CString -> StrLen -> IO (Ptr SV)

foreign import ccall safe
  perl_hv_exists :: PtrPerl -> HV -> CString -> StrLen -> IO Bool

foreign import ccall safe
  perl_hv_store :: PtrPerl -> HV -> CString -> StrLen -> SV -> IO (Ptr SV)

foreign import ccall safe
  perl_hv_delete :: PtrPerl -> HV -> CString -> StrLen -> IO SV

------
-- eval

foreign import ccall safe
  glue_eval_sv :: PtrPerl -> SV -> CInt -> Ptr (Ptr SV) -> IO CInt

foreign import ccall safe
  glue_eval_pv :: PtrPerl -> CString -> StrLen -> CInt -> Ptr (Ptr SV) -> IO CInt

------
-- call

foreign import ccall safe
  "Perl_call_sv" perl_call_sv :: PtrPerl -> SV -> CInt -> IO CInt

foreign import ccall safe
  "Perl_call_pv" perl_call_pv :: PtrPerl -> CString -> CInt -> IO CInt

foreign import ccall safe
  glue_call_pv :: PtrPerl -> CString -> StrLen -> CInt -> CInt -> Ptr SV -> Ptr (Ptr SV) -> IO CInt

foreign import ccall unsafe
  glue_get_error :: PtrPerl -> IO SV

------
-- sub wrapper

foreign import ccall unsafe
  "wrapper" wrap_sub_wrapper :: (PtrPerl -> CV -> IO SV) -> IO (FunPtr (PtrPerl -> CV -> IO SV))

foreign import ccall unsafe
  wrap_sub :: PtrPerl -> FunPtr (PtrPerl -> CV -> IO SV) -> IO RefCV

foreign import ccall safe
  reg_sub :: PtrPerl -> CString -> FunPtr (PtrPerl -> CV -> IO SV) -> IO ()

foreign import ccall unsafe
  get_sub_arg_num :: PtrPerl -> IO CInt

foreign import ccall unsafe
  get_sub_args :: PtrPerl -> Ptr SV -> CInt -> IO ()

foreign import ccall unsafe
  set_sub_returns :: PtrPerl -> Ptr SV -> CInt -> IO ()

foreign import ccall unsafe
  get_sub_context :: PtrPerl -> IO CInt

------
-- embed

foreign import ccall unsafe
  perl_pad_peek_pvn :: PtrPerl -> CString -> StrLen -> IO SV
