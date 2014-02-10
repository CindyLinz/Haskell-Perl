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
-- eval

foreign import ccall safe
  "Perl_eval_pv" perl_eval_pv :: PtrPerl -> CString -> CInt -> IO SV

------
-- call

foreign import ccall safe
  "Perl_call_sv" perl_call_sv :: PtrPerl -> SV -> CInt -> IO CInt

foreign import ccall safe
  "Perl_call_pv" perl_call_pv :: PtrPerl -> CString -> CInt -> IO CInt

foreign import ccall safe
  glue_call_pv :: PtrPerl -> CString -> CInt -> CInt -> Ptr SV -> Ptr (Ptr SV) -> IO CInt

------
-- sub wrapper

foreign import ccall unsafe
  "wrapper" wrap_sub_wrapper :: (PtrPerl -> CV -> IO ()) -> IO (FunPtr (PtrPerl -> CV -> IO ()))

foreign import ccall unsafe
  wrap_sub :: PtrPerl -> FunPtr (PtrPerl -> CV -> IO ()) -> IO RefCV

foreign import ccall safe
  reg_sub :: PtrPerl -> CString -> FunPtr (PtrPerl -> CV -> IO ()) -> IO ()

foreign import ccall unsafe
  get_sub_arg_num :: PtrPerl -> IO CInt

foreign import ccall unsafe
  get_sub_args :: PtrPerl -> Ptr SV -> CInt -> IO ()

foreign import ccall unsafe
  set_sub_returns :: PtrPerl -> Ptr SV -> CInt -> IO()
