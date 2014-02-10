module Perl.MonadGlue
  where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import Control.Monad
import Control.Monad.IO.Class

import Data.Array.Storable hiding (unsafeForeignPtrToStorableArray)
import Data.Array.Unsafe
import Data.Ix

import Perl.Type
import Perl.Constant
import Perl.Glue
import Perl.Monad

------
-- ref count

incRefCnt :: MonadIO m => SV -> PerlT s m ()
incRefCnt = liftIO . svREFCNT_inc_void_NN

decRefCnt :: MonadIO m => SV -> PerlT s m ()
decRefCnt sv = PerlT $ \perl frames -> do
  liftIO (svREFCNT_dec perl sv)
  return (frames, ())

------
-- new SV

newSV :: MonadIO m => PerlT s m SV
newSV = PerlT $ \perl (frame:frames) -> do
  sv <- liftIO $ perl_newSV perl 0
  return ((sv:frame):frames, sv)

newIntSV :: MonadIO m => IV -> PerlT s m SV
newIntSV iv = PerlT $ \perl (frame:frames) -> do
  sv <- liftIO $ perl_newSViv perl iv
  return ((sv:frame):frames, sv)

newNumSV :: MonadIO m => NV -> PerlT s m SV
newNumSV nv = PerlT $ \perl (frame:frames) -> do
  sv <- liftIO $ perl_newSVnv perl nv
  return ((sv:frame):frames, sv)

newStrSV :: MonadIO m => Ptr CChar -> StrLen -> PerlT s m SV
newStrSV str len = PerlT $ \perl (frame:frames) -> do
  sv <- liftIO $ perl_newSVpvn_flags perl str len 0
  return ((sv:frame):frames, sv)

newSVMortal :: MonadIO m => PerlT s m SV
newSVMortal = PerlT $ \perl frames -> do
  sv <- liftIO $ perl_sv_newmortal perl
  return (frames, sv)

newIntSVMortal :: MonadIO m => IV -> PerlT s m SV
newIntSVMortal iv = PerlT $ \perl frames -> do
  sv <- liftIO $ newSViv_mortal perl iv
  return (frames, sv)

newNumSVMortal :: MonadIO m => NV -> PerlT s m SV
newNumSVMortal nv = PerlT $ \perl frames -> do
  sv <- liftIO $ newSVnv_mortal perl nv
  return (frames, sv)

newStrSVMortal :: MonadIO m => Ptr CChar -> StrLen -> PerlT s m SV
newStrSVMortal str len = PerlT $ \perl frames -> do
  sv <- liftIO $ perl_newSVpvn_flags perl str len const_SVs_TEMP
  return (frames, sv)

newSVSVMortal :: MonadIO m => SV -> PerlT s m SV
newSVSVMortal old = PerlT $ \perl frames -> do
  sv <- liftIO $ perl_sv_mortalcopy perl old
  return (frames, sv)

------
-- read SV

svToInt :: MonadIO m => SV -> PerlT s m IV
svToInt sv = PerlT $ \perl frames -> do
  a <- liftIO $ svIVx perl sv
  return (frames, a)

svToNum :: MonadIO m => SV -> PerlT s m NV
svToNum sv = PerlT $ \perl frames -> do
  a <- liftIO $ svNVx perl sv
  return (frames, a)

-- the returned CStringLen is just temporary that is handled by perl
svToStr :: MonadIO m => SV -> PerlT s m CStringLen
svToStr sv = PerlT $ \perl frames -> liftIO $ alloca $ \ptrLen -> do
  ptrStr <- svPVbytex perl sv ptrLen
  len <- peek ptrLen
  return (frames, (ptrStr, fromIntegral len))

------
-- write SV

setSVInt :: MonadIO m => SV -> IV -> PerlT s m ()
setSVInt sv a = PerlT $ \perl frames -> do
  liftIO $ perl_sv_setiv_mg perl sv a
  return (frames, ())

setSVNum :: MonadIO m => SV -> NV -> PerlT s m ()
setSVNum sv a = PerlT $ \perl frames -> do
  liftIO $ perl_sv_setnv_mg perl sv a
  return (frames, ())

setSVStr :: MonadIO m => SV -> Ptr CChar -> StrLen -> PerlT s m ()
setSVStr sv ptrStr len = PerlT $ \perl frames -> do
  liftIO $ perl_sv_setpvn_mg perl sv ptrStr len
  return (frames, ())

setSVSV :: MonadIO m => SV -> SV -> PerlT s m ()
setSVSV dst src = PerlT $ \perl frames -> do
  liftIO $ perl_sv_setsv_mg perl dst src
  return (frames, ())

------
-- ref

newSVRef :: MonadIO m => SV -> PerlT s m RefSV
newSVRef sv = PerlT $ \perl (frame:frames) -> do
  rv <- liftIO $ perl_newSVRV perl sv
  return ((castPtr rv:frame):frames, rv)

newAVRef :: MonadIO m => AV -> PerlT s m RefAV
newAVRef av = PerlT $ \perl (frame:frames) -> do
  rv <- liftIO $ perl_newAVRV perl av
  return ((castPtr av:frame):frames, rv)

newHVRef :: MonadIO m => HV -> PerlT s m RefHV
newHVRef hv = PerlT $ \perl (frame:frames) -> do
  rv <- liftIO $ perl_newHVRV perl hv
  return ((castPtr rv:frame):frames, rv)

deRefSV :: MonadIO m => RefSV -> PerlT s m SV
deRefSV = liftIO . svRV

deRefAV :: MonadIO m => RefAV -> PerlT s m AV
deRefAV = liftIO . avRV

deRefHV :: MonadIO m => RefHV -> PerlT s m HV
deRefHV = liftIO . hvRV

rvType :: MonadIO m => SV -> PerlT s m CInt
rvType = liftIO . rvTYPE

------
-- eval

eval :: MonadIO m => CString -> PerlT s m SV
eval code = PerlT $ \perl frames -> do
  a <- liftIO $ perl_eval_pv perl code 1
  return (frames, a)

------
-- call

callVar :: MonadIO m => SV -> CInt -> PerlT s m CInt
callVar sv flag = PerlT $ \perl frames -> do
  a <- liftIO $ perl_call_sv perl sv flag
  return (frames, a)

callName :: (Ix i, Ix j, Num j, MonadIO m) => CString -> CInt -> StorableArray i SV -> PerlT s m (StorableArray j SV)
callName name flag args = PerlT $ \perl frames -> liftIO . withStorableArray args $ \ptrArg -> alloca $ \ptrPtrOut -> do
  argc <- liftM (fromIntegral . rangeSize) (getBounds args)
  outn <- glue_call_pv perl name flag argc ptrArg ptrPtrOut
  if outn == 0
    then do
      fptrNull <- newForeignPtr_ nullPtr
      emptyArray <- unsafeForeignPtrToStorableArray fptrNull (fromIntegral 1, fromIntegral 0)
      return (frames, emptyArray)
    else do
      ptrOut <- peek ptrPtrOut
      fptrOut <- newForeignPtr p_free ptrOut
      outArray <- unsafeForeignPtrToStorableArray fptrOut (fromIntegral 1, fromIntegral outn)
      return (frames, outArray)

------
-- sub

getSubArgs :: MonadIO m => PerlSubT s m (StorableArray Int SV)
getSubArgs = PerlSubT $ \perl cv -> liftIO $ do
  items <- get_sub_arg_num perl
  args <- newArray_ (1, fromIntegral items)
  withStorableArray args $ \ptrArgs ->
    get_sub_args perl ptrArgs items
  return args

-- must be the last step in the sub that modify the perl stack
setSubReturns :: MonadIO m => StorableArray Int SV -> PerlSubT s m ()
setSubReturns returns = PerlSubT $ \perl cv -> liftIO $ do
  (a, b) <- getBounds returns
  withStorableArray returns $ \ptrReturns -> do
    set_sub_returns perl ptrReturns (fromIntegral (b - a + 1))
