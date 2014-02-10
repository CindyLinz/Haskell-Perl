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

incRefCnt :: MonadIO m => PtrSV -> PerlT s m ()
incRefCnt = liftIO . svREFCNT_inc_void_NN

decRefCnt :: MonadIO m => PtrSV -> PerlT s m ()
decRefCnt sv = PerlT $ \perl frames -> do
  liftIO (svREFCNT_dec perl sv)
  return (frames, ())

------
-- new SV

newSV :: MonadIO m => PerlT s m PtrSV
newSV = PerlT $ \perl (frame:frames) -> do
  sv <- liftIO $ perl_newSV perl 0
  return ((sv:frame):frames, sv)

newIntSV :: MonadIO m => IV -> PerlT s m PtrSV
newIntSV iv = PerlT $ \perl (frame:frames) -> do
  sv <- liftIO $ perl_newSViv perl iv
  return ((sv:frame):frames, sv)

newNumSV :: MonadIO m => NV -> PerlT s m PtrSV
newNumSV nv = PerlT $ \perl (frame:frames) -> do
  sv <- liftIO $ perl_newSVnv perl nv
  return ((sv:frame):frames, sv)

newStrSV :: MonadIO m => Ptr CChar -> StrLen -> PerlT s m PtrSV
newStrSV str len = PerlT $ \perl (frame:frames) -> do
  sv <- liftIO $ perl_newSVpvn_flags perl str len 0
  return ((sv:frame):frames, sv)

newSVMortal :: MonadIO m => PerlT s m PtrSV
newSVMortal = PerlT $ \perl frames -> do
  sv <- liftIO $ perl_sv_newmortal perl
  return (frames, sv)

newIntSVMortal :: MonadIO m => IV -> PerlT s m PtrSV
newIntSVMortal iv = PerlT $ \perl frames -> do
  sv <- liftIO $ newSViv_mortal perl iv
  return (frames, sv)

newNumSVMortal :: MonadIO m => NV -> PerlT s m PtrSV
newNumSVMortal nv = PerlT $ \perl frames -> do
  sv <- liftIO $ newSVnv_mortal perl nv
  return (frames, sv)

newStrSVMortal :: MonadIO m => Ptr CChar -> StrLen -> PerlT s m PtrSV
newStrSVMortal str len = PerlT $ \perl frames -> do
  sv <- liftIO $ perl_newSVpvn_flags perl str len const_SVs_TEMP
  return (frames, sv)

newSVSVMortal :: MonadIO m => PtrSV -> PerlT s m PtrSV
newSVSVMortal old = PerlT $ \perl frames -> do
  sv <- liftIO $ perl_sv_mortalcopy perl old
  return (frames, sv)

------
-- read SV

svToInt :: MonadIO m => PtrSV -> PerlT s m IV
svToInt sv = PerlT $ \perl frames -> do
  a <- liftIO $ svIVx perl sv
  return (frames, a)

svToNum :: MonadIO m => PtrSV -> PerlT s m NV
svToNum sv = PerlT $ \perl frames -> do
  a <- liftIO $ svNVx perl sv
  return (frames, a)

-- the returned CStringLen is just temporary that is handled by perl
svToStr :: MonadIO m => PtrSV -> PerlT s m CStringLen
svToStr sv = PerlT $ \perl frames -> liftIO $ alloca $ \ptrLen -> do
  ptrStr <- svPVbytex perl sv ptrLen
  len <- peek ptrLen
  return (frames, (ptrStr, fromIntegral len))

------
-- write SV

setSVInt :: MonadIO m => PtrSV -> IV -> PerlT s m ()
setSVInt sv a = PerlT $ \perl frames -> do
  liftIO $ perl_sv_setiv_mg perl sv a
  return (frames, ())

setSVNum :: MonadIO m => PtrSV -> NV -> PerlT s m ()
setSVNum sv a = PerlT $ \perl frames -> do
  liftIO $ perl_sv_setnv_mg perl sv a
  return (frames, ())

setSVStr :: MonadIO m => PtrSV -> Ptr CChar -> StrLen -> PerlT s m ()
setSVStr sv ptrStr len = PerlT $ \perl frames -> do
  liftIO $ perl_sv_setpvn_mg perl sv ptrStr len
  return (frames, ())

setSVSV :: MonadIO m => PtrSV -> PtrSV -> PerlT s m ()
setSVSV dst src = PerlT $ \perl frames -> do
  liftIO $ perl_sv_setsv_mg perl dst src
  return (frames, ())

------
-- ref

newSVRef :: MonadIO m => PtrSV -> PerlT s m RefSV
newSVRef sv = PerlT $ \perl (frame:frames) -> do
  rv <- liftIO $ perl_newSVRV perl sv
  return ((castPtr rv:frame):frames, rv)

newAVRef :: MonadIO m => PtrAV -> PerlT s m RefAV
newAVRef av = PerlT $ \perl (frame:frames) -> do
  rv <- liftIO $ perl_newAVRV perl av
  return ((castPtr av:frame):frames, rv)

newHVRef :: MonadIO m => PtrHV -> PerlT s m RefHV
newHVRef hv = PerlT $ \perl (frame:frames) -> do
  rv <- liftIO $ perl_newHVRV perl hv
  return ((castPtr rv:frame):frames, rv)

deRefSV :: MonadIO m => RefSV -> PerlT s m PtrSV
deRefSV = liftIO . svRV

deRefAV :: MonadIO m => RefAV -> PerlT s m PtrAV
deRefAV = liftIO . avRV

deRefHV :: MonadIO m => RefHV -> PerlT s m PtrHV
deRefHV = liftIO . hvRV

------
-- eval

eval :: MonadIO m => CString -> PerlT s m PtrSV
eval code = PerlT $ \perl frames -> do
  a <- liftIO $ perl_eval_pv perl code 1
  return (frames, a)

------
-- call

callVar :: MonadIO m => PtrSV -> CInt -> PerlT s m CInt
callVar sv flag = PerlT $ \perl frames -> do
  a <- liftIO $ perl_call_sv perl sv flag
  return (frames, a)

callName :: (Ix i, Ix j, Num j, MonadIO m) => CString -> CInt -> StorableArray i PtrSV -> PerlT s m (StorableArray j PtrSV)
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

getSubArgs :: MonadIO m => PerlSubT s m (StorableArray Int PtrSV)
getSubArgs = PerlSubT $ \perl cv -> liftIO $ do
  items <- get_sub_arg_num perl
  args <- newArray_ (1, fromIntegral items)
  withStorableArray args $ \ptrArgs ->
    get_sub_args perl ptrArgs items
  return args

-- must be the last step in the sub that modify the perl stack
setSubReturns :: MonadIO m => StorableArray Int PtrSV -> PerlSubT s m ()
setSubReturns returns = PerlSubT $ \perl cv -> liftIO $ do
  (a, b) <- getBounds returns
  withStorableArray returns $ \ptrReturns -> do
    set_sub_returns perl ptrReturns (fromIntegral (b - a + 1))
