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

refCnt :: MonadIO m => SV -> PerlT s m CUInt
refCnt = liftIO . svREFCNT

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
-- AV

newAVEmpty :: MonadIO m => PerlT s m AV
newAVEmpty = PerlT $ \perl (frame:frames) -> do
  a <- liftIO $ perl_newAV perl
  return ((castPtr a:frame):frames, a)

newAV :: MonadIO m => StorableArray Int SV -> PerlT s m AV
newAV arr = PerlT $ \perl (frame:frames) -> liftIO $ do
  withStorableArray arr $ \ptrAs -> do
    (lower, upper) <- getBounds arr
    a <- liftIO $ perl_av_make perl (fromIntegral $ upper - lower + 1) ptrAs
    return ((castPtr a:frame):frames, a)

clearAV :: MonadIO m => AV -> PerlT s m ()
clearAV av = PerlT $ \perl frames -> liftIO $ do
  perl_av_clear perl av
  return (frames, ())

peekOrFetchAV :: MonadIO m => CUInt -> AV -> CInt -> PerlT s m (Maybe SV)
peekOrFetchAV flag av i = PerlT $ \perl frames -> liftIO $ do
  ptrSv <- perl_av_fetch perl av i flag
  if ptrSv == nullPtr
    then return (frames, Nothing)
    else do
      sv <- peek ptrSv
      return (frames, Just sv)

peekAV :: MonadIO m => AV -> CInt -> PerlT s m (Maybe SV)
peekAV = peekOrFetchAV 0

fetchAV :: MonadIO m => AV -> CInt -> PerlT s m (Maybe SV)
fetchAV = peekOrFetchAV 1

storeAV :: MonadIO m => AV -> CInt -> SV -> PerlT s m ()
storeAV av i v = PerlT $ \perl frames -> liftIO $ do
  ptrSv <- perl_av_store perl av i v
  when (ptrSv /= nullPtr) $ svREFCNT_inc_void_NN v
  return (frames, ())

existsAV :: MonadIO m => AV -> CInt -> PerlT s m Bool
existsAV av i = PerlT $ \perl frames -> liftIO $ do
  res <- perl_av_exists perl av i
  return (frames, res)

lengthAV :: MonadIO m => AV -> PerlT s m CInt
lengthAV av = PerlT $ \perl frames -> liftIO $ do
  res <- perl_av_len perl av
  let len = res + 1
  len `seq` return (frames, len)

pushAV :: MonadIO m => AV -> SV -> PerlT s m ()
pushAV av sv = PerlT $ \perl frames -> liftIO $ do
  svREFCNT_inc_void_NN sv
  perl_av_push perl av sv
  return (frames, ())

unshiftAV :: MonadIO m => AV -> SV -> PerlT s m ()
unshiftAV av sv = PerlT $ \perl frames -> liftIO $ do
  svREFCNT_inc_void_NN sv
  perl_av_unshift perl av sv
  return (frames, ())

popAV :: MonadIO m => AV -> PerlT s m SV
popAV av = PerlT $ \perl frames -> liftIO $ do
  res <- perl_av_pop perl av
  return (frames, res)

shiftAV :: MonadIO m => AV -> PerlT s m SV
shiftAV av = PerlT $ \perl frames -> liftIO $ do
  res <- perl_av_shift perl av
  return (frames, res)

------
-- HV

newHVEmpty :: MonadIO m => PerlT s m HV
newHVEmpty = PerlT $ \perl (frame:frames) -> liftIO $ do
  hv <- perl_newHV perl
  return ((castPtr hv:frame):frames, hv)

clearHV :: MonadIO m => HV -> PerlT s m ()
clearHV hv = PerlT $ \perl frames -> liftIO $ do
  perl_hv_clear perl hv
  return (frames, ())

peekOrFetchHV :: MonadIO m => (PtrPerl -> HV -> CString -> StrLen -> IO (Ptr SV)) -> HV -> CStringLen -> PerlT s m (Maybe SV)
peekOrFetchHV act hv (key, klen) = PerlT $ \perl frames -> liftIO $ do
  ptrSv <- act perl hv key (fromIntegral klen)
  if ptrSv == nullPtr
    then return (frames, Nothing)
    else do
      sv <- peek ptrSv
      return (frames, Just sv)

peekHV :: MonadIO m => HV -> CStringLen -> PerlT s m (Maybe SV)
peekHV = peekOrFetchHV perl_hv_peek

fetchHV :: MonadIO m => HV -> CStringLen -> PerlT s m (Maybe SV)
fetchHV = peekOrFetchHV perl_hv_fetch

existsHV :: MonadIO m => HV -> CStringLen -> PerlT s m Bool
existsHV hv (key, klen) = PerlT $ \perl frames -> liftIO $ do
  res <- perl_hv_exists perl hv key (fromIntegral klen)
  return (frames, res)

storeHV :: MonadIO m => HV -> CStringLen -> SV -> PerlT s m ()
storeHV hv (key, klen) val = PerlT $ \perl frames -> liftIO $ do
  ptrSv <- perl_hv_store perl hv key (fromIntegral klen) val
  when (ptrSv /= nullPtr) $ svREFCNT_inc_void_NN val
  return (frames, ())

deleteHV :: MonadIO m => HV -> CStringLen -> PerlT s m (Maybe SV)
deleteHV hv (key, klen) = PerlT $ \perl frames -> liftIO $ do
  sv <- perl_hv_delete perl hv key (fromIntegral klen)
  if sv == nullPtr
    then return (frames, Nothing)
    else return (frames, Just sv)

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

callName :: MonadIO m => CString -> CInt -> StorableArray Int SV -> PerlT s m (StorableArray Int SV)
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
