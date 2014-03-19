{-# LANGUAGE RankNTypes #-}
module Perl.Internal.MonadGlue
  where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Array.MArray
import Data.Array.Storable hiding (unsafeForeignPtrToStorableArray)
import Data.Array.Unsafe
import Data.Ix

import Perl.Type
import Perl.Class
import Perl.Constant
import Perl.Internal.Glue
import Perl.Monad

------
-- ref count

refCnt :: (MonadCatch m, MonadIO m) => SV -> PerlT s m CUInt
refCnt = liftIO . svREFCNT

incRefCnt :: (MonadCatch m, MonadIO m) => SV -> PerlT s m ()
incRefCnt = liftIO . svREFCNT_inc_void_NN

decRefCnt :: (MonadCatch m, MonadIO m) => SV -> PerlT s m ()
decRefCnt sv = PerlT $ \perl _ -> do
  liftIO (svREFCNT_dec perl sv)
  return $ pure ()

------
-- new SV

newSV :: (MonadCatch m, MonadIO m) => PerlT s m SV
newSV = PerlT $ \perl _ -> do
  sv <- liftIO $ perl_newSV perl 0
  return ([sv], sv)

newSVSV :: (MonadCatch m, MonadIO m) => SV -> PerlT s m SV
newSVSV sv = PerlT $ \perl _ -> do
  sv' <- liftIO $ perl_newSVsv perl sv
  return ([sv'], sv')

newIntSV :: (MonadCatch m, MonadIO m) => IV -> PerlT s m SV
newIntSV iv = PerlT $ \perl _ -> do
  sv <- liftIO $ perl_newSViv perl iv
  return ([sv], sv)

newNumSV :: (MonadCatch m, MonadIO m) => NV -> PerlT s m SV
newNumSV nv = PerlT $ \perl _ -> do
  sv <- liftIO $ perl_newSVnv perl nv
  return ([sv], sv)

newStrSV :: (MonadCatch m, MonadIO m) => Ptr CChar -> StrLen -> PerlT s m SV
newStrSV str len = PerlT $ \perl _ -> do
  sv <- liftIO $ perl_newSVpvn_flags perl str len 0
  return ([sv], sv)

newSVMortal :: (MonadCatch m, MonadIO m) => PerlT s m SV
newSVMortal = PerlT $ \perl _ ->
  liftIO (perl_sv_newmortal perl) >>= return . pure

newIntSVMortal :: (MonadCatch m, MonadIO m) => IV -> PerlT s m SV
newIntSVMortal iv = PerlT $ \perl _ ->
  liftIO (newSViv_mortal perl iv) >>= return . pure

newNumSVMortal :: (MonadCatch m, MonadIO m) => NV -> PerlT s m SV
newNumSVMortal nv = PerlT $ \perl _ ->
  liftIO (newSVnv_mortal perl nv) >>= return . pure

newStrSVMortal :: (MonadCatch m, MonadIO m) => Ptr CChar -> StrLen -> PerlT s m SV
newStrSVMortal str len = PerlT $ \perl _ ->
  liftIO (perl_newSVpvn_flags perl str len const_SVs_TEMP) >>= return . pure

newSVSVMortal :: (MonadCatch m, MonadIO m) => SV -> PerlT s m SV
newSVSVMortal old = PerlT $ \perl _ ->
  liftIO (perl_sv_mortalcopy perl old) >>= return . pure

getSV :: (MonadCatch m, MonadIO m) => CStringLen -> CInt -> PerlT s m SV
getSV (name, namelen) flag = PerlT $ \perl _ ->
  liftIO (perl_get_svn_flags perl name (fromIntegral namelen) flag) >>= return . pure

------
-- read SV

svToInt :: (MonadCatch m, MonadIO m) => SV -> PerlT s m IV
svToInt sv = PerlT $ \perl _ ->
  liftIO (svIVx perl sv) >>= return . pure

svToNum :: (MonadCatch m, MonadIO m) => SV -> PerlT s m NV
svToNum sv = PerlT $ \perl _ ->
  liftIO (svNVx perl sv) >>= return . pure

-- the returned CStringLen is just temporary that is handled by perl
svToStr :: (MonadCatch m, MonadIO m) => SV -> PerlT s m CStringLen
svToStr sv = PerlT $ \perl _ -> liftIO $ alloca $ \ptrLen -> do
  ptrStr <- svPVbytex perl sv ptrLen
  len <- peek ptrLen
  return $ pure (ptrStr, fromIntegral len)

------
-- write SV

setSVUndef :: (MonadCatch m, MonadIO m) => SV -> PerlT s m ()
setSVUndef sv = PerlT $ \perl _ ->
  liftIO (perl_sv_setundef perl sv) >>= return . pure

setSVInt :: (MonadCatch m, MonadIO m) => SV -> IV -> PerlT s m ()
setSVInt sv a = PerlT $ \perl _ ->
  liftIO (perl_sv_setiv_mg perl sv a) >>= return . pure

setSVNum :: (MonadCatch m, MonadIO m) => SV -> NV -> PerlT s m ()
setSVNum sv a = PerlT $ \perl _ ->
  liftIO (perl_sv_setnv_mg perl sv a) >>= return . pure

setSVStr :: (MonadCatch m, MonadIO m) => SV -> Ptr CChar -> StrLen -> PerlT s m ()
setSVStr sv ptrStr len = PerlT $ \perl _ ->
  liftIO (perl_sv_setpvn_mg perl sv ptrStr len) >>= return . pure

setSVSV :: (MonadCatch m, MonadIO m) => SV -> SV -> PerlT s m ()
setSVSV dst src = PerlT $ \perl _ ->
  liftIO (perl_sv_setsv_mg perl dst src) >>= return . pure

------
-- ref

newSVRef :: (MonadCatch m, MonadIO m) => SV -> PerlT s m RefSV
newSVRef sv = PerlT $ \perl _ -> do
  rv <- liftIO $ perl_newSVRV perl sv
  return ([castPtr rv], rv)

newAVRef :: (MonadCatch m, MonadIO m) => AV -> PerlT s m RefAV
newAVRef av = PerlT $ \perl _ -> do
  rv <- liftIO $ perl_newAVRV perl av
  return ([castPtr av], rv)

newHVRef :: (MonadCatch m, MonadIO m) => HV -> PerlT s m RefHV
newHVRef hv = PerlT $ \perl _ -> do
  rv <- liftIO $ perl_newHVRV perl hv
  return ([castPtr rv], rv)

deRefSV :: (MonadCatch m, MonadIO m) => RefSV -> PerlT s m SV
deRefSV = liftIO . svRV

deRefAV :: (MonadCatch m, MonadIO m) => RefAV -> PerlT s m AV
deRefAV = liftIO . avRV

deRefHV :: (MonadCatch m, MonadIO m) => RefHV -> PerlT s m HV
deRefHV = liftIO . hvRV

rvType :: (MonadCatch m, MonadIO m) => SV -> PerlT s m CInt
rvType = liftIO . rvTYPE

------
-- AV

newAVEmpty :: (MonadCatch m, MonadIO m) => PerlT s m AV
newAVEmpty = PerlT $ \perl _ -> do
  a <- liftIO $ perl_newAV perl
  return ([castPtr a], a)

newAV :: (MonadCatch m, MonadIO m) => SVArray -> PerlT s m AV
newAV arr = PerlT $ \perl _ -> liftIO $ do
  withStorableArray arr $ \ptrAs -> do
    (lower, upper) <- getBounds arr
    a <- liftIO $ perl_av_make perl (fromIntegral $ upper - lower + 1) ptrAs
    return ([castPtr a], a)

clearAV :: (MonadCatch m, MonadIO m) => AV -> PerlT s m ()
clearAV av = PerlT $ \perl _ ->
  liftIO (perl_av_clear perl av) >>= return . pure

peekOrFetchAV :: (MonadCatch m, MonadIO m) => CUInt -> AV -> CInt -> PerlT s m (Maybe SV)
peekOrFetchAV flag av i = PerlT $ \perl _ -> liftIO $ do
  ptrSv <- perl_av_fetch perl av i flag
  if ptrSv == nullPtr
    then return $ pure Nothing
    else do
      sv <- peek ptrSv
      return $ pure (Just sv)

peekAV :: (MonadCatch m, MonadIO m) => AV -> CInt -> PerlT s m (Maybe SV)
peekAV = peekOrFetchAV 0

fetchAV :: (MonadCatch m, MonadIO m) => AV -> CInt -> PerlT s m (Maybe SV)
fetchAV = peekOrFetchAV 1

storeAV :: (MonadCatch m, MonadIO m) => AV -> CInt -> SV -> PerlT s m ()
storeAV av i v = PerlT $ \perl _ -> liftIO $ do
  ptrSv <- perl_av_store perl av i v
  when (ptrSv /= nullPtr) $ svREFCNT_inc_void_NN v
  return $ pure ()

existsAV :: (MonadCatch m, MonadIO m) => AV -> CInt -> PerlT s m Bool
existsAV av i = PerlT $ \perl _ ->
  liftIO (perl_av_exists perl av i) >>= return . pure

lengthAV :: (MonadCatch m, MonadIO m) => AV -> PerlT s m CInt
lengthAV av = PerlT $ \perl _ -> liftIO $ do
  res <- perl_av_len perl av
  return $! pure $! res + 1

pushAV :: (MonadCatch m, MonadIO m) => AV -> SV -> PerlT s m ()
pushAV av sv = PerlT $ \perl _ -> liftIO $ do
  svREFCNT_inc_void_NN sv
  perl_av_push perl av sv
  return $ pure ()

unshiftAV :: (MonadCatch m, MonadIO m) => AV -> SV -> PerlT s m ()
unshiftAV av sv = PerlT $ \perl _ -> liftIO $ do
  svREFCNT_inc_void_NN sv
  perl_av_unshift perl av sv
  return $ pure ()

popAV :: (MonadCatch m, MonadIO m) => AV -> PerlT s m SV
popAV av = PerlT $ \perl _ ->
  liftIO (perl_av_pop perl av) >>= return . pure

shiftAV :: (MonadCatch m, MonadIO m) => AV -> PerlT s m SV
shiftAV av = PerlT $ \perl _ ->
  liftIO (perl_av_shift perl av) >>= return . pure

getAV :: (MonadCatch m, MonadIO m) => CStringLen -> CInt -> PerlT s m AV
getAV (name, namelen) flag = PerlT $ \perl _ ->
  liftIO (perl_get_avn_flags perl name (fromIntegral namelen) flag) >>= return . pure

------
-- HV

newHVEmpty :: (MonadCatch m, MonadIO m) => PerlT s m HV
newHVEmpty = PerlT $ \perl _ -> liftIO $ do
  hv <- perl_newHV perl
  return ([castPtr hv], hv)

clearHV :: (MonadCatch m, MonadIO m) => HV -> PerlT s m ()
clearHV hv = PerlT $ \perl _ ->
  liftIO (perl_hv_clear perl hv) >>= return . pure

peekOrFetchHV :: (MonadCatch m, MonadIO m) => (PtrPerl -> HV -> CString -> StrLen -> IO (Ptr SV)) -> HV -> CStringLen -> PerlT s m (Maybe SV)
peekOrFetchHV act hv (key, klen) = PerlT $ \perl _ -> liftIO $ do
  ptrSv <- act perl hv key (fromIntegral klen)
  if ptrSv == nullPtr
    then return $ pure Nothing
    else do
      sv <- peek ptrSv
      return $ pure (Just sv)

peekHV :: (MonadCatch m, MonadIO m) => HV -> CStringLen -> PerlT s m (Maybe SV)
peekHV = peekOrFetchHV perl_hv_peek

fetchHV :: (MonadCatch m, MonadIO m) => HV -> CStringLen -> PerlT s m (Maybe SV)
fetchHV = peekOrFetchHV perl_hv_fetch

existsHV :: (MonadCatch m, MonadIO m) => HV -> CStringLen -> PerlT s m Bool
existsHV hv (key, klen) = PerlT $ \perl _ ->
  liftIO (perl_hv_exists perl hv key (fromIntegral klen)) >>= return . pure

storeHV :: (MonadCatch m, MonadIO m) => HV -> CStringLen -> SV -> PerlT s m ()
storeHV hv (key, klen) val = PerlT $ \perl _ -> liftIO $ do
  ptrSv <- perl_hv_store perl hv key (fromIntegral klen) val
  when (ptrSv /= nullPtr) $ svREFCNT_inc_void_NN val
  return $ pure ()

deleteHV :: (MonadCatch m, MonadIO m) => HV -> CStringLen -> PerlT s m (Maybe SV)
deleteHV hv (key, klen) = PerlT $ \perl _ -> liftIO $ do
  sv <- perl_hv_delete perl hv key (fromIntegral klen)
  return $ pure $ if sv == nullPtr
    then Nothing
    else Just sv

getHV :: (MonadCatch m, MonadIO m) => CStringLen -> CInt -> PerlT s m HV
getHV (name, namelen) flag = PerlT $ \perl _ ->
  liftIO (perl_get_hvn_flags perl name (fromIntegral namelen) flag) >>= return . pure

------
-- eval

eval :: (MonadCatch m, MonadIO m) => CStringLen -> CInt -> PerlT s m SVArray
eval (code, codeLen) flags = PerlT $ \perl _ -> liftIO $ alloca $ \ptrPtrOut -> do
  outn <- glue_eval_pv perl code (fromIntegral codeLen) flags ptrPtrOut
  if outn == 0
    then
      newArray_ (1, 0) >>= return . pure
    else do
      ptrOut <- peek ptrPtrOut
      fptrOut <- newForeignPtr p_free ptrOut
      outArray <- unsafeForeignPtrToStorableArray fptrOut (1, fromIntegral outn)
      return $ pure outArray

getEvalError :: (MonadCatch m, MonadIO m) => PerlT s m (Maybe SV)
getEvalError = PerlT $ \perl _ -> do
  sv <- liftIO $ glue_get_error perl
  return $ pure $ if sv == nullPtr
    then Nothing
    else Just sv

------
-- call

callVar :: (MonadCatch m, MonadIO m) => SV -> CInt -> PerlT s m CInt
callVar sv flag = PerlT $ \perl _ ->
  liftIO (perl_call_sv perl sv flag) >>= return . pure

callName :: (MonadCatch m, MonadIO m) => CStringLen -> CInt -> SVArray -> PerlT s m SVArray
callName (name, nameLen) flag args = PerlT $ \perl _ -> liftIO . withStorableArray args $ \ptrArg -> alloca $ \ptrPtrOut -> do
  argc <- liftM (fromIntegral . rangeSize) (getBounds args)
  outn <- glue_call_pv perl name (fromIntegral nameLen) flag argc ptrArg ptrPtrOut
  if outn == 0
    then
      newArray_ (1, 0) >>= return . pure
    else do
      ptrOut <- peek ptrPtrOut
      fptrOut <- newForeignPtr p_free ptrOut
      outArray <- unsafeForeignPtrToStorableArray fptrOut (1, fromIntegral outn)
      return $ pure outArray

------
-- sub

finalSub :: ToSVArray ret => (forall s1. Perl s1 ret) -> PtrPerl -> CV -> IO SV
finalSub body perl cv = catchJust fromException
  ( do
    (svPool, ()) <- unPerlT (body >>= toSVMortalArray >>= setSubReturns) perl cv
    releaseSVPool perl svPool
    return nullPtr
  )
  (\(PerlException _ sv) -> return sv)

wrapSub :: (MonadCatch m, MonadIO m) => (PtrPerl -> CV -> IO SV) -> PerlT s m RefCV
wrapSub fun = PerlT $ \perl _ -> liftIO $ do
  funPtr <- wrap_sub_wrapper fun
  cv <- wrap_sub perl funPtr
  return ([castPtr cv], cv)

makeSub :: (ToSVArray ret, MonadCatch m, MonadIO m) => (forall s1. Perl s1 ret) -> PerlT s m RefCV
makeSub = wrapSub . finalSub

regSub :: (MonadCatch m, MonadIO m) => CString -> (PtrPerl -> CV -> IO SV) -> PerlT s m ()
regSub name fun = PerlT $ \perl _ ->
  liftIO $ wrap_sub_wrapper fun >>= reg_sub perl name >>= return . pure

defineSub :: (ToSVArray ret, MonadCatch m, MonadIO m) => CString -> (forall s1. Perl s1 ret) -> PerlT s m ()
defineSub name = regSub name . finalSub

getSubArgs :: (MonadCatch m, MonadIO m) => PerlT s m SVArray
getSubArgs = PerlT $ \perl cv -> liftIO $ do
  items <- get_sub_arg_num perl
  args <- newArray_ (1, fromIntegral items)
  withStorableArray args $ \ptrArgs ->
    get_sub_args perl ptrArgs items
  return $ pure args

-- must be the last step in the sub that modify the perl stack
setSubReturns :: (MonadCatch m, MonadIO m) => SVArray -> PerlT s m ()
setSubReturns returns = PerlT $ \perl cv -> liftIO $ do
  (a, b) <- getBounds returns
  withStorableArray returns $ \ptrReturns -> do
    set_sub_returns perl ptrReturns (fromIntegral (b - a + 1))
  return $ pure ()

getSubContext :: (MonadCatch m, MonadIO m) => PerlT s m CInt
getSubContext = PerlT $ \perl cv ->
  liftIO (get_sub_context perl) >>= return . pure

getCV :: (MonadCatch m, MonadIO m) => CStringLen -> CInt -> PerlT s m CV
getCV (name, namelen) flag = PerlT $ \perl _ ->
  liftIO (perl_get_cvn_flags perl name (fromIntegral namelen) flag) >>= return . pure

------
-- embed

-- | find \'my\' or \'our\' scalars
findSV :: (MonadCatch m, MonadIO m) => CStringLen -> PerlT s m SV
findSV (name, namelen) = PerlT $ \perl _ ->
  liftIO (perl_pad_peek_pvn perl name (fromIntegral namelen)) >>= return . pure

-- | find \'my\' or \'our\' arrays
findAV :: (MonadCatch m, MonadIO m) => CStringLen -> PerlT s m AV
findAV name = findSV name >>= return . castPtr

-- | find \'my\' or \'our\' hashs
findHV :: (MonadCatch m, MonadIO m) => CStringLen -> PerlT s m HV
findHV name = findSV name >>= return . castPtr
