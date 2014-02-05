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
import Perl.Glue
import Perl.Monad

------
-- ref count

incRefCnt :: MonadIO m => PtrSV -> PerlT s m ()
incRefCnt = liftIO . svREFCNT_inc_void_NN

decRefCnt :: MonadIO m => PtrSV -> PerlT s m ()
decRefCnt sv = PerlT $ \perl frames -> do
  liftIO (perl_sv_free perl sv)
  return (frames, ())

------
-- new SV

newSV :: MonadIO m => StrLen -> PerlT s m PtrSV
newSV len = PerlT $ \perl frames -> do
  sv <- liftIO $ perl_newSV perl len
  return (frames, sv)

newIntSV :: MonadIO m => IV -> PerlT s m PtrSV
newIntSV iv = PerlT $ \perl frames -> do
  sv <- liftIO $ perl_newSViv perl iv
  return (frames, sv)

newNumSV :: MonadIO m => NV -> PerlT s m PtrSV
newNumSV nv = PerlT $ \perl frames -> do
  sv <- liftIO $ perl_newSVnv perl nv
  return (frames, sv)

newStrSV :: MonadIO m => Ptr CChar -> StrLen -> CUInt -> PerlT s m PtrSV
newStrSV str len flag = PerlT $ \perl frames -> do
  sv <- liftIO $ perl_newSVpvn_flags perl str len flag
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

setSVStr :: MonadIO m => PtrSV -> CStringLen -> PerlT s m ()
setSVStr sv (ptrStr, len) = PerlT $ \perl frames -> do
  liftIO $ perl_sv_setpvn_mg perl sv ptrStr (fromIntegral len)
  return (frames, ())

setSVSV :: MonadIO m => PtrSV -> PtrSV -> PerlT s m ()
setSVSV dst src = PerlT $ \perl frames -> do
  liftIO $ perl_sv_setsv_mg perl dst src
  return (frames, ())

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

getSubArgs :: (Ix i, Num i, MonadIO m) => PerlSubT s m (StorableArray i PtrSV)
getSubArgs = PerlSubT $ \perl cv -> liftIO $ do
  items <- get_sub_arg_num perl
  args <- newArray_ (1, fromIntegral items)
  withStorableArray args $ \ptrArgs ->
    get_sub_args perl ptrArgs items
  return args

setSubReturns :: (Ix i, Integral i, MonadIO m) => StorableArray i PtrSV -> PerlSubT s m ()
setSubReturns returns = PerlSubT $ \perl cv -> liftIO $ do
  (a, b) <- getBounds returns
  withStorableArray returns $ \ptrReturns -> do
    set_sub_returns perl ptrReturns (fromIntegral (b - a - 1))
