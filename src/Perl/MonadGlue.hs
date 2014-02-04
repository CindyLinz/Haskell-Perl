module Perl.MonadGlue
  where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc

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

wrapSub :: MonadIO m => (PtrPerl -> PtrCV -> IO ()) -> PerlT s m PtrCV
wrapSub fun = PerlT $ \perl frames -> do
  funPtr <- liftIO $ wrap_sub_wrapper fun
  cv <- liftIO $ wrap_sub perl funPtr
  return (frames, cv)
