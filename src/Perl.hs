{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Perl
  where

import Foreign.C.Types
import Foreign.C.String
--import Foreign.Marshal.Alloc
import Foreign.Ptr

type Strlen = Int

foreign import ccall unsafe "init_perl"
  perlInit :: IO ()

foreign import ccall unsafe "exit_perl"
  perlExit :: IO ()

--foreign import ccall unsafe "perl

--perlEval :: String -> IO ()

data SV
newtype Scalar = Scalar (Ptr SV)

foreign import ccall unsafe "glue_newSV"
  glue_newSV :: Strlen -> IO (Ptr SV)

newEmptyScalar :: IO Scalar
newEmptyScalar = fmap Scalar (glue_newSV 0)

foreign import ccall unsafe "glue_sv_setpvn"
  glue_sv_setpvn :: Ptr SV -> Ptr CChar -> Strlen -> IO ()

setScalarString :: Scalar -> CStringLen -> IO ()
setScalarString (Scalar sv) (p_str, len) = glue_sv_setpvn sv p_str len

foreign import ccall unsafe
  glue_eval_pv :: CString -> CInt -> IO (Ptr SV)

eval :: CString -> IO Scalar
eval code = fmap Scalar (glue_eval_pv code 1)
