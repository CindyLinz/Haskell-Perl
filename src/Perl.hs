{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Perl
  where

import Foreign.C.Types
import Foreign.C.String
--import Foreign.Marshal.Alloc
import Foreign.Ptr

type Strlen = Int

data PerlInterpreter

foreign import ccall unsafe "init_perl"
  perlInit :: IO (Ptr PerlInterpreter)

foreign import ccall unsafe "exit_perl"
  perlExit :: Ptr PerlInterpreter -> IO ()

--foreign import ccall unsafe "perl

--perlEval :: String -> IO ()

data SV
newtype Scalar = Scalar (Ptr SV)

foreign import ccall unsafe
  glue_eval_pv :: Ptr PerlInterpreter -> CString -> CInt -> IO (Ptr SV)

eval :: Ptr PerlInterpreter -> CString -> IO Scalar
eval my_perl code = fmap Scalar (glue_eval_pv my_perl code 1)
