{-# LANGUAGE ForeignFunctionInterface #-}
module Perl
  where

import Foreign.C.String
import Foreign.Marshal.Alloc

foreign import ccall unsafe "init_perl"
  perlInit :: IO ()

foreign import ccall unsafe "exit_perl"
  perlExit :: IO ()
