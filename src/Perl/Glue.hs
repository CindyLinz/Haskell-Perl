{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Perl.Glue
  where

import Foreign

data PerlInterpreter

foreign import ccall unsafe
  init_perl :: IO (Ptr PerlInterpreter)

foreign import ccall unsafe
  exit_perl :: Ptr PerlInterpreter -> IO ()
