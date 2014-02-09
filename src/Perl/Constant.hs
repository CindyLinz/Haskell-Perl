module Perl.Constant
  where

const_SVs_TEMP :: Integral n => n
const_SVs_TEMP = 0x00080000

const_G_VOID :: Integral n => n
const_G_VOID = 1
const_G_SCALAR :: Integral n => n
const_G_SCALAR = 2
const_G_ARRAY :: Integral n => n
const_G_ARRAY = 3
const_G_DISCARD :: Integral n => n
const_G_DISCARD = 4

const_G_EVAL :: Integral n => n
const_G_EVAL = 8
