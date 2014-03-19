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

const_G_METHOD :: Integral n => n
const_G_METHOD = 128

const_SVt_NULL, const_SVt_IV, const_SVt_NV, const_SVt_PV, const_SVt_INVLIST,
  const_SVt_PVIV, const_SVt_PVNV, const_SVt_PVMG, const_SVt_REGEXP, const_SVt_PVGV,
  const_SVt_PVLV, const_SVt_PVAV, const_SVt_PVHV, const_SVt_PVCV, const_SVt_PVFM,
  const_SVt_PVIO, const_SVt_LAST :: Integral n => n

const_SVt_NULL = 0
const_SVt_IV = 1
const_SVt_NV = 2
const_SVt_PV = 3
const_SVt_INVLIST = 4
const_SVt_PVIV = 5
const_SVt_PVNV = 6
const_SVt_PVMG = 7
const_SVt_REGEXP = 8
const_SVt_PVGV = 9
const_SVt_PVLV = 10
const_SVt_PVAV = 11
const_SVt_PVHV = 12
const_SVt_PVCV = 13
const_SVt_PVFM = 14
const_SVt_PVIO = 15
const_SVt_LAST = 16
