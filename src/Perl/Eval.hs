{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
module Perl.Eval
  ( PerlEvalable (..)
  , voidEval
  , Retrievable (..)
  ) where

import Perl.Call
