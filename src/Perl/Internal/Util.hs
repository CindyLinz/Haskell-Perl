module Perl.Internal.Util
  where

import Foreign.Ptr

liftNull :: (Monad m) => Ptr a -> m (Ptr a)
liftNull p =
  if p == nullPtr
    then fail "NULL ptr"
    else return p
