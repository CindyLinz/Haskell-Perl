{-# LANGUAGE TypeSynonymInstances, ExistentialQuantification, Rank2Types, FlexibleInstances #-}
module Perl.Accessor
  where

import Data.Array.IArray

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Foreign.Ptr
import Foreign.C.Types

import Perl.Type
import Perl.Constant
import Perl.Monad
import Perl.Class
import Perl.SV
import Perl.AV
import Perl.HV
import Perl.Ref hiding (deRef)
import qualified Perl.Ref as Ref
import Perl.Sub
import Perl.Call hiding (eval, call)
import qualified Perl.Call as Call

-- my $data = { a => 3
-- , b => [4, 5, 6]
-- , c => { x => 1, y => 'abc', z => [3, 2, 1] }
-- , d => [ sub { return $_[0] + $_[1] + 3 } ]
-- , e => sub { return [3,$_[0]+1,5] }
-- };

-- capO <- cap "@o"
-- [capA, capB] <- caps "$a,$b"
-- capI <- cap "$_"
-- [cap1, cap2, cap4] <- caps "@_[1,2,4]"
-- [capX] <- caps "$data->{b}[1]"

data PerlVar 
  = PerlVarSV SV
  | PerlVarSVArray SVArray
  | PerlVarArrayElement AV Int
  | PerlVarHashElement HV String

infixl 2 @-, %-, &-, .&-, .&&-, -$, &, $=, @=, %=, %+=

(&) :: a -> (a -> b) -> b
a & f = f a

(-$) :: (a -> b) -> a -> b
f -$ a = f a

cap :: (MonadCatch m, MonadIO m) => String -> PerlT s m PerlVar
cap code =
  Call.eval ('\\' : code) >>= Ref.deRef >>= return . PerlVarSV

caps :: (MonadCatch m, MonadIO m) => String -> PerlT s m [PerlVar]
caps code =
  Call.eval ('\\' : code) >>= mapM (\ref -> Ref.deRef (ref :: RefSV) >>= return . PerlVarSV)

reifyScalar :: (MonadCatch m, MonadIO m) => PerlVar -> PerlT s m SV
reifyScalar v = case v of
  PerlVarSV sv -> return sv
  PerlVarSVArray svs -> fromSVArray svs
  PerlVarArrayElement av i -> do
    maybeSV <- fetchAV av (fromIntegral i)
    case maybeSV of
      Just sv -> return sv
      Nothing -> die "can't get real SV"
  PerlVarHashElement hv key -> do
    maybeSV <- fetchHV hv key
    case maybeSV of
      Just sv -> return sv
      Nothing -> die "can't get real SV"

writableScalar :: (MonadCatch m, MonadIO m) => PerlVar -> PerlT s m SV
writableScalar v = reifyScalar v

reifyScalarArray :: (MonadCatch m, MonadIO m) => PerlVar -> PerlT s m SVArray
reifyScalarArray v =
  let
    tryExtractAV svs = if rangeSize (bounds svs) == 1
      then do
        let sv = svs ! 1
        ty <- svType sv
        case () of
          _ | ty == const_SVt_PVAV -> toSVArray (castPtr sv :: AV)
          _ | ty == const_SVt_PVHV -> toSVArray (castPtr sv :: HV)
          _ -> return svs
      else return svs
  in case v of
    PerlVarSVArray svs -> tryExtractAV svs
    _ -> reifyScalar v >>= tryExtractAV . listArray (1,1) . pure

accessArray :: (MonadCatch m, MonadIO m) => PerlVar -> Int -> PerlT s m PerlVar -- array(or arrayref) access
accessArray v i = do
  sv <- reifyScalar v
  ty <- svType sv
  av <- case () of
    _ | ty < const_SVt_PVAV -> asRef sv >>= Ref.deRef
    _ | ty == const_SVt_PVAV -> return $ castPtr sv
    _ -> die "not array or array reference"
  return $ PerlVarArrayElement av i

accessHash :: (MonadCatch m, MonadIO m) => PerlVar -> String -> PerlT s m PerlVar -- hash(or hashref) access
accessHash v k = do
  sv <- reifyScalar v
  ty <- svType sv
  hv <- case () of
    _ | ty < const_SVt_PVAV -> asRef sv >>= Ref.deRef
    _ | ty == const_SVt_PVHV -> return $ castPtr sv
    _ -> die "not hash or hash reference"
  return $ PerlVarHashElement hv k

accessMethod :: (MonadCatch m, MonadIO m) => PerlVar -> String -> PerlT s m PerlVar -- method call (no args)
accessMethod v method = do
  obj <- reifyScalar v
  ret <- callMethod obj method ()
  return $ PerlVarSVArray ret

accessMethodArgs :: (ToSVList args, MonadCatch m, MonadIO m) => PerlVar -> String -> args -> PerlT s m PerlVar -- method call (with args)
accessMethodArgs v method args = do
  obj <- reifyScalar v
  callMethod obj method args >>= return . PerlVarSVArray

(@-) :: (MonadCatch m, MonadIO m) => PerlT s m PerlVar -> Int -> PerlT s m PerlVar -- array(or arrayref) access
capVar @- i = capVar >>= flip accessArray i

(%-) :: (MonadCatch m, MonadIO m) => PerlT s m PerlVar -> String -> PerlT s m PerlVar -- hash(or hashref) access
capVar %- key = capVar >>= flip accessHash key

(.&-) :: (MonadCatch m, MonadIO m) => PerlT s m PerlVar -> String -> PerlT s m PerlVar -- method call (no args)
capVar .&- method = capVar >>= flip accessMethod method

(.&&-) :: (ToSVList args, MonadCatch m, MonadIO m) => PerlT s m PerlVar -> String -> args -> PerlT s m PerlVar -- method call (with args)
(capVar .&&- method) args = capVar >>= \obj -> accessMethodArgs obj method args

deRef :: (MonadCatch m, MonadIO m) => PerlVar -> PerlT s m PerlVar -- dereference
deRef var = reifyScalar var >>= asRef >>= Ref.deRef >>= return . PerlVarSV

eval :: (MonadCatch m, MonadIO m) => String -> PerlT s m PerlVar
eval code = Call.eval code >>= return . PerlVarSVArray

call :: (MonadCatch m, MonadIO m) => String -> PerlT s m PerlVar -- call function (no args)
call func = Call.call func () >>= return . PerlVarSVArray

callArgs :: (ToSVList args, MonadCatch m, MonadIO m) => String -> args -> PerlT s m PerlVar -- call function (with args)
callArgs func args = Call.call func args >>= return . PerlVarSVArray

callSub :: (MonadCatch m, MonadIO m) => PerlVar -> PerlT s m PerlVar -- call sub (no args)
callSub sub = reifyScalar sub >>= asRef >>= flip Call.callVar () >>= return . PerlVarSVArray

callSubArgs :: (ToSVList args, MonadCatch m, MonadIO m) => PerlVar -> args -> PerlT s m PerlVar
callSubArgs sub args = reifyScalar sub >>= asRef >>= flip Call.callVar args >>= return . PerlVarSVArray

(&-) :: (ToSVList args, MonadCatch m, MonadIO m) => PerlT s m PerlVar -> args -> PerlT s m PerlVar
capSub &- args = capSub >>= flip callSubArgs args

readScalar :: (FromSV a, MonadCatch m, MonadIO m) => PerlVar -> PerlT s m a
readScalar var = reifyScalar var >>= fromSV

readInt :: (MonadCatch m, MonadIO m) => PerlVar -> PerlT s m Int
readInt = readScalar

readDouble :: (MonadCatch m, MonadIO m) => PerlVar -> PerlT s m Double
readDouble = readScalar

readStr :: (MonadCatch m, MonadIO m) => PerlVar -> PerlT s m String
readStr = readScalar

writeScalar :: (ToSV a, MonadCatch m, MonadIO m)  => PerlVar -> a -> PerlT s m ()
writeScalar var a = reifyScalar var >>= flip setSV a

writeInt :: (MonadCatch m, MonadIO m)  => PerlVar -> Int -> PerlT s m ()
writeInt = writeScalar

writeDouble :: (MonadCatch m, MonadIO m)  => PerlVar -> Double -> PerlT s m ()
writeDouble = writeScalar

writeStr :: (MonadCatch m, MonadIO m)  => PerlVar -> String -> PerlT s m ()
writeStr = writeScalar

($=) :: (ToSV a, MonadCatch m, MonadIO m) => PerlT s m PerlVar -> a -> PerlT s m PerlVar
capVar $= a = capVar >>= reifyScalar >>= flip setSV a >> capVar

readArray :: (FromSVArray a, MonadCatch m, MonadIO m) => PerlVar -> PerlT s m a
readArray var = reifyScalarArray var >>= fromSVArray

writeArray :: (ToAV a, MonadCatch m, MonadIO m) => PerlVar -> a -> PerlT s m ()
writeArray var a = do
  sv <- reifyScalar var
  ty <- svType sv
  case () of
    _ | ty < const_SVt_PVAV -> toAV a >>= newRef >>= setSV sv
    _ | ty == const_SVt_PVAV -> setAV (castPtr sv) a
    _ -> die "not array or array reference"

(@=) :: (ToAV a, MonadCatch m, MonadIO m) => PerlT s m PerlVar -> a -> PerlT s m PerlVar
capAV @= a = capAV >>= flip writeArray a >> capAV

writeHash :: (ToHV a, MonadCatch m, MonadIO m) => PerlVar -> a -> PerlT s m ()
writeHash var a = do
  sv <- reifyScalar var
  ty <- svType sv
  case () of
    _ | ty < const_SVt_PVAV -> toHV a >>= newRef >>= setSV sv
    _ | ty == const_SVt_PVHV -> setHV (castPtr sv) a
    _ -> die "not hash or hash reference"

(%=) :: (ToHV a, MonadCatch m, MonadIO m) => PerlT s m PerlVar -> a -> PerlT s m PerlVar
capHV %= a = capHV >>= flip writeHash a >> capHV

appendHash :: (ToHV a, MonadCatch m, MonadIO m) => PerlVar -> a -> PerlT s m ()
appendHash var a = do
  sv <- reifyScalar var
  ty <- svType sv
  case () of
    _ | ty < const_SVt_PVAV -> asRef sv >>= Ref.deRef >>= flip appendHV a
    _ | ty == const_SVt_PVHV -> appendHV (castPtr sv) a
    _ -> die "not hash or hash reference"

(%+=) :: (ToHV a, MonadCatch m, MonadIO m) => PerlT s m PerlVar -> a -> PerlT s m PerlVar
capHV %+= a = capHV >>= flip appendHash a >> capHV
