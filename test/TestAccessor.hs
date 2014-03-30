module Main where

import Control.Monad.IO.Class

import Perl.Type
import Perl.Class
import Perl.Monad
import Perl.Accessor
import Perl.Sub

main = runPerlT $ do
  eval "print qq(Hi$/);"
  eval "1" >>= readScalar >>= liftIO . putStrLn
  eval "[1,2]" @- 1 >>= readScalar >>= liftIO . putStrLn
  eval "{a=>2,b=>3}" %- "b" >>= readScalar >>= liftIO . putStrLn
  eval "my @a = (2, {y => 'o', x => [5,3,4]}); \\@a" @- 1 %- "x" @- 2 >>= readScalar >>= liftIO . putStrLn

  defSub "access" $ do
    cap "@a" @- 1 %- "x" @- 0 >>= readScalar >>= liftIO . putStrLn
    cap "%a" %- "b" @- 0 %- "x" >>= readScalar >>= liftIO . putStrLn
    cap "$a" %- "b" @- 0 %- "y" >>= readScalar >>= liftIO . putStrLn

    cap "@a" @- 1 %- "x" @- 3 &- "8"

    cap "@a" @- 0 @= [3,4,5 :: Int]
    cap "@a" @- 0 @- 1 $= (25 :: Int)
    cap "@a" @- 0 @- 2 %= [("a", 7), ("b", 6 :: Int)]

    cap "%a" %- "obj" .&&- "data" -$ (1 :: Int) >>= readScalar >>= liftIO . putStrLn

    cap "%a" %+= [("c", "good")]
    obj <- cap "O" .&&- "new" -$ (10 :: Int, "x") >>= readScalar
    cap "%a" %- "d" $= (obj :: SV)

    cap "%a" %- "d" .&&- "data" -$ "0" >>= readScalar >>= liftIO . putStrLn

    retSub ()

  eval
    $ "{"
    ++ "  package O;"
    ++ "  sub new {"
    ++ "    bless [splice @_, 1], $_[0];"
    ++ "  }"
    ++ "  sub data {"
    ++ "    return $_[0][$_[1]];"
    ++ "  }"
    ++ "}"
    ++ "my @a = (2, {y => 'o', x => [5,3,4, sub { print qq($_[0]$/) }]});"
    ++ "my %a = (a => 5, b => [{x => 6}], obj => O->new('aa', 9));"
    ++ "my $a = {a => 5, b => [{x => 6, y=>7}]};"
    ++ "access();"
    ++ "use Data::Dumper;"
    ++ "local $Data::Dumper::Indent = 0;"
    ++ "print '@a=', Dumper(\\@a), $/;"
    ++ "print '%a=', Dumper(\\%a), $/;"
    ++ "print '$a=', Dumper(\\$a), $/;"
