#ifndef PERL_BUILDIN_SUB_WRAPPER
#define PERL_BUILDIN_SUB_WRAPPER \
    "# stolen and modified from\n" \
    "#  http://stackoverflow.com/questions/1585560/can-you-take-a-reference-of-a-builtin-function-in-perl\n" \
    "# by Eric Strom http://stackoverflow.com/users/189416/eric-strom\n" \
    "package HasPerl;" \
    "sub make_builtin {" \
        "my ($sub, $my, $id) = ($_[0], '');" \
        "my $name = $sub;" \
        "my $proto = prototype $sub //" \
        "    prototype qq(CORE::$sub) //" \
        "    $_[1]                  //" \
        "    ($sub =~ /map|grep/ ? '&@' : '@;_');" \
        "for ($proto =~ /(\\\\?.)/g) { $id++;" \
        "    if (/(?|(\\$|&)|.(.))/) {" \
        "        $my  .= qq(my \\$_$id = shift;);" \
        "        $sub .= qq( $1\\$_$id,);" \
        "    } elsif (/([@%])/) {" \
        "        $my  .= qq(my $1_$id = splice \\@_, 0, \\@_;);" \
        "        $sub .= qq( $1_$id,);" \
        "    } elsif (/_/) {" \
        "        $my  .= qq(my \\$_$id = \\@_ ? shift : \\$_;);" \
        "        $sub .= qq( \\$_$id,)" \
        "    }" \
        "}" \
        "eval qq(*HasPerl::builtin::$name = sub ($proto) {$my $sub})" \
        "    or die qq(prototype ($proto) failed for '$_[0]', )." \
        "    qq(try passing a prototype string as \\$_[1])" \
    "}"
#endif
