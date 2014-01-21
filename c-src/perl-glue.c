#define PERL_NO_SHORT_NAMES
#include <EXTERN.h>
#include <perl.h>

static PerlInterpreter *my_perl;

void init_perl(){
    int my_argc = 3;
    char *my_argv[] = { "", "-e", "0", NULL };
    char *my_env[] = { NULL };
    PERL_SYS_INIT3(&my_argc, (char ***)&my_argv, (char ***)&my_env);
    my_perl = perl_alloc();
    perl_construct(my_perl);
    perl_parse(my_perl, NULL, 3, my_argv, NULL);
}

void exit_perl(){
    perl_destruct(my_perl);
    perl_free(my_perl);
    PERL_SYS_TERM();
}
