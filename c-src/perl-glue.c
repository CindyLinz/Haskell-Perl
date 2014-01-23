//#define PERL_NO_SHORT_NAMES
#include <EXTERN.h>
#include <perl.h>

#ifdef TRACK_PERL_GLUE
#include <stdio.h>
#endif

EXTERN_C void xs_init (pTHX);
static PerlInterpreter *my_perl;

void init_perl(){
    int my_argc = 3;
    char *my_argv[] = { "", "-e", "0", NULL };
    char *my_env[] = { NULL };
#ifdef TRACK_PERL_GLUE
    puts("init_perl()");
#endif
    PERL_SYS_INIT3(&my_argc, (char ***)&my_argv, (char ***)&my_env);
    my_perl = perl_alloc();
    perl_construct(my_perl);
    perl_parse(my_perl, xs_init, 3, my_argv, NULL);
}

void exit_perl(){
#ifdef TRACK_PERL_GLUE
    puts("exit_perl()");
#endif
    perl_destruct(my_perl);
    perl_free(my_perl);
    PERL_SYS_TERM();
}

SV* glue_newSV(const STRLEN len){
#ifdef TRACK_PERL_GLUE
    puts("glue_newSV()");
#endif
    return newSV(len);
}

void glue_sv_setpvn(SV *sv, const char *str, const STRLEN len){
#ifdef TRACK_PERL_GLUE
    puts("glue_sv_setpvn()");
#endif
    sv_setpvn(sv, str, len);
}

SV *glue_eval_pv(const char *p, I32 croak_on_error){
#ifdef TRACK_PERL_GLUE
    puts("glue_eval_pv()");
#endif
    return eval_pv(p, croak_on_error);
}
