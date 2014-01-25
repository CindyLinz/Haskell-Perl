//#define PERL_NO_SHORT_NAMES
#undef USE_ITHREADS

#include <EXTERN.h>
#include <perl.h>

#ifdef TRACK_PERL_GLUE
#include <stdio.h>
#endif

EXTERN_C void xs_init (pTHX);

tTHX init_perl(){
    int my_argc = 3;
    char *my_argv[] = { "", "-e", "0", NULL };
    char *my_env[] = { NULL };
    PERL_SYS_INIT3(&my_argc, (char ***)&my_argv, (char ***)&my_env);
    tTHX aTHX = perl_alloc();
    perl_construct(aTHX);
    perl_parse(aTHX_ xs_init, 3, my_argv, NULL);
#ifdef TRACK_PERL_GLUE
    printf("init_perl() %p", (void*)aTHX);
    puts(PL_bincompat_options);
#endif
    return my_perl;
}

void exit_perl(pTHX){
#ifdef TRACK_PERL_GLUE
    printf("exit_perl(%p)\n", (void*)aTHX);
#endif
    perl_destruct(aTHX);
    perl_free(aTHX);
    PERL_SYS_TERM();
}

SV* glue_newSV(pTHX_ const STRLEN len){
#ifdef TRACK_PERL_GLUE
    puts("glue_newSV()");
#endif
    return newSV(len);
}

void glue_sv_setpvn(pTHX_ SV *sv, const char *str, const STRLEN len){
#ifdef TRACK_PERL_GLUE
    puts("glue_sv_setpvn()");
#endif
    sv_setpvn(sv, str, len);
}

SV *glue_eval_pv(pTHX_ const char *p, I32 croak_on_error){
#ifdef TRACK_PERL_GLUE
    puts("glue_eval_pv()");
#endif
    return eval_pv(p, croak_on_error);
}
