#include <EXTERN.h>
#include <perl.h>

#ifdef TRACK_PERL_GLUE
#include <stdio.h>
#endif

EXTERN_C void xs_init (pTHX);

static int my_argc = 3;
static char *my_argv[] = { "", "-e", "0", NULL };
static char *my_env[] = { NULL };

static int running_perl = 0;

static void very_init_perl(){
    PERL_SYS_INIT3(&my_argc, (char ***)&my_argv, (char ***)&my_env);
}

static void very_exit_perl(){
    PERL_SYS_TERM();
}

tTHX init_perl(){
    tTHX aTHX;
    if( ++running_perl == 1 )
        very_init_perl();

    aTHX = perl_alloc();
    perl_construct(aTHX);
    perl_parse(aTHX_ xs_init, my_argc, my_argv, my_env);
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

    if( --running_perl == 0 )
        very_exit_perl();
}

SV* glue_newSV(pTHX_ const STRLEN len){
#ifdef TRACK_PERL_GLUE
    printf("glue_newSV(%p)\n", (void*)aTHX);
#endif
    return newSV(len);
}

void glue_sv_setpvn(pTHX_ SV *sv, const char *str, const STRLEN len){
#ifdef TRACK_PERL_GLUE
    printf("glue_sv_setpvn(%p)\n", (void*)aTHX);
#endif
    sv_setpvn(sv, str, len);
}

SV *glue_eval_pv(pTHX_ const char *p, I32 croak_on_error){
#ifdef TRACK_PERL_GLUE
    printf("glue_eval_pv(%p)\n", (void*)aTHX);
#endif
    return eval_pv(p, croak_on_error);
}

SV *svREFCNT_inc(SV *sv){
    return SvREFCNT_inc(sv);
}

SV *svREFCNT_inc_NN(SV *sv){
    return SvREFCNT_inc_NN(sv);
}

SV *svREFCNT_inc_void(SV *sv){
    SvREFCNT_inc_void(sv);
}

IV svIVx(pTHX_ SV *sv){
    return SvIVx(sv);
}

NV svNVx(pTHX_ SV *sv){
    return SvNVx(sv);
}

char *svPVbytex(pTHX_ SV *sv, STRLEN *len){
    return SvPVbytex(sv, (*len));
}
