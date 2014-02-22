#include <EXTERN.h>
#include <perl.h>
#include "perl-glue.h"

#include <stdlib.h>
#include <string.h>

#ifdef TRACK_PERL_GLUE
#include <stdio.h>
#endif

EXTERN_C void xs_init (pTHX);
EXTERN_C void freeHaskellFunPtr(XSUBADDR_t);

static int my_argc = 3;
static char *my_argv[] = { "", "-e",
    PERL_BUILDIN_SUB_WRAPPER
, NULL };
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

SV *glue_eval_pv(pTHX_ const char *p, I32 croak_on_error){
#ifdef TRACK_PERL_GLUE
    printf("glue_eval_pv(%p)\n", (void*)aTHX);
#endif
    return eval_pv(p, croak_on_error);
}

/* ref count */

U32 svREFCNT(SV *sv){
    return SvREFCNT(sv);
}

void svREFCNT_dec(pTHX_ SV *sv){
    SvREFCNT_dec(sv);
}

/*
void svREFCNT_dec_NN(pTHX_ SV *sv){
    S_SvREFCNT_dec_NN(aTHX_ MUTABLE_SV(sv));
}
*/

SV *svREFCNT_inc(SV *sv){
    return SvREFCNT_inc(sv);
}

SV *svREFCNT_inc_NN(SV *sv){
    return SvREFCNT_inc_NN(sv);
}

void svREFCNT_inc_void(SV *sv){
    SvREFCNT_inc_void(sv);
}

void svREFCNT_inc_void_NN(SV *sv){
    SvREFCNT_inc_void_NN(sv);
}

SV *newSViv_mortal(pTHX_ IV i){
    return sv_2mortal(newSViv(i));
}

SV *newSVnv_mortal(pTHX_ NV n){
    return sv_2mortal(newSVnv(n));
}

void glue_sv_setpvn(pTHX_ SV *sv, const char *str, const STRLEN len){
#ifdef TRACK_PERL_GLUE
    printf("glue_sv_setpvn(%p)\n", (void*)aTHX);
#endif
    sv_setpvn(sv, str, len);
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

/* ref */

SV *svRV(SV *rv){
    return SvRV(rv);
}

int rvTYPE(SV *rv){
    return SvTYPE(SvRV(rv));
}

/* av */
void perl_av_unshift(pTHX_ AV *av, SV *sv){
    av_unshift(av, 1);
    av_store(av, 0, sv);
}

/* hv */
HV *perl_newHV(pTHX){
    return newHV();
}

SV **perl_hv_peek(pTHX_ HV *hv, const char *key, STRLEN klen){
    return hv_fetch(hv, key, klen, 0);
}

SV **perl_hv_fetch(pTHX_ HV *hv, const char *key, STRLEN klen){
    return hv_fetch(hv, key, klen, 1);
}

bool perl_hv_exists(pTHX_ HV *hv, const char *key, STRLEN klen){
    return hv_exists(hv, key, klen);
}

SV **perl_hv_store(pTHX_ HV *hv, const char *key, STRLEN klen, SV *val){
    return hv_store(hv, key, klen, val, 0);
}

SV *perl_hv_delete(pTHX_ HV *hv, const char *key, STRLEN klen){
    return hv_delete(hv, key, klen, 0);
}

/* call */

/* return: num of return values
 */
I32 glue_call_sv(pTHX_ SV *sub_sv, I32 flags, I32 argc, SV **argv, /* out */SV ***outv){
    dSP;
    PUSHMARK(SP);
    if( argc ){
        EXTEND(SP, argc);
        { I32 i; for(i=0; i<argc; ++i){
            PUSHs(argv[i]);
        } }
        PUTBACK;
    }
    {
        I32 count = call_sv(sub_sv, flags);
        if( count > 0 ){
            SV **rets = *outv = (SV**) malloc(sizeof(SV*) * count);
            SPAGAIN;
            { I32 i; for(i=count-1; i>=0; --i){
#ifdef TRACK_PERL_GLUE
                double n;
#endif
                rets[i] = POPs;
#ifdef TRACK_PERL_GLUE
                n = SvNVx(rets[i]);
                printf("prepare return %d NV=%f\n", i, n);
#endif
            } }
            PUTBACK;
        }
        return count;
    }
}

inline
I32 glue_call_pv(pTHX_ const char *sub_name, STRLEN namelen, I32 flags, I32 argc, SV **argv, /* out */SV ***outv){
    CV *sub_sv = get_cvn_flags(sub_name, namelen, 0);
#ifdef TRACK_PERL_GLUE
    printf("glue_call_pv sub_name=%s, flags=%d, argc=%d, argv=%p, outv=%p\n", sub_name, flags, argc, (void*)argv, (void*)outv);
#endif
    if( sub_sv==NULL ) {
        char builtin_sub_name[namelen+18];
        memcpy(builtin_sub_name,"HasPerl::builtin::", 18);
        memcpy(builtin_sub_name+18, sub_name, namelen);
        sub_sv = get_cvn_flags(builtin_sub_name, namelen+18, 0);
        if( sub_sv==NULL ) {
            dSP;
            PUSHMARK(SP);
            XPUSHs(sv_2mortal(newSVpvn(sub_name, namelen)));
            PUTBACK;
            call_sv((SV*)get_cvn_flags("HasPerl::make_builtin", 21, 0), G_VOID);
        }
        sub_sv = get_cvn_flags(builtin_sub_name, namelen+18, 0);
    }
    return glue_call_sv(aTHX_ (SV*)sub_sv, flags, argc, argv, outv);
}


static int haskell_cv_free(pTHX_ SV *cv, MAGIC *mg){
#ifdef TRACK_PERL_GLUE
    printf("free cv subaddr %p\n", (void*)CvXSUB((CV*)cv));
#endif
    freeHaskellFunPtr((XSUBADDR_t)((void*)CvXSUB((CV*)cv)));
    return 0;
}

static MGVTBL haskell_cv_vtbl = {
    0, 0, 0, 0,
    haskell_cv_free
};

SV *wrap_sub(pTHX_ XSUBADDR_t subaddr){
    SV *cv_ref;
    CV *cv = MUTABLE_CV(newSV_type(SVt_PVCV));
    CvFILE(cv) = "Haskell";
    CvANON_on(cv);
    CvISXSUB_on(cv);
    CvXSUB(cv) = subaddr;
    sv_magicext((SV*)cv, (SV*)cv, PERL_MAGIC_ext, &haskell_cv_vtbl, 0, 0);
    cv_ref = Perl_newRV_noinc(aTHX_ (SV*)cv);
#ifdef TRACK_PERL_GLUE
    printf("new cv %p %p %p\n", (void*)cv_ref, (void*)cv, (void*)subaddr);
#endif
    return cv_ref;
}

void reg_sub(pTHX_ const char *name, XSUBADDR_t subaddr){
    CV *cv = Perl_newXS(aTHX_ name, subaddr, "Haskell");
    sv_magicext((SV*)cv, (SV*)cv, PERL_MAGIC_ext, &haskell_cv_vtbl, 0, 0);
#ifdef TRACK_PERL_GLUE
    printf("reg cv %s %p %p\n", name, (void*)cv, (void*)subaddr);
#endif
}

I32 get_sub_arg_num(pTHX){
    return (I32)(PL_stack_sp - PL_stack_base - TOPMARK);
}

void get_sub_args(pTHX_ SV** out_buffer, I32 items){
    I32 i;
    const I32 ax = (I32)(TOPMARK + 1);
    for(i=0; i<items; ++i)
        out_buffer[i] = PL_stack_base[ax+i];
}

void set_sub_returns(pTHX_ SV** ret_buffer, I32 items){
    I32 i;
    dSP;
    const I32 ax = (I32)(POPMARK + 1);
    if( ax + items > AvMAX(PL_curstack) )
        EXTEND(PL_stack_base, ax + items);
    for(i=0; i<items; ++i)
        PL_stack_base[ax+i] = ret_buffer[i];
    PL_stack_sp = PL_stack_base + ax + items - 1;
}
