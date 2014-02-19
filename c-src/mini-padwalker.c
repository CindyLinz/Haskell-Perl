/* This file is stolen and modified from CPAN PadWalker 1.98 by Robin Houston
 * http://search.cpan.org/~robin/PadWalker-1.98/
 */
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#ifndef isGV_with_GP
#define isGV_with_GP(x) isGV(x)
#endif

#ifndef CxOLD_OP_TYPE
#  define CxOLD_OP_TYPE(cx)      (0 + (cx)->blk_eval.old_op_type)
#endif

/* For development testing */
#ifdef PADWALKER_DEBUGGING
# define debug_print(x) printf x
#else
# define debug_print(x)
#endif

/* For debugging */
#ifdef PADWALKER_DEBUGGING
static char *
cxtype_name(U32 cx_type1)
{
  switch(cx_type1 & CXTYPEMASK)
  {
    case CXt_NULL:   return "null";
    case CXt_SUB:    return "sub";
    case CXt_EVAL:   return "eval";
    //case CXt_LOOP:   return "loop";
    case CXt_SUBST:  return "subst";
    case CXt_BLOCK:  return "block";
    case CXt_FORMAT: return "format";

    default:         debug_print(("Unknown context type 0x%lx\n", cx_type1));
                                         return "(unknown)";
  }
}

static void
show_cxstack(void)
{
    I32 i;
    for (i = cxstack_ix; i>=0; --i)
    {
        printf(" =%ld= %s (%lx)", (long)i,
            cxtype_name(CxTYPE(&cxstack[i])), cxstack[i].blk_oldcop->cop_seq);
        if (CxTYPE(&cxstack[i]) == CXt_SUB) {
              CV *cv = cxstack[i].blk_sub.cv;
              printf("\t%s", (cv && CvGV(cv)) ? GvNAME(CvGV(cv)) :"(null)");
        }
        printf("\n");
    }
}
#else
# define show_cxstack()
#endif

#ifndef SvOURSTASH
# ifdef OURSTASH
#  define SvOURSTASH OURSTASH
# else
#  define SvOURSTASH GvSTASH
# endif
#endif

#ifndef COP_SEQ_RANGE_LOW
#  define COP_SEQ_RANGE_LOW(sv)                  U_32(SvNVX(sv))
#endif
#ifndef COP_SEQ_RANGE_HIGH
#  define COP_SEQ_RANGE_HIGH(sv)                 U_32(SvUVX(sv))
#endif

#ifndef PadARRAY
typedef AV PADNAMELIST;
typedef SV PADNAME;
# if PERL_VERSION < 8 || (PERL_VERSION == 8 && !PERL_SUBVERSION)
typedef AV PADLIST;
typedef AV PAD;
# endif
# define PadlistARRAY(pl)	((PAD **)AvARRAY(pl))
# define PadlistMAX(pl)		AvFILLp(pl)
# define PadlistNAMES(pl)	(*PadlistARRAY(pl))
# define PadnamelistARRAY(pnl)	((PADNAME **)AvARRAY(pnl))
# define PadnamelistMAX(pnl)	AvFILLp(pnl)
# define PadARRAY		AvARRAY
# define PadnameIsOUR(pn)	!!(SvFLAGS(pn) & SVpad_OUR)
# define PadnameOURSTASH(pn)	SvOURSTASH(pn)
# define PadnameOUTER(pn)	!!SvFAKE(pn)
# define PadnamePV(pn)		(SvPOKp(pn) ? SvPVX(pn) : NULL)
#endif


/* Originally stolen from pp_ctl.c; now significantly different */

static I32
dopoptosub_at(pTHX_ PERL_CONTEXT *cxstk, I32 startingblock)
{
    dTHR;
    I32 i;
    PERL_CONTEXT *cx;
    for (i = startingblock; i >= 0; i--) {
        cx = &cxstk[i];
        switch (CxTYPE(cx)) {
        default:
            continue;
        case CXt_SUB:
        /* In Perl 5.005, formats just used CXt_SUB */
#ifdef CXt_FORMAT
       case CXt_FORMAT:
#endif
            debug_print(("**dopoptosub_at: found sub #%ld\n", (long)i));
            return i;
        }
    }
        debug_print(("**dopoptosub_at: not found #%ld\n", (long)i));
    return i;
}

static I32
dopoptosub(pTHX_ I32 startingblock)
{
    dTHR;
    return dopoptosub_at(aTHX_ cxstack, startingblock);
}

/* This function is based on the code of pp_caller */
static PERL_CONTEXT*
upcontext(pTHX_ I32 count, COP **cop_p, PERL_CONTEXT **ccstack_p,
                                I32 *cxix_from_p, I32 *cxix_to_p)
{
    PERL_SI *top_si = PL_curstackinfo;
    I32 cxix = dopoptosub(aTHX_ cxstack_ix);
    PERL_CONTEXT *ccstack = cxstack;

    if (cxix_from_p) *cxix_from_p = cxstack_ix+1;
    if (cxix_to_p)   *cxix_to_p   = cxix;
    for (;;) {
        /* we may be in a higher stacklevel, so dig down deeper */
        while (cxix < 0 && top_si->si_type != PERLSI_MAIN) {
            top_si  = top_si->si_prev;
            ccstack = top_si->si_cxstack;
            cxix = dopoptosub_at(aTHX_ ccstack, top_si->si_cxix);
                        if (cxix_to_p && cxix_from_p) *cxix_from_p = *cxix_to_p;
                        if (cxix_to_p) *cxix_to_p = cxix;
        }
        if (cxix < 0 && count == 0) {
                    if (ccstack_p) *ccstack_p = ccstack;
            return (PERL_CONTEXT *)0;
                }
        else if (cxix < 0)
            return (PERL_CONTEXT *)-1;
        if (PL_DBsub && cxix >= 0 &&
                ccstack[cxix].blk_sub.cv == GvCV(PL_DBsub))
            count++;
        if (!count--)
            break;

        if (cop_p) *cop_p = ccstack[cxix].blk_oldcop;
        cxix = dopoptosub_at(aTHX_ ccstack, cxix - 1);
                        if (cxix_to_p && cxix_from_p) *cxix_from_p = *cxix_to_p;
                        if (cxix_to_p) *cxix_to_p = cxix;
    }
    if (ccstack_p) *ccstack_p = ccstack;
    return &ccstack[cxix];
}

/* end thievery */

static SV*
fetch_from_stash(HV *stash, char *name_str, U32 name_len)
{
    /* This isn't the most efficient approach, but it has
     * the advantage that it uses documented API functions. */
    char *package_name = HvNAME(stash);
    char *qualified_name;
    SV *ret = 0;  /* Initialise to silence spurious compiler warning */
    
    New(0, qualified_name, strlen(package_name) + 2 + name_len, char);
    strcpy(qualified_name, package_name);
    strcat(qualified_name, "::");
    strcat(qualified_name, name_str+1);

    debug_print(("fetch_from_stash: Looking for %c%s\n",
                 name_str[0], qualified_name));
    switch (name_str[0]) {
      case '$': ret =       get_sv(qualified_name, FALSE); break;
      case '@': ret = (SV*) get_av(qualified_name, FALSE); break;
      case '%': ret = (SV*) get_hv(qualified_name, FALSE); break;
      default:  die("PadWalker: variable '%s' of unknown type", name_str);
    }
    if (ret)
      debug_print(("%s\n", sv_peek(ret)));
    else
      /* I don't _think_ this should ever happen */
      debug_print(("XXXX - Variable %c%s not found\n",
                   name_str[0], qualified_name));
    Safefree(qualified_name);
    return ret;
}

static SV *
context_vars_name(PERL_CONTEXT *cx, const char *name, STRLEN namelen, U32 seq, CV *cv){
    /* If cx is null, we take that to mean that we should look
     * at the cv instead
     */
    if (cx == (PERL_CONTEXT*)-1)
        croak("Not nested deeply enough");

    else {
        CV*  cur_cv = cx ? cx->blk_sub.cv           : cv;
        long depth  = cx ? cx->blk_sub.olddepth + 1 : 1;

        if (!cur_cv)
            die("panic: Context has no CV!\n");
    
        while (cur_cv) {
            debug_print(("\tcv name = %s; depth=%ld\n",
                    CvGV(cur_cv) ? GvNAME(CvGV(cur_cv)) :"(null)", depth));
            if (CvPADLIST(cur_cv)){
                PADLIST* padlist = CvPADLIST(cur_cv);
                U32 valid_at_seq = seq;
                long depth_ = depth;
                {
                    PADNAMELIST *pad_namelist;
                    PAD *pad_vallist;
                    
                    if (depth_ == 0) depth_ = 1;

                    if (!padlist) {
                        /* Probably an XSUB */
                        die("PadWalker: cv has no padlist");
                    }
                    pad_namelist = PadlistNAMES(padlist);
                    pad_vallist  = PadlistARRAY(padlist)[depth_];

                    {
                        I32 i;

                        debug_print(("pads_into_hash(%p, %p, ..)\n",
                            (void*)pad_namelist, (void*) pad_vallist));

                        for (i=PadnamelistMAX(pad_namelist); i>=0; --i) {
                          PADNAME* name_sv = PadnamelistARRAY(pad_namelist)[i];

                          if (name_sv) {
                            char *name_str = PadnamePV(name_sv);
                            if (!name_str )
                                continue;

                            debug_print(("** %s (%lx,%lx) [%lx]%s\n", name_str,
                                   COP_SEQ_RANGE_LOW(name_sv), COP_SEQ_RANGE_HIGH(name_sv), valid_at_seq,
                                   PadnameOUTER(name_sv) ? " <fake>" : ""));

                            if ( strncmp(name, name_str, namelen)==0) {

                            debug_print(("** %s (%lx,%lx) [%lx]%s\n", name_str,
                                   COP_SEQ_RANGE_LOW(name_sv), COP_SEQ_RANGE_HIGH(name_sv), valid_at_seq,
                                   PadnameOUTER(name_sv) ? " <fake>" : ""));
                            
                            /* Check that this variable is valid at the cop_seq
                             * specified, by peeking into the NV and IV slots
                             * of the name sv. (This must be one of those "breathtaking
                             * optimisations" mentioned in the Panther book).

                             * Anonymous subs are stored here with a name of "&",
                             * so also check that the name is longer than one char.
                             * (Note that the prefix letter is here as well, so a
                             * valid variable will _always_ be >1 char)

                             * We ignore 'our' variables, since you can always dig
                             * them out of the stash directly.
                             */

                            if ((PadnameOUTER(name_sv) || 0 == valid_at_seq ||
                                (valid_at_seq <= COP_SEQ_RANGE_HIGH(name_sv) &&
                                valid_at_seq > COP_SEQ_RANGE_LOW(name_sv))) &&
                                strlen(name_str) > 1 )

                              {
                                SV *val_sv;
                                U32 name_len = strlen(name_str);
                                bool is_our = ((SvFLAGS(name_sv) & SVpad_OUR) != 0);

                                debug_print(((is_our ? "**     FOUND OUR %s\n"
                                                     : "**     FOUND MY %s\n"), name_str));

                                  if (is_our) {
                                    val_sv = fetch_from_stash(PadnameOURSTASH(name_sv),
                                                              name_str, name_len);
                                    if (!val_sv) {
                                        debug_print(("Value of our variable is undefined\n"));
                                        val_sv = &PL_sv_undef;
                                    }
                                  }
                                  else
                                  {
                                    val_sv =
                                      pad_vallist ? PadARRAY(pad_vallist)[i] : &PL_sv_undef;
                                    if (!val_sv) val_sv = &PL_sv_undef;
                                  }

                                  if( val_sv )
                                      return val_sv;
                              }
                            }
                          }
                        }
                    }
                }
            }
            cur_cv = CvOUTSIDE(cur_cv);
            if (cur_cv) depth  = CvDEPTH(cur_cv);
        }
    }
    return NULL;
}

SV *
perl_pad_peek_pvn(pTHX_ const char *name, STRLEN namelen){
    PERL_CONTEXT *cx, *ccstack;
    COP *cop = 0;
    I32 cxix_from, cxix_to, i;
    bool first_eval = TRUE;
    SV *ret = NULL;

    debug_print(("pad_keep_pvn by %s (len=%d)\n", name, namelen));

    show_cxstack();
    if (PL_curstackinfo->si_type != PERLSI_MAIN)
          debug_print(("!! We're in a higher stack level\n"));

    cx = upcontext(aTHX_ 0, &cop, &ccstack, &cxix_from, &cxix_to);
    debug_print(("** cxix = (%ld,%ld)\n", cxix_from, cxix_to));
    if (cop == 0) {
           debug_print(("**Setting cop to PL_curcop\n"));
           cop = PL_curcop;
        }
    debug_print(("**Cop file = %s\n", CopFILE(cop)));

    ret = context_vars_name(cx, name, namelen, cop->cop_seq, PL_main_cv);
    if( ret )
        return ret;

    for (i = cxix_from-1; !ret && i > cxix_to; --i) {
        debug_print(("** CxTYPE = %s (cxix = %ld)\n",
            cxtype_name(CxTYPE(&ccstack[i])), i));
        switch (CxTYPE(&ccstack[i])) {
        case CXt_EVAL:
            debug_print(("\told_op_type = %ld\n", CxOLD_OP_TYPE(&ccstack[i])));
            switch(CxOLD_OP_TYPE(&ccstack[i])) {
            case OP_ENTEREVAL:
                if (first_eval) {
                   ret = context_vars_name(0, name, namelen, cop->cop_seq, ccstack[i].blk_eval.cv);
                   if( ret )
                     return ret;
                   first_eval = FALSE;
                }
                ret = context_vars_name(0, name, namelen, ccstack[i].blk_oldcop->cop_seq,
                                                ccstack[i].blk_eval.cv);
                if( ret )
                    return ret;
                break;
            case OP_REQUIRE:
            case OP_DOFILE:
                debug_print(("blk_eval.cv = %p\n", (void*) ccstack[i].blk_eval.cv));
                if (first_eval)
                   ret = context_vars_name(0, name, namelen,
                    cop->cop_seq, ccstack[i].blk_eval.cv);
                return ret;
                /* If it's OP_ENTERTRY, we skip this altogether. */
            }
            break;

        case CXt_SUB:
#ifdef CXt_FORMAT
        case CXt_FORMAT:
#endif
                Perl_die(aTHX_ "PadWalker: internal error");
                    exit(EXIT_FAILURE);
        }
    }
    return NULL;
}
