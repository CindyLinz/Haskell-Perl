#include <EXTERN.h>
#include <perl.h>

#include <stdlib.h>
#ifdef TRACK_PERL_GLUE
#include <stdio.h>
#endif

EXTERN_C void xs_init (pTHX);

static int my_argc = 3;
static char *my_argv[] = { "", "-e",
    "sub abs{abs$_[0]}\n"
    "sub accept{accept$_[0],$_[1]}\n"
    "sub alarm{alarm$_[0]}\n"
    "sub atan2{atan2$_[0],$_[1]}\n"
    "sub bind{bind$_[0],$_[1]}\n"
    "sub binmode{binmode$_[0],$_[1]}\n"
    "sub bless{bless$_[0],$_[1]}\n"
    "sub caller{caller$_[0]}\n"
    "sub chdir{chdir$_[0]}\n"
    "sub chmod{chmod@_}\n"
    "sub chomp{chomp@_}\n"
    "sub chop{chop@_}\n"
    "sub chown{chown@_}\n"
    "sub chr{chr$_[0]}\n"
    "sub chroot{chroot$_[0]}\n"
    "sub close{close$_[0]}\n"
    "sub closedir{closedir$_[0]}\n"
    "sub connect{connect$_[0],$_[1]}\n"
    "sub cos{cos$_[0]}\n"
    "sub crypt{crypt$_[0],$_[1]}\n"
    "sub defined{defined$_[0]}\n"
    "sub die{die@_}\n"
    "sub each{each$_[0]}\n"
    "sub endgrent{endgrent}\n"
    "sub endhostent{endhostent}\n"
    "sub endnetent{endnetent}\n"
    "sub endprotoent{endprotoent}\n"
    "sub endpwent{endpwent}\n"
    "sub endservent{endservent}\n"
    "sub eof{eof$_[0]}\n"
    "sub eval{eval$_[0]}\n"
    "sub exec{exec@_}\n"
    "sub exit{exit$_[0]}\n"
    "sub exp{exp$_[0]}\n"
    "sub fcntl{fcntl$_[0],$_[1],$_[2]}\n"
    "sub fileno{fileno$_[0]}\n"
    "sub flock{flock$_[0],$_[1]}\n"
    "sub fork{fork}\n"
    "sub formline{formline$_[0],$_[1]}\n"
    "sub getc{getc$_[0]}\n"
    "sub getgrent{getgrent}\n"
    "sub getgrgid{getgrgid$_[0]}\n"
    "sub getgrnam{getgrnam$_[0]}\n"
    "sub gethostbyaddr{gethostbyaddr$_[0],$_[1]}\n"
    "sub gethostbyname{gethostbyname$_[0]}\n"
    "sub gethostent{gethostent}\n"
    "sub getlogin{getlogin}\n"
    "sub getnetbyaddr{getnetbyaddr$_[0],$_[1]}\n"
    "sub getnetbyname{getnetbyname$_[0]}\n"
    "sub getnetent{getnetent}\n"
    "sub getpeername{getpeername$_[0]}\n"
    "sub getpgrp{getpgrp$_[0]}\n"
    "sub getppid{getppid}\n"
    "sub getpriority{getpriority$_[0],$_[1]}\n"
    "sub getprotobyname{getprotobyname$_[0]}\n"
    "sub getprotobynumber{getprotobynumber$_[0]}\n"
    "sub getprotoent{getprotoent}\n"
    "sub getpwent{getpwent}\n"
    "sub getpwnam{getpwnam$_[0]}\n"
    "sub getpwuid{getpwuid$_[0]}\n"
    "sub getservbyname{getservbyname$_[0],$_[1]}\n"
    "sub getservbyport{getservbyport$_[0],$_[1]}\n"
    "sub getservent{getservent}\n"
    "sub getsockname{getsockname$_[0]}\n"
    "sub getsockopt{getsockopt$_[0],$_[1],$_[2]}\n"
    "sub glob{glob$_[0]}\n"
    "sub gmtime{gmtime$_[0]}\n"
    "sub grep{grep shift,@_}\n"
    "sub hex{hex$_[0]}\n"
    "sub index{index$_[0],$_[1],$_[2]}\n"
    "sub int{int$_[0]}\n"
    "sub ioctl{ioctl$_[0],$_[1],$_[2]}\n"
    "sub join{join@_}\n"
    "sub keys{keys$_[0]}\n"
    "sub kill{kill$_[0],$_[1]}\n"
    "sub lc{lc$_[0]}\n"
    "sub lcfirst{lcfirst$_[0]}\n"
    "sub length{length$_[0]}\n"
    "sub link{link$_[0],$_[1]}\n"
    "sub listen{listen$_[0],$_[1]}\n"
    "sub localtime{localtime$_[0]}\n"
    "sub log{log$_[0]}\n"
    "sub lstat{lstat$_[0]}\n"
    "sub map{map shift,@_}\n"
    "sub mkdir{mkdir$_[0],$_[1]}\n"
    "sub msgctl{msgctl$_[0],$_[1],$_[2]}\n"
    "sub msgget{msgget$_[0],$_[1]}\n"
    "sub msgrcv{msgrcv$_[0],$_[1],$_[2],$_[3],$_[4]}\n"
    "sub msgsnd{msgsnd$_[0],$_[1],$_[2]}\n"
    "sub oct{oct$_[0]}\n"
    "sub open{if(@_==2){open$_[0],$_[1]}elsif(@_==3){open$_[0],$_[1],$_[2]}elsif(@_==4){open$_[0],$_[1],$_[2],$_[3]}else{open$_[0]}}\n"
    "sub opendir{opendir$_[0],$_[1]}\n"
    "sub ord{ord$_[0]}\n"
    "sub pack{pack$_[0],$_[1]}\n"
    "sub pipe{pipe$_[0],$_[1]}\n"
    "sub pop{pop$_[0]}\n"
    "sub pos{pos$_[0]}\n"
    "sub print{print@_}\n"
    "sub printf{printf@_}\n"
    "sub prototype{prototype$_[0]}\n"
    "sub push{push@_}\n"
    "sub quotemeta{quotemeta$_[0]}\n"
    "sub rand{rand($_[0]//1)}\n"
    "sub read{read$_[0],$_[1],$_[2],$_[3]}\n"
    "sub readdir{readdir$_[0]}\n"
    "sub readlink{readlink$_[0]}\n"
    "sub readpipe{readpipe$_[0]}\n"
    "sub recv{recv$_[0],$_[1],$_[2],$_[3]}\n"
    "sub ref{ref$_[0]}\n"
    "sub rename{rename$_[0],$_[1]}\n"
    "sub reset{reset$_[0]}\n"
    "sub reverse{reverse@_}\n"
    "sub rewinddir{rewinddir$_[0]}\n"
    "sub rindex{if(@_==3){rindex$_[0],$_[1],$_[2]}else{rindex$_[0],$_[1]}}\n"
    "sub rmdir{rmdir$_[0]}\n"
    "sub seek{seek$_[0],$_[1],$_[2]}\n"
    "sub seekdir{seekdir$_[0],$_[1]}\n"
    "sub select{if(@_==1){select$_[0]}else{select$_[0],$_[1],$_[2],$_[3]}}\n"
    "sub semctl{semctl$_[0],$_[1],$_[2],$_[3]}\n"
    "sub semget{semget$_[0],$_[1],$_[2]}\n"
    "sub semop{semop$_[0],$_[1]}\n"
    "sub send{send$_[0],$_[1],$_[2],$_[3]}\n"
    "sub setgrent{setgrent}\n"
    "sub sethostent{sethostent$_[0]}\n"
    "sub setnetent{setnetent$_[0]}\n"
    "sub setpgrp{setpgrp$_[0],$_[1]}\n"
    "sub setpriority{setpriority$_[0],$_[1],$_[2]}\n"
    "sub setprotoent{setprotoent$_[0]}\n"
    "sub setpwent{setpwent}\n"
    "sub setservent{setservent$_[0]}\n"
    "sub setsockopt{setsockopt$_[0],$_[1],$_[2],$_[3]}\n"
    "sub shift{shift$_}\n"
    "sub shmctl{shmctl$_[0],$_[1],$_[2]}\n"
    "sub shmget{shmget$_[0],$_[1],$_[2]}\n"
    "sub shmread{shmread$_[0],$_[1],$_[2],$_[3]}\n"
    "sub shmwrite{shmwrite$_[0],$_[1],$_[2],$_[3]}\n"
    "sub shutdown{shutdown$_[0],$_[1]}\n"
    /*"sub sin{sin$_[0]}\n"*/
    "sub sleep{sleep$_[0]}\n"
    "sub socket{socket$_[0],$_[1],$_[2],$_[3]}\n"
    "sub socketpair{socketpair$_[0],$_[1],$_[2],$_[3],$_[4]}\n"
    "sub sort{sort@_}\n"
    "sub splice{if(@_==1){splice$_[0]}elsif(@_==2){splice$_[0],$_[1]}elsif(@_==3){splice$_[0],$_[1],$_[2]}else{splice$_[0],$_[1],$_[2],$_[3]}}\n"
    "sub split{split$_[0],$_[1],$_[2]}\n"
    "sub sprintf{sprintf@_}\n"
    "sub sqrt{sqrtr$_[0]}\n"
    "sub srand{srand$_[0]}\n"
    "sub stat{stat$_[0]}\n"
    "sub study{study$_[0]}\n"
    "sub substr{if(@_==2){substr$_[0],$_[1]}elsif(@_==3){substr$_[0],$_[1],$_[2]}else{substr$_[0],$_[1],$_[2],$_[3]}}\n"
    "sub symlink{symlink$_[0],$_[1]}\n"
    "sub syscall{syscall$_[0],$_[1]}\n"
    "sub sysopen{sysopen$_[0],$_[1],$_[2],$_[3]}\n"
    "sub sysread{sysread$_[0],$_[1],$_[2],$_[3]}\n"
    "sub sysseek{sysseek$_[0],$_[1],$_[2]}\n"
    "sub system{system@_}\n"
    "sub syswrite{if(@_==2){syswrite$_[0],$_[1]}elsif(@_==3){syswrite$_[0],$_[1],$_[2]}else{syswrite$_[0],$_[1],$_[2],$_[3]}}\n"
    "sub tell{tell$_[0]}\n"
    "sub telldir{telldir$_[0]}\n"
    "sub tie{tie$_[0],$_[1],@_[2..$#_]}\n"
    "sub tied{tied$_[0]}\n"
    "sub time{time}\n"
    "sub times{times}\n"
    "sub truncate{truncate$_[0],$_[1]}\n"
    "sub uc{uc$_[0]}\n"
    "sub ucfirst{ucfirst$_[0]}\n"
    "sub umask{umask$_[0]}\n"
    "sub unlink{unlink@_}\n"
    "sub unpack{unpack$_[0],$_[1]}\n"
    "sub unshift{unshift shift,@_}\n"
    "sub untie{untie$_[0]}\n"
    "sub utime{utime@_}\n"
    "sub values{values$_[0]}\n"
    "sub vec{vec$_[0],$_[1],$_[2]}\n"
    "sub wait{wait}\n"
    "sub waitpid{waitpid$_[0],$_[1]}\n"
    "sub wantarray{wantarray}\n"
    "sub warn{warn@_}\n"
    "sub write{write$_[0]}\n"
    "sub sin{sin$_[0]}\n"
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

/* call */

/* return: num of return values
 */
I32 glue_call_pv(pTHX_ const char *sub_name, I32 flags, I32 argc, SV **argv, /* out */SV ***outv){
    dSP;
    PUSHMARK(SP);
#ifdef TRACK_PERL_GLUE
    printf("glue_call_pv sub_name=%s, flags=%d, argc=%d, argv=%p, outv=%p\n", sub_name, flags, argc, (void*)argv, (void*)outv);
#endif
    if( argc ){
        EXTEND(SP, argc);
        { I32 i; for(i=0; i<argc; ++i){
            PUSHs(argv[i]);
        } }
        PUTBACK;
    }
    {
        I32 count = call_pv(sub_name, flags);
        if( count > 0 ){
            SV **rets = *outv = (SV**) malloc(sizeof(SV*) * count);
            { I32 i; for(i=count-1; i>=0; --i){
                rets[i] = POPs;
            } }
            PUTBACK;
        }
        return count;
    }
}
