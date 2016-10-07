// To keep this file independent on a
// users system we will define all types here
// instead of trying to parse system headers

typedef unsigned long int size_t;
typedef long int ssize_t;
typedef int div_t;
typedef int ldiv_t;
typedef int lldiv_t;
typedef int wchar_t;


void  *g_malloc(size_t size);
void  *g_malloc0(size_t size);
void  *g_malloc_n(size_t size);
void  g_free(void *ptr);

void *malloc_vector(size_t size);

// sprintf((char *)&v48, "mtd_tool /dev/mtd/7 setenv PIN %s", &v49);

//int sprintf0(char * str, const char * format);
//int sprintf1(char * str, const char * format, char * arg1);
//int sprintf2(char * str, const char * format, char * arg1, char * arg2);
//int sprintf3(char * str, const char * format, char * arg1, char * arg2, char * arg3);
//int sprintf4(char * str, const char * format, char * arg1, char * arg2, char * arg3, char * arg4);
//int sprintf5(char * str, const char * format, char * arg1, char * arg2, char * arg3, char * arg4, char * arg5);

//int sprintf(char * str, const char * format, char * arg1);
int sprintf(char * str, const char * format, char * arg1, char * arg2, char * arg3, char * arg4, char * arg5);

int snprintf(char * str, size_t n, const char * format, char * arg1, char * arg2, char * arg3, char * arg4, char * arg5);

int sqlite3_exec(  void*, const char *sql, void *, void *, char **errmsg);

int gettimeofday(struct timeval *tv, struct timezone *tz);
signed int time (signed int* timer);

// stdlib.h

void          _Exit(int code);
long          a64l(const char *);
void          abort(void);
int           abs(int x);
int           atexit(void (*callback)(void));
double        atof(const char *);
int           atoi(const char *);
long          atol(const char *);
long long     atoll(const char *);
void         *bsearch(const void *key,
                      const void *base,
                      size_t nmemb,
                      size_t size,
                      int (*compare)(const void *, const void *));
void         *calloc(size_t nmemb, size_t size);
div_t         div(int numenator, int denominator);
double        drand48(void);
double        erand48(unsigned short [3]);
void          exit(int code);
void          free(void *ptr);
char         *getenv(const char *name);
int           getsubopt(char **, char *const *, char **);
int           grantpt(int);
char         *initstate(unsigned, char *, size_t);
long          jrand48(unsigned short [3]);
char         *l64a(long);
long          labs(long);
void          lcong48(unsigned short [7]);
ldiv_t        ldiv(long, long);
long long     llabs(long long);
lldiv_t       lldiv(long long, long long);
long          lrand48(void);
void         *malloc(size_t size);
int           mblen(const char *, size_t);
size_t        mbstowcs(wchar_t *, const char *, size_t);
int           mbtowc(wchar_t *, const char *, size_t);
char         *mkdtemp(char *);
int           mkstemp(char *);
long          mrand48(void);
long          nrand48(unsigned short [3]);
int           posix_memalign(void **, size_t, size_t);
int           posix_openpt(int);
char         *ptsname(int);
int           putenv(char *name);
void          qsort(void *base, size_t nmemb, size_t size, int (*compare)(const void *, const void *));
int           rand(void);
int           rand_r(unsigned *);
long          random(void);
void         *realloc(void *ptr, size_t size);
char         *realpath(const char *input, char *output);
unsigned short *seed48(unsigned short [3]);
int           setenv(const char *, const char *, int);
void          setkey(const char *);
char         *setstate(char *);
void          srand(unsigned);
void          srand48(long);
void          srandom(unsigned);
double        strtod(const char *, char **);
float         strtof(const char *, char **);
long          strtol(const char *, char **, int);
long double   strtold(const char *, char **);
long long     strtoll(const char *, char **, int);
unsigned long strtoul(const char *, char **, int);
unsigned long long
              strtoull(const char *, char **, int);
int           system(const char *command);
int           unlockpt(int);
int           unsetenv(const char *);
size_t        wcstombs(char *, const wchar_t *, size_t);
int           wctomb(char *, wchar_t);

// stdio.h
typedef void * FILE;

char *fgets(char *s, int size, FILE *stream);
char *gets(char *s);
FILE *fdopen(int fd, const char *mode);
FILE *fopen(const char *path, const char *mode);
FILE *freopen(const char *path, const char *mode, FILE *stream);
int fclose(FILE *fp);
int feof(FILE *stream);
int ferror(FILE *stream);
int fgetc(FILE *stream);
int fgetc(FILE *stream);
int fileno(FILE *stream);
int fputc(int c, FILE *stream);
int fputs(const char *s, FILE *stream);
int getc(FILE *stream);
int getchar(void);
int putchar(int c);
int putc(int c, FILE *stream);
int puts(const char *s);
int ungetc(int c, FILE *stream);
size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
void clearerr(FILE *stream);


// string.h

void    *memccpy(void *, const void *, int, size_t);
void    *memchr(const void *, int, size_t);
int      memcmp(const void *, const void *, size_t);
void    *memcpy(void *dst, const void *src, size_t n);
void    *memmove(void *, const void *, size_t);
void    *memset(void *, int, size_t);
int strcasecmp(const char *s1, const char *s2);
int strncasecmp(const char *s1, const char *s2, size_t n);
char *index(const char *s, int c);
char *rindex(const char *s, int c);
char *stpcpy(char *dest, const char *src);
char *strcat(char *dest, const char *src);
char *strchr(const char *s, int c);
int strcmp(const char *s1, const char *s2);
int strcoll(const char *s1, const char *s2);
char *strcpy(char *dest, const char *src);
size_t strcspn(const char *s, const char *reject);
char *strdup(const char *s);
char *strfry(char *string);
size_t strlen(const char *s);
char *strncat(char *dest, const char *src, size_t n);
int strncmp(const char *s1, const char *s2, size_t n);
char *strncpy(char *dest, const char *src, size_t n);
char *strpbrk(const char *s, const char *accept);
char *strrchr(const char *s, int c);
char *strsep(char **stringp, const char *delim);
size_t strspn(const char *s, const char *accept);
char *strstr(const char *haystack, const char *needle);
char *strtok(char *s, const char *delim);
size_t strxfrm(char *dest, const char *src, size_t n);


// unix
typedef int mode_t;
int open(const char *pathname, int flags);
int creat(const char *pathname, mode_t mode);

// socket
struct sockaddr;
typedef size_t socklen_t;

int     accept(int, struct sockaddr *, socklen_t *);
int     bind(int, const struct sockaddr *, socklen_t);
int     connect(int, const struct sockaddr *, socklen_t);
int     getpeername(int, struct sockaddr *, socklen_t *);
int     getsockname(int, struct sockaddr *, socklen_t *);
int     getsockopt(int, int, int, void *, socklen_t *);
int     listen(int, int);
ssize_t recv(int, void *, size_t, int);
ssize_t recvfrom(int, void *, size_t, int,
        struct sockaddr *, socklen_t *);
ssize_t recvmsg(int, struct msghdr *, int);
ssize_t send(int, const void *, size_t, int);
ssize_t sendmsg(int, const struct msghdr *, int);
ssize_t sendto(int, const void *, size_t, int, const struct sockaddr *,
        socklen_t);
int     setsockopt(int, int, int, const void *, socklen_t);
int     shutdown(int, int);
int     sockatmark(int);
int     socket(int, int, int);
int     socketpair(int, int, int, int [2]);

// unistd.h
typedef int uid_t;
typedef int gid_t;
typedef int pid_t;
typedef int off_t;

int          access(const char *, int);
unsigned     alarm(unsigned);
int          chdir(const char *);
int          chown(const char *, uid_t, gid_t);
int          close(int);
size_t       confstr(int, char *, size_t);
char        *crypt(const char *key, const char *salt);
int          dup(int);
int          dup2(int, int);
void         _exit(int);
void         encrypt(char [64], int);
int          execl(const char *, const char *, ...);
int          execle(const char *, const char *, ...);
int          execlp(const char *, const char *, ...);
int          execv(const char *, char *const []);
int          execve(const char *, char *const [], char *const []);
int          execvp(const char *, char *const []);
int          faccessat(int, const char *, int, int);
int          fchdir(int);
int          fchown(int, uid_t, gid_t);
int          fchownat(int, const char *, uid_t, gid_t, int);
int          fdatasync(int);
int          fexecve(int, char *const [], char *const []);
pid_t        fork(void);
long         fpathconf(int, int);
int          fsync(int);
int          ftruncate(int, off_t);
char        *getcwd(char *, size_t);
gid_t        getegid(void);
uid_t        geteuid(void);
gid_t        getgid(void);
int          getgroups(int, gid_t []);
long         gethostid(void);
int          gethostname(char *, size_t);
char        *getlogin(void);
int          getlogin_r(char *, size_t);
int          getopt(int, char * const [], const char *);
pid_t        getpgid(pid_t);
pid_t        getpgrp(void);
pid_t        getpid(void);
pid_t        getppid(void);
pid_t        getsid(pid_t);
uid_t        getuid(void);
int          isatty(int);
int          lchown(const char *, uid_t, gid_t);
int          link(const char *, const char *);
int          linkat(int, const char *, int, const char *, int);
int          lockf(int, int, off_t);
off_t        lseek(int, off_t, int);
int          nice(int);
long         pathconf(const char *, int);
int          pause(void);
int          pipe(int [2]);
ssize_t      pread(int, void *, size_t, off_t);
ssize_t      pwrite(int, const void *, size_t, off_t);
ssize_t      read(int, void *, size_t);
ssize_t      readv(int fd, const struct iovec *iov, int iovcnt);
ssize_t      readlink(const char *, char *, size_t);
ssize_t      readlinkat(int, const char *, char *, size_t);
int          rmdir(const char *);
int          setegid(gid_t);
int          seteuid(uid_t);
int          setgid(gid_t);
int          setpgid(pid_t, pid_t);
pid_t        setpgrp(void);
int          setregid(gid_t, gid_t);
int          setreuid(uid_t, uid_t);
pid_t        setsid(void);
int          setuid(uid_t);
unsigned     sleep(unsigned);
void         swab(const void *, void *, ssize_t);
int          symlink(const char *, const char *);
int          symlinkat(const char *, int, const char *);
void         sync(void);
long         sysconf(int);
pid_t        tcgetpgrp(int);
int          tcsetpgrp(int, pid_t);
int          truncate(const char *, off_t);
char        *ttyname(int);
int          ttyname_r(int, char *, size_t);
int          unlink(const char *);
int          unlinkat(int, const char *, int);
ssize_t      write(int, const void *, size_t);
