/* defines.h	- Definitions of things needed in all C files
 *
 * $Revision: 2.75 $
 * $Date: 2008/05/19 13:42:39 $
 */

#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <errno.h>

/* Defines for ifdef'd code -- define them in the Makefile */

//#define DEBUG			// adds in debugging code
//#define ZERO_MEMORY		// zeros all of process memory before it starts to run
#define NATIVES			// allows native binaries and PDP-11 binaries in the filespace
#define EMU211			// add 2.11BSD emulation
#define EMUV1			// add 1st Edition emulation

/* Special defines to enable/disable certain
 * functionality. These are added as required
 * to port to new platforms. Please send in new
 * defines, please!
 */

#if defined(__FreeBSD__) && __FreeBSD__ < 3
#define NO_GETPGID
#endif

#ifdef __FreeBSD__
#define Reboot(x) reboot(x)
#endif

#ifdef __linux__
#define NO_CHFLAGS
#define NO_STFLAGS
#define NO_GETPGID
#define NEED_MAP_FCNTL
#define SIGEMT 0
#ifndef SIGSYS
#define SIGSYS 0
#endif
#define OXTABS XTABS
#define VDSUSP VSUSP		/* I don't think these are equivalent */
#define O_SHLOCK 0
#define O_EXLOCK 0
#endif

#if defined(__NetBSD__) || defined(__OpenBSD__)
#define Reboot(x) reboot(x,NULL)
#endif

#ifndef Reboot
#define Reboot(x) exit(0)
#endif

#if !defined(__FreeBSD__) && !defined(__NetBSD__) && \
    !defined(__OpenBSD__) && !defined(__linux__) && !defined(__APPLE__)
#define NEED_INT_N
#endif

/* Type definitions for PDP data types. You may need to
 * define NEED_INT_N if your system  doesn't provide the
 * types defined below. If you do this, the best way is
 * to add some #if .. #define .. #endif lines above,
 * rather then modifying the ones below. If you make
 * changes to the #if's above, then I would  be very
 * happy to include them.
 *
 * Warren Toomey: wkt@tuhs.org
 */

#ifdef NEED_INT_N
typedef char int8_t;
typedef short int16_t;
typedef long int32_t;
typedef unsigned char u_int8_t;
typedef unsigned short u_int16_t;
typedef unsigned long u_int32_t;
#endif

/* Macro defines for debug output, makes
 * the code look somewhat cleaner
 */

#ifdef DEBUG
#define TrapDebug(x) if (trap_debug) (void)fprintf x
#define InstDebug(x) if (inst_debug) (void)fprintf x
#define JsrDebug(x)  if (jsr_debug)  (void)fprintf x
#define FpDebug(x)   if (fp_debug)   (void)fprintf x
#else
#define TrapDebug(x)
#define InstDebug(x)
#define JsrDebug(x)
#define FpDebug(x)
#endif

/* Defines for -DSTREAM_BUFFERING */
#define NFILE   40		/* Number of file pointers we can buffer */
#define ValidFD(x) ((x>=0) && (x<NFILE))
/* Used for opening on directories */
#define TMP_PLATE       "/tmp/apout_tmp_dir.XXXXXX"

#define SP	6		/* stack pointer */
#define PC	7		/* program counter */

#define PDP_MEM_SIZE	65536	/* Size of inst-space and data-space */
#define MAX_ARGS	200	/* Max cmd-line args per process */

/* Global variables. */

extern u_int16_t regs[8];	/* general registers */
extern u_int16_t ir;		/* current instruction register */
extern int CC_N;		/* The processor status word is represented */
extern int CC_Z;		/* by these four values. On some */
extern int CC_V;		/* architectures, you may get a performance */
extern int CC_C;		/* increase by changing the size of the vars */

extern u_int8_t *ispace, *dspace;
extern u_int16_t dwrite_base;	/* Lowest addr where dspace writes can occur */


/* The following array holds the FILE pointers
 * that correspond to open file descriptors.
 * Only fds which are not ttys have
 * FILE * pointers
 */
extern FILE *stream[NFILE];
extern char *streammode[NFILE];

extern int sig_arrived;		/* Indicates if a signal has arrived */
extern int Argc, Envc;		/* Arguments passed to new process */
extern char *Argv[MAX_ARGS], *Envp[MAX_ARGS];
extern int Binary;		/* Type of binary this a.out is. One of: */
#define IS_UNKNOWN	0
#define IS_V1		1
#define IS_V2		2
#define IS_V3		3
#define IS_V4		4
#define IS_V5		5
#define IS_V6		6
#define IS_V7		7
#define IS_A68		68
#define IS_29BSD	29
#define IS_211BSD	211

/* 2.11BSD overlay stuff */
extern u_int32_t ov_changes;	/* Number of overlay changes */
extern u_int8_t current_ov;	/* Current overlay number */

#ifdef DEBUG
/* Debugging flags */
extern int inst_debug,		/* Print a line before each instruction */
       trap_debug,			/* Print details of each trap */
       jsr_debug,			/* Print out each jsr */
       fp_debug;			/* Print out each floating-point instruction */
extern FILE *dbg_file;		/* Debugging output file */
extern char *progname;		/* The program's name - used in debugging */
#endif

/* We keep a list of signals that are pending */
struct our_siglist {
    int sig;			/* Number of the signal */
    struct our_siglist *next;
};
extern struct our_siglist *Sighead;	/* Head of the list */
extern struct our_siglist *Sigtail;	/* Tail of the list */

#define CLR_CC_C()	CC_C=0
#define SET_CC_C()	CC_C=1

extern u_int16_t *adptr;

#define copylong(to,from) \
	buf = (char *) &(to); buf2 = (char *) &(from); \
	buf[0]=buf2[2]; buf[1]=buf2[3]; buf[2]=buf2[0]; buf[3]=buf2[1]

#ifndef EMUV1
/* lli_word() - Load a word from the given ispace logical address. */
#define lli_word(addr, word) \
	{ adptr= (u_int16_t *)&(ispace[addr]); word= *adptr; }

/* ll_word() - Load a word from the given logical address. */
#define ll_word(addr, word) \
	{ adptr= (u_int16_t *)&(dspace[addr]); word= *adptr; }

/* sl_word() - Store a word at the given logical address. */
#define sl_word(addr, word) \
	{ adptr= (u_int16_t *)&(dspace[addr]); *adptr= word; }

#else
/* These versions of the macros are required */
/* because the KE11-A module is mapped into */
/* a process' memory space in 1st Edition */
#define KE11LO	0177300
#define KE11HI	0177317

/* lli_word() - Load a word from the given ispace logical address. */
#define lli_word(addr, word) \
	{ if ((Binary<IS_V3) && (addr>=KE11LO) && (addr<=KE11HI)) {	\
		word= kell_word(addr);					\
	  } else { adptr= (u_int16_t *)&(ispace[addr]); word= *adptr; }	\
	}

/* ll_word() - Load a word from the given logical address. */
#define ll_word(addr, word) \
	{ if ((Binary<IS_V3) && (addr>=KE11LO) && (addr<=KE11HI)) {	\
		word= kell_word(addr);					\
	  } else { adptr= (u_int16_t *)&(dspace[addr]); word= *adptr; }	\
	}

/* sl_word() - Store a word at the given logical address. */
#define sl_word(addr, word) \
	{ if ((Binary<IS_V3) && (addr>=KE11LO) && (addr<=KE11HI)) {	\
		kesl_word(addr, word);					\
	  } else { adptr= (u_int16_t *)&(dspace[addr]); *adptr= word; }	\
	}
#endif

typedef void (*TrapFunc)(void);
extern TrapFunc emt_func, trap_func;

#ifdef __cplusplus
extern "C" {
#endif
int load_a_out(const char *file, const char *origpath, int want_env);
void set_apout_root(char *dirname);
void run(void);
int special_magic(u_int16_t * cptr);
char *xlate_filename(char *name);
void bus_error(int);
void sim_init(void);
void Reset(void);
void Execute(int n);
void RegSet(uint16_t num, uint16_t data);
void v1trap(void);
void v7trap(void);
void DumpTrace(void);
int8_t kell_byte(u_int16_t addr);
int16_t kell_word(u_int16_t addr);
void kesl_byte(u_int16_t addr, u_int8_t byte);
void kesl_word(u_int16_t addr, u_int16_t word);
void emt(void);
void bsdtrap(void);
void do_bsd_overlay(void);
void sigcatcher(int sig);
int trap_ioctl(void);
void set_bsdsig_dfl(void);
int emt_or_trap(uint16_t op);
#ifdef __cplusplus
}
#endif
