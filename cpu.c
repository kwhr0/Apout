/* cpu.c - this holds the main loop for the emulator, plus generic
 * functions to deal with exceptional instructions and events
 *
 * $Revision: 1.26 $
 * $Date: 2008/05/15 07:52:45 $
 */
#include "defines.h"
#include <unistd.h>

u_int8_t *ispace, *dspace;	/* Instruction and Data spaces */
u_int16_t dwrite_base = 2;	/* Lowest addr where dspace writes can occur */

u_int16_t regs[8];		/* general registers */
u_int16_t ir;			/* current instruction register */
u_int16_t *adptr;		/* used in memory access macros */

int CC_N = 0;			/* The processor status word is represented */
int CC_Z = 0;			/* by these four values. On some */
int CC_V = 0;			/* architectures, you may get a performance */
int CC_C = 0;			/* increase by using shorts or bytes */

/* Run until told to stop. */
void run() {
    while (1)
		Execute(10000);
}

/* sim_init() - Initialize the cpu registers. */
void sim_init(uint16_t sp, uint16_t pc) {
	if (Binary >= IS_V3) sp &= ~1; // 可能なら偶数にする
	Reset(sp, pc);
}

void bus_error(int signo)
{
	TrapDebug((dbg_file, "Apout - pid %d bus error at PC 0%06o\n",
			   (int) getpid(), regs[PC]));
	TrapDebug((dbg_file, "%06o  ", ir));
	TrapDebug((dbg_file, "%o %o %o %o %o %o %o %o  ",
			   regs[0], regs[1], regs[2], regs[3],
			   regs[4], regs[5], regs[6], regs[7]));
	TrapDebug((dbg_file, "NZVC2 are %d%d%d%d\n", CC_N, CC_Z, CC_V, CC_C));
	exit(EXIT_FAILURE);
}

void emt()
{
	TrapDebug((stderr, "Apout - pid %d emt instruction at PC 0%o\n",
			   (int) getpid(), regs[PC]));
	exit(EXIT_FAILURE);
}

void sigcatcher(int sig) {}
