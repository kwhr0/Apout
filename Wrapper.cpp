#include "DCJ11.h"

static DCJ11 cpu;

void Reset() { cpu.Reset(); }

void RegSet(uint16_t num, uint16_t data) {
	regs[num] = data;
	cpu.SetGPR(num, data);
}

void Execute(int n) { cpu.Execute(n); }
