#include "DCJ11.h"

static DCJ11 cpu;

void Reset(uint16_t sp, uint16_t pc) { cpu.Reset(sp, pc); }

void Execute(int n) { cpu.Execute(n); }
