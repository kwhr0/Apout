// DCJ11
// Copyright 2025 © Yasuo Kuwahara
// MIT License

#include "DCJ11.h"
#include <cmath>

// User Mode only
// unimplemented instructions: wrtlck,tstset,mark,bpt,iot,rti,rtt,spl,csm,halt,wait,reset,mfpt,mtpd,mtpi,stst
// emulator implementation: emt,trap
// exceptions and double precision not implemented

#define P(x)			(cnv.pmf = &DCJ11::x, cnv.p)
#define PI(x, i)		(cnv.pmf = &DCJ11::x<i>, cnv.p)
#define PW(x, i, j)		(cnv.pmf = &DCJ11::x<i, j>, cnv.p)
#define PWS(x, i, j, s)	(cnv.pmf = &DCJ11::x<i, j, s>, cnv.p)

#define MODE(op, mask, x) {\
a(op | 000, mask | 070, PI(x, 0));\
a(op | 010, mask | 070, PI(x, 1));\
a(op | 020, mask | 070, PI(x, 2));\
a(op | 030, mask | 070, PI(x, 3));\
a(op | 040, mask | 070, PI(x, 4));\
a(op | 050, mask | 070, PI(x, 5));\
a(op | 060, mask | 070, PI(x, 6));\
a(op | 070, mask | 070, PI(x, 7));\
}
#define MODE2(op, mask, x, m) {\
a(op | 000, mask | 070, PW(x, m, 0));\
a(op | 010, mask | 070, PW(x, m, 1));\
a(op | 020, mask | 070, PW(x, m, 2));\
a(op | 030, mask | 070, PW(x, m, 3));\
a(op | 040, mask | 070, PW(x, m, 4));\
a(op | 050, mask | 070, PW(x, m, 5));\
a(op | 060, mask | 070, PW(x, m, 6));\
a(op | 070, mask | 070, PW(x, m, 7));\
}
#define MODE3(op, mask, x, m, s) {\
a(op | 000, mask | 070, PWS(x, m, 0, s));\
a(op | 010, mask | 070, PWS(x, m, 1, s));\
a(op | 020, mask | 070, PWS(x, m, 2, s));\
a(op | 030, mask | 070, PWS(x, m, 3, s));\
a(op | 040, mask | 070, PWS(x, m, 4, s));\
a(op | 050, mask | 070, PWS(x, m, 5, s));\
a(op | 060, mask | 070, PWS(x, m, 6, s));\
a(op | 070, mask | 070, PWS(x, m, 7, s));\
}
#define MODEW(op, mask, x) {\
MODE2(op | 00000, mask | 07000, x, 0);\
MODE2(op | 01000, mask | 07000, x, 1);\
MODE2(op | 02000, mask | 07000, x, 2);\
MODE2(op | 03000, mask | 07000, x, 3);\
MODE2(op | 04000, mask | 07000, x, 4);\
MODE2(op | 05000, mask | 07000, x, 5);\
MODE2(op | 06000, mask | 07000, x, 6);\
MODE2(op | 07000, mask | 07000, x, 7);\
}
#define MODE4(op, mask, x, s) {\
a(op | 000, mask | 070, PW(x, 0, s));\
a(op | 010, mask | 070, PW(x, 1, s));\
a(op | 020, mask | 070, PW(x, 2, s));\
a(op | 030, mask | 070, PW(x, 3, s));\
a(op | 040, mask | 070, PW(x, 4, s));\
a(op | 050, mask | 070, PW(x, 5, s));\
a(op | 060, mask | 070, PW(x, 6, s));\
a(op | 070, mask | 070, PW(x, 7, s));\
}
#define MODE5(op, mask, x, s) {\
MODE3(op | 00000, mask | 07000, x, 0, s);\
MODE3(op | 01000, mask | 07000, x, 1, s);\
MODE3(op | 02000, mask | 07000, x, 2, s);\
MODE3(op | 03000, mask | 07000, x, 3, s);\
MODE3(op | 04000, mask | 07000, x, 4, s);\
MODE3(op | 05000, mask | 07000, x, 5, s);\
MODE3(op | 06000, mask | 07000, x, 6, s);\
MODE3(op | 07000, mask | 07000, x, 7, s);\
}
#define MODES(op, mask, x) {\
MODE4(op | 0000000, mask | 0100000, x, 1);\
MODE4(op | 0100000, mask | 0100000, x, 0);\
}
#define MODEWS(op, mask, x) {\
MODE5(op | 0000000, mask | 0100000, x, 1);\
MODE5(op | 0100000, mask | 0100000, x, 0);\
}

static struct Insn {
	using pmf_t = void (DCJ11::*)(uint16_t);
	using pf_t = void (*)(DCJ11 *, uint16_t);
	Insn() {
		union { pmf_t pmf; pf_t p; } cnv; // not portable
		for (int i = 0; i < 0x10000; i++) fn[i] = P(undef);
		MODES(0005000, 0077700, clr);
		MODES(0005100, 0077700, com);
		MODES(0005200, 0077700, inc);
		MODES(0005300, 0077700, dec);
		MODES(0005700, 0077700, tst);
		MODES(0005400, 0077700, neg);
		MODES(0006200, 0077700, asr);
		MODES(0006300, 0077700, asl);
		MODES(0006000, 0077700, ror);
		MODES(0006100, 0077700, rol);
		MODES(0000300, 0177700, swab);
		MODES(0005500, 0077700, adc);
		MODES(0005600, 0077700, sbc);
		MODE(0006700, 0177700, sxt);
		MODE(0106700, 0177700, mfps);
		MODE(0106400, 0177700, mtps);
		MODEWS(0010000, 0070000, mov);
		MODEW(0060000, 0170000, add);
		MODEW(0160000, 0170000, sub);
		MODE(0072000, 0177000, ash);
		MODE(0073000, 0177000, ashc);
		MODE(0070000, 0177000, mul);
		MODE(0071000, 0177000, div);
		MODEWS(0020000, 0070000, cmp);
		MODEWS(0030000, 0070000, bit);
		MODEWS(0040000, 0070000, bic);
		MODEWS(0050000, 0070000, bis);
		MODE(0074000, 0077000, _xor);
		a(0000400, 0177400, P(br));
		a(0001000, 0177400, P(bne));
		a(0001400, 0177400, P(beq));
		a(0100000, 0177400, P(bpl));
		a(0100400, 0177400, P(bmi));
		a(0102000, 0177400, P(bvc));
		a(0102400, 0177400, P(bvs));
		a(0103000, 0177400, P(bcc));
		a(0103400, 0177400, P(bcs));
		a(0002000, 0177400, P(bge));
		a(0002400, 0177400, P(blt));
		a(0003000, 0177400, P(bgt));
		a(0003400, 0177400, P(ble));
		a(0101000, 0177400, P(bhi));
		a(0101400, 0177400, P(blos));
		a(0103000, 0177400, P(bhis));
		MODE(0000100, 0177700, jmp);
		MODE(0004000, 0177000, jsr);
		a(0000200, 0177770, P(rts));
		a(0077000, 0177000, P(sob));
		a(0104000, 0177000, P(emttrap)); // emt/trap
		a(0000240, 0177740, P(cco)); // cln/sen/clz/sez/clv/sev/clc/sec/ccc/scc
		MODE(0170600, 0177700, absf);
		MODE(0172000, 0177400, addf);
		a(0170000, 0177777, P(cfcc));
		MODE(0170400, 0177700, clrf); // clrf/clrd
		MODE(0173400, 0177400, cmpf);
		MODE(0174400, 0177400, divf);
		MODE(0177000, 0177400, ldc); // ldcif/ldcid/ldclf/ldcld
		MODE(0176400, 0177400, ldexp);
		MODE(0177400, 0177400, ldf); // ldcdf/ldcfd
		MODE(0172400, 0177400, ldf); // ldf/ldd
		MODE(0170100, 0177700, ldfps);
		MODE(0171400, 0177400, modf);
		MODE(0171000, 0177400, mulf);
		MODE(0170700, 0177700, negf);
		a(0170001, 0177767, P(setdf));
		a(0170002, 0177767, P(setil));
		MODE(0176000, 0177400, stf); // stcfd/stcdf
		MODE(0175400, 0177400, stcfi); // stcfi/stcfl/stcdi/stcdl
		MODE(0175000, 0177400, stexp);
		MODE(0174000, 0177400, stf);
		MODE(0170200, 0177700, stfps);
		MODE(0173000, 0177400, subf);
		MODE(0170500, 0177400, tstf);
	}
	void a(uint16_t op, uint16_t mask, pf_t f) {
		int lim = (op & 0xf000) + 0x1000;
		for (int i = op & 0xf000; i < lim; i++)
			if ((i & mask) == op) fn[i] = f;
	}
	static void exec1(DCJ11 *mpu, uint16_t op) { fn[op](mpu, op); }
	static inline pf_t fn[0x10000];
} insn;

DCJ11::DCJ11() {
#if DCJ11_TRACE
	memset(tracebuf, 0, sizeof(tracebuf));
	tracep = tracebuf;
#endif
}

void DCJ11::Reset(u16 sp, u16 pc) {
	memset(gpr, 0, sizeof(gpr));
	memset(fpr, 0, sizeof(fpr));
	gpr[6] = sp;
	gpr[7] = pc;
	psw = fps = 0;
	islong = isdouble = false;
}

// addressing: 0,2,4,6...6-5  1,3,5,7...6-11,6-12
// RW: 0...address only 1...read 2...write 3...modify
// S: 0...byte 1...word 2...word/long depends on islong 3...float/double depends on isdouble
template<int RW, int M, int S, typename F> DCJ11::u16 DCJ11::ea(int reg, F func) {
	u16 adr = 0;
	reg &= 7;
	if constexpr (M == 1) adr = gpr[reg];
	if constexpr ((M & 6) == 2) {
		adr = gpr[reg];
		if constexpr (S == 0) gpr[reg] += reg >= 6 ? 2 : 1;
		if constexpr (S == 1) gpr[reg] += 2;
		if constexpr (S == 2) gpr[reg] += islong && reg != 7 ? 4 : 2;
		if constexpr (S == 3) gpr[reg] += isdouble ? 8 : 4;
	}
	if constexpr ((M & 6) == 4) {
		if constexpr (S == 0) gpr[reg] -= reg >= 6 ? 2 : 1;
		if constexpr (S == 1) gpr[reg] -= 2;
		if constexpr (S == 2) gpr[reg] -= islong && reg != 7 ? 4 : 2;
		if constexpr (S == 3) gpr[reg] -= isdouble ? 8 : 4;
		adr = gpr[reg];
	}
	if constexpr ((M & 6) == 6) {
		adr = fetch2();
		adr += gpr[reg];
	}
	if constexpr (M == 3 || M == 5 || M == 7) adr = ld2(adr);
	u32 data = 0;
	if constexpr ((RW & 1) != 0) {
		if constexpr (!M) {
			if constexpr (S == 0) data = gpr[reg] & 0xff;
			if constexpr (S == 1 || S == 2) data = gpr[reg];
			if constexpr (S == 3) data = fpr[reg & 3].i;
		}
		else {
			if constexpr (S == 0) data = reg == 7 ? ld1i(adr) : ld1(adr);
			if constexpr (S == 1) data = reg == 7 ? ld2i(adr) : ld2(adr);
			if constexpr (S == 2) data = islong ? ld4(adr) : ld2(adr);
			if constexpr (S == 3) data = ld4(adr);
		}
	}
	if constexpr (RW == 1) func(data);
	if constexpr (RW == 2) data = func();
	if constexpr (RW == 3) data = func(data);
	if constexpr ((RW & 2) != 0) {
		if constexpr (!M) {
			if constexpr (S == 0) stR(reg, (gpr[reg] & 0xff00) | (data & 0xff));
			if constexpr (S == 1 || S == 2) stR(reg, data);
			if constexpr (S == 3) stI(reg & 3, data);
		}
		else {
			if constexpr (S == 0) st1(adr, data);
			if constexpr (S == 1) st2(adr, data);
			if constexpr (S == 2) islong ? st4(adr, data) : st2(adr, data);
			if constexpr (S == 3) st4(adr, data);
		}
	}
	return adr;
}

template<int M> void DCJ11::ash(u16 op) {
	ea<1, M>(op, [&](s16 s) {
		s = s << 10 >> 10;
		s16 v = gpr[op >> 6 & 7], r = s >= 0 ? v << s : v >> -s;
		fash(r, s, v);
		stR(op >> 6 & 7, r);
	});
}

template<int M> void DCJ11::ashc(u16 op) {
	ea<1, M>(op, [&](s16 s) {
		s = s << 10 >> 10;
		s32 v = gpr[op >> 6 & 7] << 16 | gpr[(op >> 6 & 7) | 1], r = s >= 0 ? v << s : v >> -s;
		fash<2>(r, s, v);
		stR(op >> 6 & 7, r >> 16);
		stR((op >> 6 & 7) | 1, r);
	});
}

template<int M> void DCJ11::div(u16 op) {
	ea<1, M>(op, [&](s16 v) {
		s32 d = gpr[op >> 6 & 7] << 16 | gpr[(op >> 6 & 7) | 1], q = d / v, r = d % v; // in case v==0 ?
		fdiv(q, v, d);
		stR(op >> 6 & 7, q);
		stR((op >> 6 & 7) | 1, r);
	});
}

void DCJ11::emttrap(uint16_t op) { // emulator
	ir = op;
	memcpy(regs, gpr, sizeof(gpr));
	CC_N = psw & 8 ? 1 : 0;
	CC_Z = psw & 4 ? 1 : 0;
	CC_V = psw & 2 ? 1 : 0;
	CC_C = psw & 1;
	if (op & 0400)
		if (trap_func) trap_func();
		else { rts(op); return; }
	else if (emt_func) emt_func();
	else { rts(op); return; }
	memcpy(gpr, regs, sizeof(gpr));
	psw = (CC_N ? 8 : 0) | (CC_Z ? 4 : 0) | (CC_V ? 2 : 0) | (CC_C ? 1 : 0);
}

template<int M> float DCJ11::_ldf(u16 op) {
	if constexpr (M != 0) {
		FPR c;
		ea<1, M, 3>(op, [&](u32 v) { c.i = (v & ~MFS) >= EOFS ? (v & MFS) | (v - EOFS & ~MFS) : 0; });
		return c.f;
	}
	else return fpr[op & 3].f;
}

template<int M> void DCJ11::stf(u16 op) {
	u32 t = fpr[op >> 6 & 3].i;
	if constexpr (M != 0) ea<2, M, 3>(op, [&] { return t ? (t & MFS) | ((t | MFS) <= -1 - EOFS ? (t + EOFS & ~MFS) : MFE) : 0; });
	else stI(op & 3, t);
}

template<int M> void DCJ11::modf(u16 op) {
	float f = fpr[op >> 6 & 3].f *= _ldf<M>(op), g;
	fpr[(op >> 6 & 3) | 1].f = g = f >= 0.f ? floorf(f) : ceilf(f);
	ffp<1>(fpr[op >> 6 & 3].f = f - g);
}

template<int M> void DCJ11::stcfi(u16 op) {
	ea<2, M, 2>(op, [&] {
		int64_t t = fpr[op >> 6 & 3].f;
		if (islong)
			if (t == (s32)t) ffp((s32)t);
			else { ffp(t = 0); fps |= MC; }
		else if (t == (s16)t) ffp((s16)t);
		else { ffp(t = 0); fps |= MC; }
		cfcc(0);
		return (s32)t;
	});
}

int DCJ11::Execute(int n) {
	int cycle = 0;
	do {
#if DCJ11_TRACE
		tracep->pc = gpr[7];
		tracep->index = 0;
#endif
		Insn::exec1(this, fetch2());
#if DCJ11_TRACE
		tracep->psw = psw;
#if DCJ11_TRACE > 1
		if (++tracep >= tracebuf + TRACEMAX - 1) StopTrace();
#else
		if (++tracep >= tracebuf + TRACEMAX) tracep = tracebuf;
#endif
#endif
		cycle++;
	} while (cycle < n);
	return cycle - n;
}

#define BITS	(8 << S)
#define MSB_N	(BITS - 1)

template<int DM, int S> DCJ11::u16 DCJ11::fset(u32 r, u32 s, u32 d) {
	if constexpr ((DM & 0xf) == C0) psw &= ~MC;
	if constexpr ((DM & 0xf) == C1) psw |= MC;
	if constexpr ((DM & 0xf) == CADD) psw = ((s & d) | (~r & d) | (s & ~r)) & 1 << MSB_N ? psw | MC : psw & ~MC;
	if constexpr ((DM & 0xf) == CSUB) psw = ((s & ~d) | (r & ~d) | (s & r)) & 1 << MSB_N ? psw | MC : psw & ~MC;
	if constexpr ((DM & 0xf) == CLEFT) psw = s & 1 << MSB_N ? psw | MC : psw & ~MC;
	if constexpr ((DM & 0xf) == CRIGHT) psw = s & 1 ? psw | MC : psw & ~MC;
	if constexpr ((DM & 0xf) == CASH) psw = s ? d & 1 << ((s32)s > 0 ? (8 << S) - s : -s - 1) ? psw | MC : psw & ~MC : psw;
	if constexpr ((DM & 0xf) == CMUL) psw = r != (s16)r ? psw | MC : psw & ~MC;
	if constexpr ((DM & 0xf) == CDIV) psw = !s ? psw | MC : psw & ~MC;
	if constexpr ((DM & 0xf000) == NDEF) psw = r & 1 << MSB_N ? psw | MN : psw & ~MN;
	if constexpr ((DM & 0xf0) == V0) psw &= ~MV;
	if constexpr ((DM & 0xf0) == VADD) psw = ((s & d & ~r) | (~s & ~d & r)) & 1 << MSB_N ? psw | MV : psw & ~MV;
	if constexpr ((DM & 0xf0) == VSUB) psw = ((~s & d & ~r) | (s & ~d & r)) & 1 << MSB_N ? psw | MV : psw & ~MV;
	if constexpr ((DM & 0xf0) == VLEFT || (DM & 0xf0) == VRIGHT)
		psw = (psw >> (LN - LC) ^ psw) & MC ? psw | MV : psw & ~MV; // depend on C and N
	if constexpr ((DM & 0xf0) == VASH)
		psw = (d >> MSB_N & 1 ? ~d : d) & ((s32)s <= 0 ? 0 : s >= 8 << S ? -1 : ((1 << s) - 1) << (MSB_N - s)) ? psw | MV : psw & ~MV;
	if constexpr ((DM & 0xf0) == VDIV) psw = !s || r != (s16)r ? psw | MV : psw & ~MV;
	if constexpr ((DM & 0xf00) == ZDEF) psw = !r ? psw | MZ : psw & ~MZ;
	return r;
}

template<int V> float DCJ11::ffp(float f) {
	if constexpr (V) {
		fps &= ~MC;
		FPR v { .f = f };
		fps = (v.i >> 23 & 0xff) >= 0xfe ? fps | MV : fps & ~MV;
	}
	else fps &= ~(MV | MC);
	fps = f == 0.f ? fps | MZ : fps & ~MZ;
	fps = f < 0.f ? fps | MN : fps & ~MN;
	return f;
}

#if DCJ11_TRACE
#include <string>
void DCJ11::StopTrace() {
	TraceBuffer *endp = tracep;
	int i = 0;
	FILE *fo;
	if (!(fo = fopen((std::string(getenv("HOME")) + "/Desktop/trace.txt").c_str(), "w"))) exit(1);
	do {
		if (++tracep >= tracebuf + TRACEMAX) tracep = tracebuf;
		fprintf(fo, "%4d %06o %06o ", i++, tracep->pc,
				isKE(tracep->pc) ? kell_word(tracep->pc) : *(uint16_t *)&(ispace[tracep->pc]));
		fprintf(fo, "%c%c%c%c ",
				tracep->psw & 8 ? 'N' : '-',
				tracep->psw & 4 ? 'Z' : '-',
				tracep->psw & 2 ? 'V' : '-',
				tracep->psw & 1 ? 'C' : '-');
		for (Acs *p = tracep->acs; p < tracep->acs + tracep->index; p++) {
			FPR f;
			switch (p->type) {
				case acsLoad8:
					fprintf(fo, "L %06o %03o ", p->adr, p->data & 0xff);
					break;
				case acsLoad16:
					fprintf(fo, "L %06o %06o ", p->adr, p->data & 0xffff);
					break;
				case acsLoad32:
					fprintf(fo, "L %06o %011o ", p->adr, p->data);
					break;
				case acsStore8:
					fprintf(fo, "S %06o %06o ", p->adr, p->data & 0xff);
					break;
				case acsStore16:
					fprintf(fo, "S %06o %06o ", p->adr, p->data & 0xffff);
					break;
				case acsStore32:
					fprintf(fo, "S %06o %011o ", p->adr, p->data);
					break;
				case acsStoreR:
					fprintf(fo, "%06o->R%d ", p->data, p->adr);
					break;
				case acsStoreF:
					f.i = p->data;
					fprintf(fo, "%e->F%d ", f.f, p->adr);
					break;
			}
		}
		fprintf(fo, "\n");
	} while (tracep != endp);
	fclose(fo);
	fprintf(stderr, "trace dumped.\n");
	exit(1);
}
#endif

void DCJ11::undef(u16 op) {
	fprintf(stderr, "undefined instruction: PC=%05o OP=%05o\n", gpr[7] - 2, op);
#if DCJ11_TRACE
	StopTrace();
#endif
	exit(1);
}
