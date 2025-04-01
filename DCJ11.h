// DCJ11
// Copyright 2025 © Yasuo Kuwahara
// MIT License

#include "defines.h"	// emulator

#define DCJ11_TRACE		0

#if DCJ11_TRACE
#define DCJ11_TRACE_LOG(adr, data, type) \
	if (tracep->index < ACSMAX) tracep->acs[tracep->index++] = { adr, data, type }
#else
#define DCJ11_TRACE_LOG(adr, data, type)
#endif

class DCJ11 {
	friend class Insn;
	using s8 = int8_t;
	using u8 = uint8_t;
	using s16 = int16_t;
	using u16 = uint16_t;
	using s32 = int32_t;
	using u32 = uint32_t;
	enum {
		LC, LV, LZ, LN
	};
	enum {
		MC = 1 << LC, MV = 1 << LV, MZ = 1 << LZ, MN = 1 << LN
	};
	enum {
		FDEF = 1, F0, F1, FADD, FSUB, FLEFT, FRIGHT, FASH, FMUL, FDIV
	};
#define F(flag, type)	flag##type = F##type << (L##flag << 2)
	enum {
		F(C, 0), F(C, 1), F(C, ADD), F(C, SUB), F(C, LEFT), F(C, RIGHT), F(C, ASH), F(C, MUL), F(C, DIV),
		F(V, 0), F(V, ADD), F(V, SUB), F(V, LEFT), F(V, RIGHT), F(V, ASH), F(V, DIV),
		F(Z, DEF),
		F(N, DEF),
	};
#undef F
	union FPR { u32 i; float f; };
public:
	DCJ11();
	void Reset(u16 sp, u16 pc);
	int Execute(int n);
#if DCJ11_TRACE
	void StopTrace();
#endif
private:
	void stR(u16 num, u16 data) {
		gpr[num] = data;
		DCJ11_TRACE_LOG(num, data, acsStoreR);
	}
	void stI(u16 num, u32 data) {
		fpr[num].i = data;
		DCJ11_TRACE_LOG(num, data, acsStoreF);
	}
	void stF(u16 num, float data) {
		fpr[num].f = data;
		DCJ11_TRACE_LOG(num, fpr[num].i, acsStoreF);
	}
	// customized access -- start
	bool isKE(u16 adr) { return Binary < IS_V3 && (adr & 0177760) == 0177300; }
	u8 ld1(u16 adr) {
#ifdef EMUV1
		u16 data = isKE(adr) ? kell_byte(adr) : dspace[adr];
#else
		u16 data = dspace[adr];
#endif
		DCJ11_TRACE_LOG(adr, data, acsLoad8);
		return data;
	}
	u16 ld2(u16 adr) {
#ifdef EMUV1
		u16 data = isKE(adr) ? kell_word(adr) : (u16 &)dspace[adr];
#else
		u16 data = (u16 &)dspace[adr];
#endif
		DCJ11_TRACE_LOG(adr, data, acsLoad16);
		return data;
	}
	u32 ld4(u16 adr) {
		u32 data = (u32 &)dspace[adr];
		DCJ11_TRACE_LOG(adr, data << 16 | data >> 16, acsLoad32);
		return data << 16 | data >> 16;
	}
	u8 ld1i(u16 adr) {
		u16 data = ispace[adr];
		DCJ11_TRACE_LOG(adr, data, acsLoad8);
		return data;
	}
	u16 ld2i(u16 adr) {
		u16 data = (u16 &)ispace[adr];
		DCJ11_TRACE_LOG(adr, data, acsLoad16);
		return data;
	}
	void st1(u16 adr, u8 data) {
#ifdef EMUV1
		if (isKE(adr)) kesl_byte(adr, data);
		else dspace[adr] = data;
#else
		dspace[adr] = data;
#endif
		DCJ11_TRACE_LOG(adr, data, acsStore8);
	}
	void st2(u16 adr, u16 data) {
#ifdef EMUV1
		if (isKE(adr)) kesl_word(adr, data);
		else (u16 &)dspace[adr] = data;
#else
		(u16 &)dspace[adr] = data;
#endif
		DCJ11_TRACE_LOG(adr, data, acsStore16);
	}
	void st4(u16 adr, u32 data) {
		(u32 &)dspace[adr] = data << 16 | data >> 16;
		DCJ11_TRACE_LOG(adr, data, acsStore32);
	}
	u16 fetch2() {
		u16 data = (u16 &)ispace[gpr[7]];
		gpr[7] += 2;
		return data;
	}
	// customized access -- end
	u16 gpr[8];
	FPR fpr[4];
	u16 psw, fps;
	bool islong, isdouble;
#if DCJ11_TRACE
	static constexpr int TRACEMAX = 10000;
	static constexpr int ACSMAX = 4;
	enum {
		acsStoreR = 1, acsStoreI, acsStoreF,
		acsStore8, acsStore16, acsStore32, acsLoad8, acsLoad16, acsLoad32
	};
	struct Acs {
		u16 adr;
		u32 data;
		u8 type;
	};
	struct TraceBuffer {
		u16 pc, index, psw;
		Acs acs[ACSMAX];
	};
	TraceBuffer tracebuf[TRACEMAX];
	TraceBuffer *tracep;
#endif
	// flags
	template<int DM, int S = 1> u16 fset(u32 r, u32 s = 0, u32 d = 0);
	template<int S = 1> u16 fmov(u16 r) { return fset<NDEF | ZDEF | V0, S>(r); }
	template<int S = 1> u16 ftst(u16 r) { return fset<NDEF | ZDEF | V0 | C0, S>(r); }
	template<int S = 1> u16 fcom(u16 r) { return fset<NDEF | ZDEF | V0 | C1, S>(r); }
	template<int S> u16 fleft(u16 r, u16 s) { return fset<NDEF | ZDEF | VLEFT | CLEFT, S>(r, s); }
	template<int S> u16 fright(u16 r, u16 s) { return fset<NDEF | ZDEF | VRIGHT | CRIGHT, S>(r, s); }
	template<int S = 1> u16 fash(u32 r, u32 s, u32 d) { return fset<NDEF | ZDEF | VASH | CASH, S>(r, s, d); }
	void fmul(u32 r) { fset<NDEF | ZDEF | V0 | CMUL, 2>(r); }
	u16 fdiv(u32 r, u32 s, u32 d) { return fset<NDEF | ZDEF | VDIV | CDIV>(r, s, d); }
	template<int S> u16 finc(u16 r, u16 s) { return fset<NDEF | ZDEF | VADD, S>(r, 1, s); }
	template<int S> u16 fdec(u16 r, u16 s) { return fset<NDEF | ZDEF | VSUB, S>(r, 1, s); }
	template<int S = 1> u16 fadd(u16 r, u16 s, u16 d) { return fset<NDEF | ZDEF | VADD | CADD, S>(r, s, d); }
	template<int S = 1> u16 fsub(u16 r, u16 s, u16 d) { return fset<NDEF | ZDEF | VSUB | CSUB, S>(r, s, d); }
	//
	template<int RW, int M, int S = 1, typename F> u16 ea(int reg, F func);
	// integer instructions 6-23(list)
	template<int M, int S> void clr(u16 op) { ea<2, M, S>(op, [&] { return ftst<S>(0); }); } // 6-26
	template<int M, int S> void com(u16 op) { ea<3, M, S>(op, [&](u16 v) { return fcom<S>(~v); }); } // 6-26
	template<int M, int S> void inc(u16 op) { ea<3, M, S>(op, [&](u16 v) { return finc<S>(v + 1, v); }); } // 6-27
	template<int M, int S> void dec(u16 op) { ea<3, M, S>(op, [&](u16 v) { return fdec<S>(v - 1, v); }); } // 6-27
	template<int M, int S> void tst(u16 op) { ea<1, M, S>(op, [&](u16 v) { ftst<S>(v); }); } // 6-28
	template<int M, int S> void neg(u16 op) { ea<3, M, S>(op, [&](u16 v) { return fsub<S>(-v, v, 0); }); } // 6-28
	template<int M, int S> void asr(u16 op) { ea<3, M, S>(op, [&](u16 v) { return fright<S>(v >> 1, v); }); } // 6-30
	template<int M, int S> void asl(u16 op) { ea<3, M, S>(op, [&](u16 v) { return fleft<S>(v << 1, v); }); } // 6-30
	template<int M, int S> void ror(u16 op) { ea<3, M, S>(op, [&](u16 v) { // 6-31
		return fright<S>(v >> 1 | (psw & 1) << ((8 << S) - 1), v); });
	}
	template<int M, int S> void rol(u16 op) { ea<3, M, S>(op, [&](u16 v) { return fleft<S>(v << 1 | (psw & 1), v); }); } // 6-32
	template<int M, int S> void swab(u16 op) { ea<3, M, S>(op, [&](u16 v) { return v << 8 | ftst<0>(v >> 8); }); } // 6-33
	template<int M, int S> void adc(u16 op) { ea<3, M, S>(op, [&](u16 v) { return fadd<S>(v + (psw & 1), psw & 1, v); }); } // 6-34
	template<int M, int S> void sbc(u16 op) { ea<3, M, S>(op, [&](u16 v) { return fsub<S>(v - (psw & 1), psw & 1, v); }); } // 6-35
	template<int M> void sxt(u16 op) { ea<2, M>(op, [&] { return fmov(psw & MN ? -1 : 0); }); } // 6-35
	template<int M> void mfps(u16 op) { ea<2, M, 0>(op, [&] { return fmov<0>(psw); }); } // 6-36
	template<int M> void mtps(u16 op) { ea<1, M, 0>(op, [&](u8 v) { psw = (psw & 0xfff0) | (v & 0xf); }); } // 6-36
	template<int MS, int MD, int S> void mov(u16 op) { // 6-37
		ea<1, MS, S>(op >> 6, [&](u16 v) {
			if constexpr (!MD && !S) stR(op & 7, fmov<S>((s8)v));
			else ea<2, MD, S>(op, [&] { return fmov<S>(v); });
		});
	}
	template<int MS, int MD, int S> void cmp(u16 op) { // 6-38
		ea<1, MS, S>(op >> 6, [&](u16 v) { return ea<1, MD, S>(op, [&](u16 d) { fsub<S>(v - d, d, v); }); });
	}
	template<int MS, int MD> void add(u16 op) { // 6-39
		ea<1, MS>(op >> 6, [&](u16 v) { return ea<3, MD>(op, [&](u16 d) { return fadd(d + v, v, d); }); });
	}
	template<int MS, int MD> void sub(u16 op) { // 6-39
		ea<1, MS>(op >> 6, [&](u16 v) { return ea<3, MD>(op, [&](u16 d) { return fsub(d - v, v, d); }); });
	}
	template<int M> void ash(u16 op); // 6-40
	template<int M> void ashc(u16 op); // 6-40
	template<int M> void mul(u16 op) { // 6-41
		ea<1, M>(op, [&](s16 v) {
			s32 r = (s16)gpr[op >> 6 & 7] * v; stR(op >> 6 & 7, r >> 16); stR((op >> 6 & 7) | 1, r); fmul(r);
		});
	}
	template<int M> void div(u16 op); // 6-42
	template<int MS, int MD, int S> void bit(u16 op) { // 6-42
		ea<1, MS, S>(op >> 6, [&](u16 v) { return ea<1, MD, S>(op, [&](u16 d) { fmov<S>(v & d); }); });
	}
	template<int MS, int MD, int S> void bic(u16 op) { // 6-43
		ea<1, MS, S>(op >> 6, [&](u16 v) { return ea<3, MD, S>(op, [&](u16 d) { return fmov<S>(~v & d); }); });
	}
	template<int MS, int MD, int S> void bis(u16 op) { // 6-43
		ea<1, MS, S>(op >> 6, [&](u16 v) { return ea<3, MD, S>(op, [&](u16 d) { return fmov<S>(v | d); }); });
	}
	template<int M> void _xor(u16 op) { ea<3, M>(op, [&](u16 v) { return fmov(gpr[op >> 6 & 7] ^ v); }); } // 6-44
	void br(u16 op) { gpr[7] += (s8)(op & 0xff) << 1; } // 6-45
	void bne(u16 op) { if (!(psw & MZ)) gpr[7] += (s8)op << 1; } // 6-46
	void beq(u16 op) { if (psw & MZ) gpr[7] += (s8)op << 1; } // 6-47
	void bpl(u16 op) { if (!(psw & MN)) gpr[7] += (s8)op << 1; } // 6-47
	void bmi(u16 op) { if (psw & MN) gpr[7] += (s8)op << 1; } // 6-47
	void bvc(u16 op) { if (!(psw & MV)) gpr[7] += (s8)op << 1; } // 6-48
	void bvs(u16 op) { if (psw & MV) gpr[7] += (s8)op << 1; } // 6-48
	void bcc(u16 op) { if (!(psw & MC)) gpr[7] += (s8)op << 1; } // 6-48
	void bcs(u16 op) { if (psw & MC) gpr[7] += (s8)op << 1; } // 6-48
	void bge(u16 op) { if (!((psw >> (LN - LV) ^ psw) & MV)) gpr[7] += (s8)op << 1; } // 6-50
	void blt(u16 op) { if ((psw >> (LN - LV) ^ psw) & MV) gpr[7] += (s8)op << 1; } // 6-50
	void bgt(u16 op) { if (!((psw >> (LZ - LV) | (psw >> (LN - LV) ^ psw)) & MV)) gpr[7] += (s8)op << 1; } // 6-51
	void ble(u16 op) { if ((psw >> (LZ - LV) | (psw >> (LN - LV) ^ psw)) & MV) gpr[7] += (s8)op << 1; } // 6-51
	void bhi(u16 op) { if (!((psw >> (LZ - LC) | psw) & MC)) gpr[7] += (s8)op << 1; } // 6-51
	void blos(u16 op) { if ((psw >> (LZ - LC) | psw) & MC) gpr[7] += (s8)op << 1; } // 6-51
	void bhis(u16 op) { if (!(psw & MC)) gpr[7] += (s8)op << 1; } // 6-52
	template<int M> void jmp(u16 op) { gpr[7] = ea<0, M>(op, []{}); } // 6-52
	template<int M> void jsr(u16 op) { // 6-53
		u16 a = ea<0, M>(op, []{}); st2(gpr[6] -= 2, gpr[op >> 6 & 7]); gpr[op >> 6 & 7] = gpr[7]; gpr[7] = a;
	}
	void rts(u16 op) { gpr[7] = gpr[op & 7]; gpr[op & 7] = ld2(gpr[6]); gpr[6] += 2; } // 6-55
	void sob(u16 op) { if (--gpr[op >> 6 & 7]) gpr[7] -= (op & 077) << 1; } // 6-56
	void emttrap(u16 op); // 6-58
	void cco(u16 op) { psw = op & 0x10 ? psw | (op & 0xf) : psw & ~(op & 0xf); } // 6-66
	// float instructions
	enum { MFS = 0x80000000U, MFE = 0x7f800000, EOFS = 0x01000000 };
	template<int V = 0> float ffp(float f);
	template<int M> float _ldf(u16 op);
	template<int M> void absf(u16 op) { ea<3, M, 3>(op, [&](u32 v) { return v & ~MFS; }); ffp(_ldf<M>(op)); } // 7-12
	template<int M> void addf(u16 op) { stF(op >> 6 & 3, ffp<1>(fpr[op >> 6 & 3].f + _ldf<M>(op))); } // 7-13
	void cfcc(u16) { psw = (psw & 0xfff0) | (fps & 0xf); } // 7-14
	template<int M> void clrf(u16 op) { ea<2, M, 3>(op, [] { return 0; }); ffp(0.f); } // 7-14
	template<int M> void cmpf(u16 op) { ffp(_ldf<M>(op) - fpr[op >> 6 & 3].f); } // 7-15
	template<int M> void divf(u16 op) { stF(op >> 6 & 3, ffp<1>(fpr[op >> 6 & 3].f / _ldf<M>(op))); } // 7-15
	template<int M> void ldc(u16 op) { // ldcif/ldcid/ldclf/ldcld 7-17
		ea<1, M, 2>(op, [&](s32 v) { stF(op >> 6 & 3, ffp(islong ? v : v << 16 >> 16)); });
	}
	template<int M> void ldexp(u16 op) { // 7-18
		ea<1, M>(op, [&](u16 v) {
			stI(op >> 6 & 3, (fpr[op >> 6 & 3].i & ~MFE) | ((v + 126) << 23 & MFE)); ffp(fpr[op >> 6 & 3].f); fps |= v & 0x80 ? MV : 0;
		});
	}
	template<int M> void ldf(u16 op) { stF(op >> 6 & 3, ffp(_ldf<M>(op))); } // 7-19
	template<int M> void ldfps(u16 op) { ea<1, M>(op, [&](u16 v) { fps = v; islong = v >> 6 & 1; isdouble = v >> 7 & 1; }); } // 7-20
	template<int M> void modf(u16 op); // 7-20
	template<int M> void mulf(u16 op) { stF(op >> 6 & 3, ffp<1>(fpr[op >> 6 & 3].f * _ldf<M>(op))); } // 7-23
	template<int M> void negf(u16 op) { ea<3, M, 3>(op, [&](u32 v) { return v ^ MFS; }); ffp(_ldf<M>(op)); } // 7-24
	void setdf(u16 op) { isdouble = op >> 3 & 1; } // setd/setf 7-24
	void setil(u16 op) { islong = op >> 3 & 1; } // seti/setl 7-25
	template<int M> void stcfi(u16 op); // stcfi/stcfl/stcdi/stcdl 7-26
	template<int M> void stexp(u16 op) { // 7-28
		ea<2, M>(op, [&] { s16 t = fpr[op >> 6 & 3].i >> 23 & 0xff; ffp(t = t ? t - 126 : -128); cfcc(0); return t; });
	}
	template<int M> void stf(u16 op); // 7-28
	template<int M> void stfps(u16 op) { ea<2, M>(op, [&] { return fps & 0xcfef; }); } // 7-29
	template<int M> void subf(u16 op) { stF(op >> 6 & 3, ffp<1>(fpr[op >> 6 & 3].f - _ldf<M>(op))); } // 7-29
	template<int M> void tstf(u16 op) { ffp(_ldf<M>(op)); } // 7-31
	void undef(u16 op);
};
