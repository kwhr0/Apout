# Makefile for Apout PDP-11 application emulator
#
# $Revision: 1.31 $
# $Date: 2008/05/19 13:42:39 $
#

CFLAGS = -O3
CXXFLAGS = --std=c++17 -O3

# Install destinations
PREFIX?=/usr/local
MANDIR=$(PREFIX)/share/man/man1
BINDIR=$(PREFIX)/bin

OBJS =	DCJ11.o Wapper.o aout.o bsd_ioctl.o bsd_signal.o bsdtrap.o \
	cpu.o ke11a.o magic.o main.o v1trap.o v7trap.o

all: apout

apout: $(OBJS)
	$(CXX) $(LDFLAGS) $(OBJS) -o $@

install: apout apout.1
	cp apout $(BINDIR)
	chmod 755 $(BINDIR)/apout
	cp apout.1 $(MANDIR)
	chmod 644 $(MANDIR)/apout.1

clean:
	rm -f apout $(OBJS)

# Dependencies for object files
DCJ11.o: DCJ11.cpp DCJ11.h defines.h Makefile
Wrapper.o: Wrapper.cpp DCJ11.h CompareProcess.h Makefile
aout.o: aout.c defines.h aout.h Makefile
bsd_ioctl.o: bsd_ioctl.c defines.h Makefile
bsd_signal.o: bsd_signal.c defines.h bsdtrap.h Makefile
bsdtrap.o: bsdtrap.c bsdtrap.h defines.h Makefile
cpu.o: cpu.c defines.h Makefile
ke11a.o: ke11a.c defines.h Makefile
magic.o: magic.c defines.h Makefile
main.o: main.c defines.h Makefile
v1trap.o: v1trap.c v1trap.h defines.h Makefile
v7trap.o: v7trap.c v7trap.h defines.h Makefile
