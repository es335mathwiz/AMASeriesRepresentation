# This makefile can be used to build all or some of the sample
# programs.  To build all of them, use the command
# 'make all'.  To build one, say addtwo, use the command
# 'make addtwo'.

# Portions of this makefile require the use of GNU make.
# see http://www.gnu.org/software/make for more information.

VERSION=10.0
WSLINKDIR = $(shell pwd)/..
SYS = Linux-x86-64
CADDSDIR = ${WSLINKDIR}/CompilerAdditions
EXTRA_CFLAGS=-m64

INCDIR = ${CADDSDIR}
LIBDIR = ${CADDSDIR}

WSPREP = ${CADDSDIR}/wsprep
RM = rm

BINARIES = addtwo bitops counter factor factor2 factor3 quotient reverse sumalist

all : $(BINARIES)

addtwo : addtwotm.o addtwo.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} addtwotm.o addtwo.o -L${LIBDIR} -lWSTP64i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

bitops : bitopstm.o bitops.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} bitopstm.o bitops.o -L${LIBDIR} -lWSTP64i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

counter : countertm.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} countertm.o -L${LIBDIR} -lWSTP64i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

factor : factor.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} factor.o -L${LIBDIR} -lWSTP64i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

factor2 : factor2.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} factor2.o -L${LIBDIR} -lWSTP64i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

factor3 : factor3.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} factor3.o -L${LIBDIR} -lWSTP64i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

quotient : quotient.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} quotient.o -L${LIBDIR} -lWSTP64i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

reverse : reversetm.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} reversetm.o -L${LIBDIR} -lWSTP64i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

sumalist : sumalisttm.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} sumalisttm.o -L${LIBDIR} -lWSTP64i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

%.o : %.cxx
	${CXX} -c ${EXTRA_CFLAGS} -I${INCDIR} $<

addtwotm.cxx : addtwo.tm
	${WSPREP} $? -o $@

bitopstm.cxx : bitops.tm
	${WSPREP} $? -o $@

countertm.cxx : counter.tm
	${WSPREP} $? -o $@

reversetm.cxx : reverse.tm
	${WSPREP} $? -o $@

sumalisttm.cxx : sumalist.tm
	${WSPREP} $? -o $@

clean :
	@ ${RM} -rf *.o *tm.cxx $(BINARIES)
