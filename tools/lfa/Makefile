#
#-----------------------------------------------
# Makefile area.
#-----------------------------------------------
#
EXEf=lfacop lfac lfafreq lfacre lfalaf lfaminm lfareu lfatest lfa_demo lfa2dd
EXEF=lfadiffart lfadiffrel lfadiff lfa2lfa lfamoy lfa_autodocumentation

all: liblfa.a lfa2lfp lfp2lfa $(EXEf) $(EXEF)

exe: lfa2lfp lfp2lfa cllangu $(EXEf) $(EXEF)

lfa.o: lfa.F90 lfatail.h lfayom.h
	$(FC) -c lfa.F90

liblfa.a: lfa.o
	$(BIBC) liblfa.a lfa.o

lfa2lfp: lfa2lfp.F90 liblfa.a lfp.F90 lfpyom.h 
	$(FC) -o $@ lfa2lfp.F90 lfp.F90 liblfa.a
	rm -f $@.o

lfp2lfa: lfp2lfa.F90 liblfa.a lfp.F90 lfpyom.h 
	$(FC) -o $@ lfp2lfa.F90 lfp.F90 liblfa.a
	rm -f $@.o

cllangu: cllangu.F90 cllang.F90
	$(FC) -o $@ cllangu.F90
	rm -f $@.o

$(EXEf): $(%.F90) liblfa.a
	$(FC) -o $@ $@.F90 liblfa.a
	rm -f $@.o

$(EXEF): $(%.F90) liblfa.a
	$(FC) -o $@ $@.F90 liblfa.a
	rm -f $@.o

clean: 
	rm -f *.o *.a *.mod lfacop lfac lfafreq lfacre lfalaf lfaminm lfareu lfatest lfa_demo lfa2dd lfadiffart lfadiffrel lfadiff lfa2lfp lfp2lfa lfa2lfa cllangu lfamoy lfa_autodocumentation *.lf95.exe
