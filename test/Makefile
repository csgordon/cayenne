.SUFFIXES: .cy
VPATH=example

CAYENNE = env CAYENNEDIR=.. ../cayenne
#CAYENNE = cayenne
CAYENNEFLAGS = -p ..:.
AOUT = ./a.out
OUT = out
DIR = example
TIME = time
OTIMERS = simple factest tauttest stack monad io op primes unify merge congr printf
TESTS = simple factest tauttest stack monad io op primes unify merge congr printf \
	integer nlist coerce vector apply least div poly ts interp \
	subtypes mapT MapCompose comb

# path problems: tinterp tinterp_pe 
# broken: st Rev

TIMEFLAGS = -no-ghc -no-ghcComp

.cy.o:
	$(CAYENNE) $(CAYENNEFLAGS) $<

all:	$(TESTS)
	rm -f $(AOUT) $(OUT)

timing:
	$(TIME) ${MAKE} $(OTIMERS) > /dev/null 2>> TIMING

ntiming:
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) hello.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) factest.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) taut.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) tauttest.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) Stack.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) Queue.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) StackL.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) SQ.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) usestack.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) monad.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) io.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) op.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) primes.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) merge.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) usemerge.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) printf.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) useprintf.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) unify.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) xlogic.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) equiv.cy
	$(CAYENNE) $(TIMEFLAGS) $(CAYENNEFLAGS) congr.cy

simple:
	$(CAYENNE) $(CAYENNEFLAGS) hello.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$hello'
	$(AOUT) > $(OUT)
	cmp simple.out $(OUT)

factest:
	$(CAYENNE) $(CAYENNEFLAGS) factest.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$factest'
	$(AOUT) > $(OUT)
	cmp factest.out $(OUT)

integer:
	$(CAYENNE) $(CAYENNEFLAGS) integer.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$integer'
	$(AOUT) > $(OUT)
	cmp integer.out $(OUT)

tauttest:
	$(CAYENNE) $(CAYENNEFLAGS) taut.cy
	$(CAYENNE) $(CAYENNEFLAGS) tauttest.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$tauttest'
	$(AOUT) > $(OUT)
	cmp tauttest.out $(OUT)

stack:
	$(CAYENNE) $(CAYENNEFLAGS) Stack.cy
	$(CAYENNE) $(CAYENNEFLAGS) Queue.cy
	$(CAYENNE) $(CAYENNEFLAGS) StackL.cy
	$(CAYENNE) $(CAYENNEFLAGS) StackToQueue.cy
	$(CAYENNE) $(CAYENNEFLAGS) usestack.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$usestack'
	$(AOUT) > $(OUT)
	cmp stack.out $(OUT)

monad:
	$(CAYENNE) $(CAYENNEFLAGS) monad.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$monad'
	$(AOUT) > $(OUT)
	cmp monad.out $(OUT)

io:
	$(CAYENNE) $(CAYENNEFLAGS) io.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$io'
	$(AOUT) hello > $(OUT)
	cmp io.out $(OUT)

op:
	$(CAYENNE) $(CAYENNEFLAGS) op.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$op'
	$(AOUT) > $(OUT)
	cmp op.out $(OUT)

primes:
	$(CAYENNE) $(CAYENNEFLAGS) primes.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$primes'
	$(AOUT) > $(OUT)
	cmp primes.out $(OUT)

merge:
	$(CAYENNE) $(CAYENNEFLAGS) merge.cy
	$(CAYENNE) $(CAYENNEFLAGS) usemerge.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$usemerge'
	$(AOUT) > $(OUT)
	cmp merge.out $(OUT)

printf:
	$(CAYENNE) $(CAYENNEFLAGS) printf.cy
	$(CAYENNE) $(CAYENNEFLAGS) useprintf.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$useprintf'
	$(AOUT) > $(OUT)
	cmp printf.out $(OUT)

unify:
	$(CAYENNE) $(CAYENNEFLAGS) unify.cy
# this is a proof, execution is pointless

congr:
	$(CAYENNE) $(CAYENNEFLAGS) xlogic.cy
	$(CAYENNE) $(CAYENNEFLAGS) equiv.cy
	$(CAYENNE) $(CAYENNEFLAGS) congr.cy
# this is a proof, execution is pointless

nlist:
	$(CAYENNE) $(CAYENNEFLAGS) nlist.cy
	$(CAYENNE) $(CAYENNEFLAGS) usenlist.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$usenlist'
	$(AOUT) > $(OUT)
	cmp nlist.out $(OUT)

vector:
	$(CAYENNE) $(CAYENNEFLAGS) vector.cy
	$(CAYENNE) $(CAYENNEFLAGS) usevector.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$usevector'
	$(AOUT) > $(OUT)
	cmp vector.out $(OUT)

coerce:
	$(CAYENNE) $(CAYENNEFLAGS) coerce.cy

apply:
	$(CAYENNE) $(CAYENNEFLAGS) apply.cy
	$(CAYENNE) $(CAYENNEFLAGS) useapply.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$useapply'
	$(AOUT) > $(OUT)
	cmp apply.out $(OUT)

atsyn:
	$(CAYENNE) $(CAYENNEFLAGS) atsyn.cy
	$(CAYENNE) $(CAYENNEFLAGS) useatsyn.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$useatsyn'
	$(AOUT) > $(OUT)
	cmp atsyn.out $(OUT)

least:
	$(CAYENNE) $(CAYENNEFLAGS) misc.cy
	$(CAYENNE) $(CAYENNEFLAGS) Sequence.cy
	$(CAYENNE) $(CAYENNEFLAGS) least.cy

div:
	$(CAYENNE) $(CAYENNEFLAGS) div.cy

poly:
	$(CAYENNE) $(CAYENNEFLAGS) Bifunctor.cy
	$(CAYENNE) $(CAYENNEFLAGS) Tree.cy
	$(CAYENNE) $(CAYENNEFLAGS) Regular.cy
	$(CAYENNE) $(CAYENNEFLAGS) PMap.cy
	$(CAYENNE) $(CAYENNEFLAGS) Base.cy
	$(CAYENNE) $(CAYENNEFLAGS) testBase.cy
	$(CAYENNE) $(CAYENNEFLAGS) usepoly.cy
	$(CAYENNE) $(CAYENNEFLAGS) 'example$$usepoly'
	$(AOUT) > $(OUT)
	cmp poly.out $(OUT)

ts:
	$(CAYENNE) $(CAYENNEFLAGS) ts.cy
	$(CAYENNE) $(CAYENNEFLAGS) usets.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$usets'
	$(AOUT) > $(OUT)
	cmp ts.out $(OUT)

mapT:
	$(CAYENNE) $(CAYENNEFLAGS) mapT.cy
	$(CAYENNE) $(CAYENNEFLAGS) usemapT.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$usemapT'
	$(AOUT) > $(OUT)
	cmp mapT.out $(OUT)

tinterp::
	cd tinterp; ${MAKE} test

tinterp_pe::
	cd tinterp_pe; ${MAKE} test

interp:
	$(CAYENNE) $(CAYENNEFLAGS) xlogic.cy
	$(CAYENNE) $(CAYENNEFLAGS) abssyntype.cy
	$(CAYENNE) $(CAYENNEFLAGS) ienv.cy
	$(CAYENNE) $(CAYENNEFLAGS) abssyn.cy
	$(CAYENNE) $(CAYENNEFLAGS) tcheck.cy
	$(CAYENNE) $(CAYENNEFLAGS) iprint.cy
	$(CAYENNE) $(CAYENNEFLAGS) eval.cy
	$(CAYENNE) $(CAYENNEFLAGS) interp.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$interp$$interp'
	$(AOUT) > $(OUT)
	cmp interp.out $(OUT)

dinterp:
	$(CAYENNE) $(CAYENNEFLAGS) xlogic.cy
	$(CAYENNE) $(CAYENNEFLAGS) abssyntype.cy
	$(CAYENNE) $(CAYENNEFLAGS) ienv.cy
	$(CAYENNE) $(CAYENNEFLAGS) dabssyn.cy
	$(CAYENNE) $(CAYENNEFLAGS) deval.cy
#	$(AOUT) > $(OUT)
#	cmp interp.out $(OUT)

dimensions:
	$(CAYENNE) $(CAYENNEFLAGS) dimensions.cy
	$(CAYENNE) $(CAYENNEFLAGS) usedimensions.cy

lexp:
	$(CAYENNE) $(CAYENNEFLAGS) xlogic.cy
	$(CAYENNE) $(CAYENNEFLAGS) lexp.cy

st:
	$(CAYENNE) $(CAYENNEFLAGS) useST.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$useST'
	$(AOUT) > $(OUT)
	cmp st.out $(OUT)

subtypes:
	$(CAYENNE) $(CAYENNEFLAGS) subtypes.cy

Rev:
	$(CAYENNE) $(CAYENNEFLAGS) -steps 3000000 Rev.cy

MapCompose:
	$(CAYENNE) $(CAYENNEFLAGS) -steps 3000000 MapCompose.cy

comb:
	$(CAYENNE) $(CAYENNEFLAGS) comb.cy
	$(CAYENNE) $(CAYENNEFLAGS) usecomb.cy
	$(CAYENNE) $(CAYENNEFLAGS) '$(DIR)$$usecomb'
	$(AOUT) > $(OUT)
	cmp comb.out $(OUT)

clean:
	rm -rf $(DIR) $(AOUT) $(OUT) *.hs *.hi *.o
	cd tinterp; ${MAKE} clean
	cd tinterp_pe; ${MAKE} clean

