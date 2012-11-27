CAYENNEDIR = /usr/local/lib/cayenne
OBJS = PTS.o Bool.ci Char.ci CharType.ci Double.ci Int.ci List.ci String.ci Error.ci \
       Trace.ci Unit.ci IO.ci Monad.ci MonadUtil.ci Maybe.ci Either.ci Tuples.ci HO.ci \
       Integer.ci Void.ci Nat.ci Num.ci StringUtil.ci Id.ci Logic.ci Prelude.ci
# ST.o 
#  Double.o ### compiler problems.
POBJS = Class.o
CAYENNE = ../cayenne
CAYENNEFLAGS = -d .. -p .. -i .
#GHC = /usr/pkg/bin/ghc
GHC = ghc
GHCFLAGS = -fglasgow-exts
RM = rm

#	${CAYENNE} ${CAYENNEFLAGS} -no-lml $*.cy

.SUFFIXES :
.SUFFIXES : .hs .o .ci .cy

.hs.o :
	${GHC} -c ${GHCFLAGS} $<

.cy.ci :
	${CAYENNE} ${CAYENNEFLAGS} $<

all:	${OBJS}
	cp PTS.hi ..

pall:	${POBJS}

# A hack to get the interface file for Char.
Char.cb:	CharXXX.cy
	${CAYENNE} -no-ghc ${CAYENNEFLAGS} CharXXX.cy

Bool.ci:	String.ci

Char.ci:	Char.cb CharType.ci Int.ci List.ci String.ci Bool.ci

CharType.ci:

Double.ci:	String.ci Bool.ci

Either.ci:	String.ci List.ci

Error.ci:	String.ci

HO.ci:		Tuples.ci

Id.ci:

IO.ci:		String.ci Unit.ci Int.ci Monad.ci List.ci

Int.ci:		String.ci Bool.ci

Integer.ci:	String.ci Bool.ci

List.ci:	Bool.ci Int.ci Integer.ci Monad.ci Error.ci String.ci

Logic.ci:	Bool.ci

Maybe.ci:	Error.ci Monad.ci String.ci List.ci

Monad.ci:

MonadUtil.ci:	Monad.ci HO.ci

Nat.ci:		Integer.ci Int.ci

Num.ci:		Nat.ci Int.ci

Prelude.ci:	Unit.ci String.ci List.ci Bool.ci Char.ci Error.ci IO.ci Tuples.ci Either.ci HO.ci Int.ci

String.ci:	CharType.ci

StringUtil.ci:	Bool.ci Char.ci List.ci String.ci

Trace.ci:	String.ci

Tuples.ci:	String.ci List.ci

Unit.ci:	String.ci

Void.ci:

PTS.o:	PTS.hsr
	cp PTS.hsr PTS.hs
	${GHC} -c ${GHCFLAGS} PTS.hs

ST.o: Tuples.o Bool.o Unit.o HO.o Monad.o Int.o ref.o

ref.o: ref.m
	$(LMLC) -c -Z ref.m
	sed -e 's/^import _/import /' ref.t > _ref.t
	mv _ref.t ref.t

clean:
	${RM} -f *.ci *.cb *.o PTS.hi ../PTS.hi lml_IO.t ref.t *.hi *.hs

install: all
	mkdir -p ${CAYENNEDIR}/System
	cp M*.hi *.o *.ci *.cb ${CAYENNEDIR}/System
	cp PTS.hi PTS.o ${CAYENNEDIR}
	cp -r ../INCLUDE ${CAYENNEDIR}
