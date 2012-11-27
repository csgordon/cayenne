.SUFFIXES: .cy

CAYENNEROOT=../..

CAYENNE = env CAYENNEDIR=$(CAYENNEROOT) $(CAYENNEROOT)/cayenne
CAYENNEFLAGS = -p $(CAYENNEROOT):..:. -d ..
VPATH=../example/tinterp_pe

.cy.o:
	$(CAYENNE) $(CAYENNEFLAGS) $<

all: interp

interp: abssyn.o parser.o parseLam.o parseIf.o parseFix.o tparser.o aux.o prelude.o interp.o eval.o lex.o iprint.o
	$(CAYENNE) $(CAYENNEFLAGS) -o interp 'example$$tinterp_pe$$interp'

../example/parselib.o:
	cd ..; make parselib.o

../example/logic.o:
	cd ..; make logic.o

abssyn.o: ../example/logic.o

parser.o: abssyn.o aux.o lex.o tparser.o parseLam.o parseIf.o parseFix.o ../example/parselib.o

parseLam.o: abssyn.o aux.o lex.o tparser.o ../example/parselib.o

parseIf.o: abssyn.o aux.o lex.o tparser.o ../example/parselib.o

parseFix.o: abssyn.o aux.o lex.o tparser.o ../example/parselib.o

tparser.o: abssyn.o lex.o ../example/parselib.o

aux.o: abssyn.o

prelude.o: abssyn.o aux.o

interp.o: parser.o tparser.o abssyn.o prelude.o eval.o

eval.o: abssyn.o iprint.o

lex.o:

iprint: abssyn.o

clean:
	rm -rf ../example/tinterp_pe
	rm -f interp

test: interp
	./interp "(fix f :: (Int -> Int) -> \ x::Int -> if x <= 1 then 1 else f (x-1) + f (x-2)) 20" | cmp interp.out -
