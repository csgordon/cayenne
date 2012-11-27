OBJS =	err1a.x err1b.x err2a.x err3a.x err4a.x err5a.x err7a.x err8a.x err9a.x err10a.x \
	err10b.x err13a.x err13b.x err14a.x err15a.x err16a.x err17a.x err18a.x err19a.x err20a.x \
	err21a.x err22a.x err23a.x err24a.x err24b.x err25a.x \
	warn1a.x warn2a.x

CAYENNE = ../cayenne
CAYENNEFLAGS = -p ..:.

.SUFFIXES :
.SUFFIXES : .x .ci .cy

.cy.x :
	${CAYENNE} ${CAYENNEFLAGS} $<  > out 2>&1 || exit 0
	cmp out $*.out

all:	${OBJS}

clean:
	rm -f out *.o *.m *.x *.ci
