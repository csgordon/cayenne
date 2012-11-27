#
CAYENNEDIR = /usr/local/lib/cayenne
BINDIR = /usr/local/bin
#
RM = rm
DIST = cayenne-`sed -n -e 's/^version.*\(20..-..-..\).*/\1/p' cayenne.hs`.tar.gz

GHC = ghc

cayenne:	*.hs Libs/*.hs
	${GHC} -iLibs --make cayenne.hs -o cayenne

.PHONY:	all
all:	cayenne
	cd System; ${MAKE}

.PHONY:	install
install:
	cp cayenne ${BINDIR}
	cd System; ${MAKE} CAYENNEDIR=${CAYENNEDIR} install

.PHONY:	installdoc
installdoc:
	cd doc; ${MAKE} paper.ps interp.ps
	cp doc/paper.ps html
	cp doc/interp.ps html
	cd html; ${MAKE} install

.PHONY:	clean
clean:
	${RM} -f *.o *.hi cayenne Libs/*.o Libs/*.hi
	cd System; ${MAKE} clean
	cd test; ${MAKE} clean
	cd doc; ${MAKE} clean

.PHONY:	newversion
newversion:
	mv cayenne.hs cayenne.hs.old
	sed "s/20..-..-../`date +%Y-%m-%d`/" cayenne.hs.old > cayenne.hs

dist:	*.hs System/*.cy System/*.m
	-@rm -rf Cayenne
	cvs export -D now Cayenne
	tar cf - Cayenne | gzip > $(DIST)
	rm -rf Cayenne

.PHONY: installdist
installdist: dist
	cp $(DIST) $(HOME)/.ftp/

.PHONY:	test
test:
	cd System; make
	cd test; make
