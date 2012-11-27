RM=rm

all:	paper.ps interp.ps eqproof.ps

clean:
	${RM} -f *.dvi *.blg *.bbl *.log *.aux paper.ps interp.ps

paper.dvi:	paper.tex paper.bbl
	latex paper.tex; latex paper.tex

interp.dvi:	interp.tex interp.bbl
	latex interp.tex; latex interp.tex

using.dvi:	using.tex
	latex using.tex; latex using.tex

eqproof.dvi:	eqproof.tex
	latex eqproof.tex; latex eqproof.tex

bibtex:
	-latex paper.tex
	-bibtex paper
	latex paper.tex; latex paper.tex

paper.ps:	paper.dvi
	dvips paper.dvi -o paper.ps

interp.ps:	interp.dvi
	dvips interp.dvi -o interp.ps

eqproof.ps:	eqproof.dvi
	dvips eqproof.dvi -o eqproof.ps

using.ps:	using.dvi
	dvips using.dvi -o using.ps

slides.dvi:	slides.tex
	latex slides.tex

installdist: $(DIST)
	cp $(DIST) $(HOME)/.ftp/
