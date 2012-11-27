HTMLDIR = /users/cs/augustss/.www/cayenne

install: system.html
	mkdir -p ${HTMLDIR}/Greek
	mkdir -p ${HTMLDIR}/interp
	cp *.* ${HTMLDIR}
	cp Greek/*.gif ${HTMLDIR}/Greek
	cp interp/*.cy ${HTMLDIR}/interp
	chmod 644 ${HTMLDIR}/Greek/* ${HTMLDIR}/interp/*

system.html: ../System/*.ci sys.sed system-1 system-2 Makefile
	cp system-1 sys.html
	for f in ../System/*.ci; do\
	    echo "<h2>"`basename $$f .ci`"</h2>" >>sys.html; \
	    sed -n '/^--@ /s///p' `echo $$f | sed 's/ci$$/cy/` >>sys.html; \
	    echo "<pre>" >>sys.html; \
	    if [ "$$f" = "../System/Bool.ci" ]; then \
		cat bool.sig >> sys.html; \
	    else \
	        sed -f sys.sed $$f >>sys.html; \
	    fi; \
	    echo "</pre>" >>sys.html; \
	done
	cat system-2 >> sys.html
	sed "s/Last modified: .*/Last modified: `date`/" sys.html > system.html
