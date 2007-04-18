# Copyright 2003-2007 The Savonet team

PROGNAME = ocaml-irc
DISTFILES = CHANGES COPYING Makefile README \
            src/OCamlMakefile src/Makefile \
            src/META src/*.ml src/*.mli \
            doc/html

.PHONY: all install uninstall update opt byte doc clean distclean dist

all:
	make -C src

opt byte update install uninstall:
	make -C src $@

doc:
	make -C src htdoc
	mkdir -p doc
	rm -rf doc/html
	mv src/doc/mad/html doc
	rm -rf src/doc

clean:
	-make -C src clean
#	-make -C examples clean

distclean: clean
	rm -rf autom4te.cache config.log config.status src/META src/Makefile
	rm -rf doc
	-make -C examples distclean

dist: doc
	VERSION="$(shell grep 'AC_INIT' configure.ac)"; \
		VERSION=`echo "$$VERSION" | sed -e 's/AC_INIT([^,]*, \([^,]*\), .*)/\1/'`; \
		mkdir $(PROGNAME)-$$VERSION; \
		cp -r --parents $(DISTFILES) $(PROGNAME)-$$VERSION; \
		tar zcvf $(PROGNAME)-$$VERSION.tar.gz $(PROGNAME)-$$VERSION; \
		rm -rf $(PROGNAME)-$$VERSION
