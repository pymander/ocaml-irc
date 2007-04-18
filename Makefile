# Copyright 2003-2007 The Savonet team

PROGNAME = ocaml-irc
VERSION = 0.1.0
DISTFILES = CHANGES COPYING Makefile README \
            src/OCamlMakefile src/Makefile \
            src/META.in src/*.ml src/*.mli \
            doc/html

.PHONY: all install uninstall update opt byte doc clean distclean dist

all: src/META
	make -C src

opt byte update install uninstall: src/META
	make -C src $@

doc:
	make -C src htdoc
	mkdir -p doc
	rm -rf doc/html
	mv src/doc/irc/html doc
	rm -rf src/doc

clean:
	-make -C src clean
#	-make -C examples clean

distclean: clean
	rm -rf autom4te.cache config.log config.status src/META src/Makefile
	rm -rf doc
	-make -C examples distclean

dist: doc
	VERSION="$(VERSION)"; \
		mkdir $(PROGNAME)-$$VERSION; \
		cp -r --parents $(DISTFILES) $(PROGNAME)-$$VERSION; \
		tar zcvf $(PROGNAME)-$$VERSION.tar.gz $(PROGNAME)-$$VERSION; \
		rm -rf $(PROGNAME)-$$VERSION

src/META: src/META.in
	sed -e 's/@VERSION@/$(VERSION)/' $< > $@
