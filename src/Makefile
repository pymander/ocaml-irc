SOURCES=irc.mli irc.ml
RESULT=irc

OCAMLDOCFLAGS = -stars

all: byte opt

byte: byte-code-library

opt: native-code-library

install: libinstall

uninstall: libuninstall

update: uninstall install

-include OCamlMakefile
