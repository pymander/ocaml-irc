SOURCES=irc.mli irc.ml
RESULT=irc

LIBINSTALL_FILES = $(wildcard META *.mli *.cmi *.cma *.cmxa *.a *.so)
OCAMLDOCFLAGS = -stars
NO_CUSTOM = yes

all: byte opt

byte: byte-code-library

opt: native-code-library

install: libinstall

uninstall: libuninstall

update: uninstall install

-include OCamlMakefile
