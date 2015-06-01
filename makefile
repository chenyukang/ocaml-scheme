TARGET=scheme

SOURCES = \
    type.ml \
    scheme.ml \
	lexer.mll

OCAMLBUILD=ocamlbuild

default: byte

all: byte native

byte:
	$(OCAMLBUILD) $(TARGET).byte

native:
	$(OCAMLBUILD) $(TARGET).native

clean:
	$(OCAMLBUILD) -clean
