TARGET=scheme

SOURCES = \
    type.ml \
    scheme.ml \
    lexer.mll

OCAMLBUILD = ocamlbuild -pkg alcotest

default: byte

all: byte native test

byte:
	$(OCAMLBUILD) $(TARGET).byte

native:
	$(OCAMLBUILD) $(TARGET).native

test:
	$(OCAMLBUILD) test.byte; ./test.byte

clean:
	$(OCAMLBUILD) -clean
