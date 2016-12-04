#!/usr/bin/make -f

SRC=src/source.ml src/target.ml src/main.ml
COMPONENT=$(SRC)
TARGET=region_ml

MKTOPLOAD=  -I src/ -ccopt -Lsrc/
MKTOPUTOP= -thread -linkpkg -package utop
MKTOPFLAGS= $(MKTOPLOAD) $(MKTOPUTOP)

all: $(TARGET)

$(TARGET): $(COMPONENT)
	ocamlfind ocamlmktop -o _$(TARGET) $(MKTOPFLAGS) $(COMPONENT)
	echo "./_region_ml -I src/" > region_ml
	chmod +x region_ml

.PHONY: clean
clean:
	/bin/rm -f $(TARGET) _$(TARGET)
	/bin/rm -f src/*.cmi src/*.cmo src/*.mli
