#!/usr/bin/make -f

SRC= \
	src/source/syntax.ml \
	src/source/eval.ml src/source/type.ml \
	src/main.ml

COMPONENT= $(SRC)
TARGET= region_ml

MKTOPLOAD=  -I src/ -ccopt -Lsrc/ -I src/source/ -ccopt -Lsrc/source/
MKTOPUTOP= -thread -linkpkg -package utop
MKTOPFLAGS= $(MKTOPLOAD) $(MKTOPUTOP)

all: $(TARGET)

$(TARGET): $(COMPONENT)
	ocamlfind ocamlmktop -o _$(TARGET) $(MKTOPFLAGS) $(COMPONENT)
	echo "./_region_ml -I src/ -I src/source/" > region_ml
	chmod +x region_ml

clean:
	/bin/rm -f $(TARGET) _$(TARGET)
	/bin/rm -f src/*.cmi src/*.cmo src/*.mli
	/bin/rm -f src/source/*.cmi src/source/*.cmo src/source/*.mli
	/bin/rm -f src/target/*.cmi src/target/*.cmo src/target/*.mli
