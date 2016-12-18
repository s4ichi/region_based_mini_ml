#!/Usr/bin/make -f

SRC=source.ml parser.mly lexer.mll target.ml main.ml
COMPONENT=source.ml target.ml parser.mli parser.ml lexer.ml main.ml
TARGET=region_ml

all: $(TARGET)

$(TARGET): $(COMPONENT)
	ocamlfind ocamlmktop -o $(TARGET) $(COMPONENT)

parser.mli: parser.mly
	ocamlyacc parser.mly

parser.ml:  parser.mly
	ocamlyacc parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

clean:
	/bin/rm -f $(TARGET)
	/bin/rm -f parser.ml parser.mli lexer.ml
	/bin/rm -f *.cmi *.cmo *.mli
