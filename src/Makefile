#!/Usr/bin/make -f

SRC=target.ml main.ml
COMPONENT= target.ml main.ml
TARGET=region_ml

all: $(TARGET)

$(TARGET): $(COMPONENT)
	ocamlfind ocamlmktop -annot -o $(TARGET) $(COMPONENT)

clean:
	/bin/rm -f $(TARGET)
	/bin/rm -f *.cmi *.cmo *.mli
