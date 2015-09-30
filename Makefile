TARGET=make-full-linux-history

all:
	ocamlc -w -24 str.cma unix.cma common.ml make-full-linux-history.ml -o $(TARGET)

FILES=common.ml make-full-linux-history.ml \
      history-dave-annotated.log history-tglx-annotated-smallversion.log \
      Makefile


tar: 
	tar cvfz $(TARGET).tgz $(FILES)

clean: 
	rm *.cm* $(TARGET)


