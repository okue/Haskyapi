SRC_C=Api/Capi.c
SO=lib$(O).so
O=api
TARGET=bin/haskyapictl

all: libapi.so main

libapi.so:
	gcc -shared $(SRC_C) -o $(SO) -fPIC

main :
	mkdir -p bin
	ghc --make Main -L. -l$(O) -o $(TARGET)

reset:
	make clean
	rm $(SO)

clean:
	rm -f *.o              *.hi
	rm -f Web/*.o          Web/*.hi
	rm -f Web/Haskyapi/*.o Web/Haskyapi/*.hi
	rm -f Foreign/*.o      Foreign/*.hi
	rm -f Api/*.o          Api/*.hi
