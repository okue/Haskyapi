SRC_C=Api/Capi.c
SO=lib$(O).so
O=api
TARGET=bin/haskyapictl

all: main

main:
	mkdir -p bin
	ghc --make Main -o $(TARGET)

libapi.so:
	gcc -shared $(SRC_C) -o $(SO) -fPIC

main-using-c:
	mkdir -p bin
	ghc --make Main -L. -l$(O) -o $(TARGET)

reset:
	make clean
	rm $(SO)

clean:
	rm -f *.o              *.hi
	rm -f Web/*.o          Web/*.hi
	rm -f Web/Haskyapi/*.o Web/Haskyapi/*.hi
	rm -f Api/*.o          Api/*.hi
	rm -f Config/*.o       Config/*.hi
	rm -f Console/*.o      Console/*.hi
