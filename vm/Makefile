all: macc

%.o: CFLAGS+= -Wall -pedantic
LDLIBS +=-lgc -ggdb
macc: macc.c


clean:
	rm -f *.o macc

.PHONY: clean all
