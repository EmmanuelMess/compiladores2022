all: macc

%.o: CFLAGS+= -Wall -pedantic
LDLIBS +=-lgc
macc: macc.c


clean:
	rm -f *.o macc

.PHONY: clean all
