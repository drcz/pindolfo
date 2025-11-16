all: memtest desugar

memtest: se.c memtest.c
	gcc -o memtest se.c memtest.c

desugar: se.c parser.c desugar.c
	gcc -o desugar se.c parser.c desugar.c

install:
	cp desugar ../
	cp memtest ../

clean:
	rm -f memtest desugar *.o

