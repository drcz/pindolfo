all: memtest desugar

memtest: se.c memtest.c
	gcc -o memtest se.c memtest.c

desugar: se.c parser.c desugar.c
	gcc -o desugar se.c parser.c desugar.c

clean:
	rm -f memtest desugar *.o

