all: memtest toolbox

memtest: se.c memtest.c
	gcc -o memtest se.c memtest.c

toolbox: se.c parser.c toolbox.c
	gcc -o toolbox se.c parser.c toolbox.c

install:
	cp toolbox ../

clean:
	rm -f memtest toolbox *.o

