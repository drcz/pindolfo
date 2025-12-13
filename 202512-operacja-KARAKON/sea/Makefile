bin_name = karakon0

all: se.c parser.c interpreter.c
	gcc -o $(bin_name) se.c parser.c interpreter.c

install:
	cp $(bin_name) ../

clean:
	rm -f $(bin_name) *.o

tests:
	./run-tests.rb ${bin_name}
