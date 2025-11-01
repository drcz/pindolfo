#include<stdio.h>
#include"sexp.h"
#include"parser.h"
/// gcc -o test1 sexp.c parser.c test1.c

void main() {
    SExp *s;
    init();
    printf("> ");
    s = read_se(stdin);
    printf("read this:\n");
    write_se(s,stdout);
    printf("\n");
}
