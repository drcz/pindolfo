#include<stdio.h>
#include"sexp.h"
/// gcc -o test0 sexp.c test0.c

void main() {
    int i;
    SExp *s,*s2;
    init();
    printf("init ok\n");
    s = mk_cons(mk_num(23),mk_sym("symbollo"));
    printf("mk_*** ok\n");
    printf("s: (%i . %s)\n", NUMVAL(CAR(s)),SYMVAL(CDR(s)));
    s2 = clone_se(s);
    printf("s2: (%i . %s)\n", NUMVAL(CAR(s2)),SYMVAL(CDR(s2)));
    free_se(s);
    printf("free s ok\n");
    printf("s2: (%i . %s)\n", NUMVAL(CAR(s2)),SYMVAL(CDR(s2)));
    free_se(s2);
    printf("free s2 ok\n\n");
    
    s = NIL;
    for(i=0;i<10000;i++) {
        s = mk_cons(mk_sym("SPACE-FILLA"),s);
    }
    printf("10k space-fillas ok\n");
    s2 = clone_se(s);
    printf("10k clone ok\n");
    free_se(s);
    free_se(s2);
    printf("free ok\n");
}
