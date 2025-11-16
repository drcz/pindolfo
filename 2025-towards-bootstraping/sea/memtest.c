#include <stdio.h>
#include <assert.h>

#include "se.h"

void test_mem() {
    SE *s0, *s1, *s2, *s3, *s4;
    UL n,m;
    alloc_heap(1024);
    printf("alloc ok\n");

    s0 = mk_num(23);
    assert(REFCOUNT(s0)==0); /* not referenced from anywhere yet */
    assert(TYPE(s0)==NUM);
    printf("mk_num ok\n");

    s1 = mk_sym("Eris");
    assert(REFCOUNT(s1)==1); /* 1 for the_symbols ref y'know */
    assert(TYPE(s1)==SYM);
    s4 = all_symbols();
    assert(s4 != NIL);
    assert(CAR(s4)==s1);
    assert(CDR(s4)==NIL);

    s2 = mk_sym("Eris");
    assert(s2==s1);
    assert(REFCOUNT(s1)==1);
    s4 = all_symbols();
    assert(CAR(s4)==s1);
    assert(CDR(s4)==NIL);

    s2 = mk_sym("an-apple");
    assert(s2!=s1);
    assert(TYPE(s2)==SYM);
    assert(REFCOUNT(s2)==1);
    s4 = all_symbols();
    assert(CAR(s4)==s2);
    assert(CAR(CDR(s4))==s1);
    assert(CDR(CDR(s4))==NIL);
    printf("mk_sym ok\n");

    printf("the_symbols:");
    for(SE *s = all_symbols(); s!=NIL; s=CDR(s)) {
        assert(TYPE(CAR(s))==SYM);
        assert(REFCOUNT(CAR(s))>0);
        printf(" %s", SYMVAL(CAR(s)));
    }
    printf(" -- ok?\n");

    s3 = mk_cons(s0, s1);
    assert(REFCOUNT(s0)==1); /* just the cons */
    assert(REFCOUNT(s1)==2); /* the_symbols + cons */
    assert(REFCOUNT(s2)==1); /* just the_symbols */
    assert(REFCOUNT(s3)==0); /* nothing yet */
    assert(TYPE(s3)==CONS); assert(CAR(s3)==s0); assert(CDR(s3)==s1);
    printf("mk_cons ok\n");

    s4 = mk_cons(s2 ,s3);
    assert(REFCOUNT(s0)==1); /* just s3 */
    assert(REFCOUNT(s1)==2); /* the_symbols + s3 */
    assert(REFCOUNT(s2)==2); /* the_symbols + s4 */
    assert(REFCOUNT(s3)==1); /* just s4 */
    assert(REFCOUNT(s4)==0); /* nothing yet */
    assert(TYPE(s4)==CONS); assert(CAR(s4)==s2); assert(CDR(s3)==s1);
    printf("mk_cons refs ok\n");

    put_back_SE(s4);
    assert(REFCOUNT(s0)==0);
    assert(REFCOUNT(s1)==1);
    assert(REFCOUNT(s2)==1);
    assert(REFCOUNT(s3)==0);
    assert(REFCOUNT(s4)==0);
    printf("put_back pt.1 ok\n");

    s0 = NIL; s1 = mk_sym("SPACE-FILLA");
    n = free_cells_count();
    printf("free cells: %lu\n", n);
    for(int i=0;i<1000;i++) s0 = mk_cons(s1,s0);
    printf("1k space-fillas ok\n");
    printf("free cells: %lu\n", free_cells_count());
    printf("now put_back...\n");
    put_back_SE(s0);
    m = free_cells_count();
    printf("free cells: %lu\n", m);
    assert(n==m);
    printf("put_back pt.2 ok\n");

    assert(equal_SE(mk_num(997),mk_num(997)));
    assert(equal_SE(mk_sym("Eris"),mk_sym("Eris")));
    s0 = mk_cons(mk_sym("aaa"),mk_sym("bbb"));
    s1 = mk_cons(mk_sym("aaa"),mk_sym("bbb"));
    assert(equal_SE(s0,s1));
    printf("eq ok\n");

    assert(!equal_SE(mk_num(42),mk_num(69)));
    assert(!equal_SE(mk_sym("Eris"),mk_sym("bbb")));
    s0 = mk_cons(mk_sym("aaa"),mk_sym("aaa"));
    s1 = mk_cons(mk_sym("aaa"),mk_sym("bbb"));
    assert(!equal_SE(s0,s1));
    printf("!eq ok\n");
    /* (...) */
}

void main() { test_mem(); printf("all good, auf wiedersehen!\n"); }
