#include <stdlib.h> /* malloc, free */
#include <string.h> /* strcmp (we could use our own tbh) */
#include <assert.h> /* guess what */
#include <stdio.h> /* just tmp for printf tests */

typedef unsigned long UL;
typedef enum { CONS, NUM, SYM } SEtype;
typedef struct SE SE;

struct SE { SEtype type;
            UL refcount; /* too much??? hunoz! */
            union { UL num;
                    const char *str;
                    SE *cons[2]; }; };

#define NIL ((void *)0)

#define TYPE(E) (E)->type
#define REFCOUNT(E) (E)->refcount
#define NUMVAL(E) (E)->num
#define SYMVAL(E) (E)->str
#define CAR(E) (E)->cons[0]
#define CDR(E) (E)->cons[1]

SE *the_heap = NIL;
SE *the_free_cells = NIL;
SE *the_symbols = NIL;

void alloc_heap(UL heap_size) {
    unsigned int i;
    if(the_heap != NIL) { free(the_heap);
                          the_heap = NIL; }
    the_heap = (SE *)malloc(sizeof(SE) * heap_size);
    assert(the_heap);
    the_symbols = NIL;
    the_free_cells = NIL;
    for(i=0; i<heap_size; i++) { TYPE(&the_heap[i]) = CONS;
                                 CAR(&the_heap[i]) = NIL;
                                 CDR(&the_heap[i]) = the_free_cells;
                                 the_free_cells = &the_heap[i]; }
}

SE *pick_up_SE() {
    SE *s;
    assert(the_free_cells != NIL); /* TODO: allocate more? */
    s = the_free_cells;
    assert(TYPE(s)==CONS && REFCOUNT(s)==0); /* SANITY */
    the_free_cells = CDR(s);
    return s;
}

void put_back_SE(SE *s) {
    if(s==NIL) return;
    if(REFCOUNT(s)>0) REFCOUNT(s)--;
    if(REFCOUNT(s)==0) {
        if(TYPE(s) == CONS) { put_back_SE(CAR(s)); /* callstack happy? */
                              put_back_SE(CDR(s)); }
        TYPE(s) = CONS; CAR(s) = NIL; CDR(s) = the_free_cells;
        the_free_cells = s;
    }
}

UL free_cells_count() {
    SE *s; UL n;
    for(n=0,s=the_free_cells; s!=NIL; s=CDR(s),n++)
        assert(TYPE(s)==CONS && REFCOUNT(s)==0); /* SANITY */
    return n;
}

SE *mk_cons(SE *car, SE *cdr) {
    SE *s = pick_up_SE();
    assert(TYPE(s)==CONS && REFCOUNT(s)==0); /* SANITY */
    CAR(s) = car; CDR(s) = cdr;
    if(car!=NIL) REFCOUNT(car)++;
    if(cdr!=NIL) REFCOUNT(cdr)++;
    return s;
}

SE *mk_num(UL n) {
    SE *s = pick_up_SE();
    assert(REFCOUNT(s)==0); /* SANITY */
    TYPE(s) = NUM; NUMVAL(s) = n;
    return s;
}

SE *mk_sym(const char *str) {
    SE *s;
    for(s=the_symbols; s!=NIL; s=CDR(s))
        if(strcmp(SYMVAL(CAR(s)), str)==0) return CAR(s);
    /* ok so it's a new one! */
    s = pick_up_SE();
    assert(REFCOUNT(s)==0); /* SANITY */
    TYPE(s) = SYM; SYMVAL(s) = str;
    the_symbols = mk_cons(s, the_symbols);
    /* TODO: don't we want to have REFCOUNT(the_symbols)==1 too?? */
    return s;
}

int equal_SE(SE *s0, SE *s1) {
    if(s0==s1) return 1;
    if(s0==NIL || s1==NIL) return 0;
    if(TYPE(s0)!=TYPE(s1)) return 0;
    switch(TYPE(s0)) { case NUM: return NUMVAL(s0)==NUMVAL(s1);
                       case SYM: return 0; /* yes! */
                       case CONS: return equal_SE(CAR(s0),CAR(s1))
                                      && equal_SE(CDR(s0),CDR(s1)); }
}

/////////////////////////////////////////////////////////////////////
/// crude testing, because reasons.
void main() {
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
    assert(the_symbols != NIL);
    assert(CAR(the_symbols)==s1);
    assert(CDR(the_symbols)==NIL);

    s2 = mk_sym("Eris");
    assert(s2==s1);
    assert(REFCOUNT(s1)==1);
    assert(CAR(the_symbols)==s1);
    assert(CDR(the_symbols)==NIL);

    s2 = mk_sym("an-apple");
    assert(s2!=s1);
    assert(TYPE(s2)==SYM);
    assert(REFCOUNT(s2)==1);
    assert(CAR(the_symbols)==s2);
    assert(CAR(CDR(the_symbols))==s1);
    assert(CDR(CDR(the_symbols))==NIL);
    printf("mk_sym ok\n");

    printf("the_symbols:");
    for(SE *s = the_symbols; s!=NIL; s=CDR(s)) {
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
    printf("all good so far!");
}
