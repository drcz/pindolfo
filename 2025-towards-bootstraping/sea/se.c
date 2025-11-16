#include <stdlib.h> /* malloc, free */
#include <string.h> /* strcmp (we could use our own tbh) */
#include <assert.h> /* guess what */

#include "se.h"

SE *the_heap = NIL;
SE *the_free_cells = NIL;
SE *the_symbols = NIL;

void alloc_heap(UL heap_size) {
    unsigned int i;
    if(the_heap != NIL) { free(the_heap);
                          the_heap = NIL; }
    the_heap = (SE *)malloc(sizeof(SE) * heap_size);
    assert(the_heap);
    the_symbols = NIL; the_free_cells = NIL;
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

SE *all_symbols() { return the_symbols; }

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
