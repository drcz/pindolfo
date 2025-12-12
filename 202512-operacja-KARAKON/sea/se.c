#include <stdlib.h> /* malloc, free */
#include <string.h> /* strcmp (we could use our own tbh) */
#include <assert.h> /* guess what */

#include "se.h"

UL the_heap_size = 0;
SE *the_heap = NIL;
SE *the_free_cells = NIL;
SE *the_symbols = NIL;
SE *symbols_bookmark = NIL;

void alloc_heap(UL heap_size) {
    unsigned int i;
    if(the_heap != NIL) { free(the_heap); the_heap = NIL; }
    the_heap = (SE *)malloc(sizeof(SE) * heap_size); assert(the_heap);
    the_heap_size = heap_size; the_symbols = NIL; the_free_cells = NIL;
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
    if(TYPE(s)==SYM) free(SYMVAL(s));
    REFCOUNT(s) = 0; TYPE(s) = CONS;
    CAR(s) = NIL; CDR(s) = the_free_cells;
    the_free_cells = s;
}

UL get_heap_size() { return the_heap_size; }

UL get_free_cells_count() {
    SE *s; UL n;
    for(n=0,s=the_free_cells; s!=NIL; s=CDR(s),n++)
        assert(TYPE(s)==CONS && REFCOUNT(s)==0); /* SANITY */
    return n;
}

SE *get_all_symbols() { return the_symbols; }

void bookmark_symbols() { symbols_bookmark = the_symbols; }

void free_symbols_up_to_bookmark() {
    SE *tmp;
    while(the_symbols!=NIL &&
          the_symbols!=symbols_bookmark) { assert(TYPE(the_symbols)==CONS); /* SANITY */
                                           tmp = the_symbols; the_symbols = CDR(the_symbols);
                                           put_back_SE(CAR(tmp)); /* the symbol */
                                           put_back_SE(tmp); } /* cons that held it */
}

void free_SE(SE *expr) {
    /* recursive dereferencing and putback of any part with REFCOUNT <1 */
    if(expr==NIL || TYPE(expr)==SYM) return;
    if(REFCOUNT(expr)>0) { REFCOUNT(expr)--; return; }
    free_SE(CAR(expr)); free_SE(CDR(expr));
    put_back_SE(expr);
}

SE *mk_cons(SE *car, SE *cdr) {
    SE *s = pick_up_SE();
    assert(TYPE(s)==CONS && REFCOUNT(s)==0); /* SANITY */
    CAR(s) = car; CDR(s) = cdr;
    return s;
}

SE *mk_sym(char *str) {
    SE *s;
    for(s=the_symbols; s!=NIL; s=CDR(s))
        if(strcmp(SYMVAL(CAR(s)), str)==0) return CAR(s);
    /* ok so it's a new one! */
    s = pick_up_SE();
    assert(REFCOUNT(s)==0); /* SANITY */
    TYPE(s) = SYM; SYMVAL(s) = str;
    the_symbols = mk_cons(s, the_symbols);
    return s;
}

int equal_SE(SE *s0, SE *s1) {
    if(s0==s1) return 1;
    if(s0==NIL || s1==NIL) return 0;
    if(TYPE(s0)!=TYPE(s1)) return 0;
    switch(TYPE(s0)) { case SYM: return 0; /* yes! */
                       case CONS: return equal_SE(CAR(s0),CAR(s1))
                                      && equal_SE(CDR(s0),CDR(s1)); }
}
