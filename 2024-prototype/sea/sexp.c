#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "sexp.h"

/* this is super-naive implementation, to get stuff working
   and to figure out how much (or how little) we gain from various
   optimizations, hacks and stuff. so each symbol is a separate
   entry with separate char* pointer etc */

#define DEFAULT_HEAPSIZE 65536 /* not too much hehe */

SExp *first_free;

SExp *alloc_heap(unsigned int heap_size) {
    unsigned int i;
    SExp *s = NIL;
    SExp *heap = (SExp *)malloc(sizeof(SExp)*heap_size);
    assert(heap);
    /* does it matter if we run it in this direction or backwards? */
    for(i=0;i<heap_size;i++) {
        TYPE(&heap[i]) = CONS;
        CAR(&heap[i]) = NIL; /* whatever really? */
        CDR(&heap[i]) = s;
        s = &heap[i];
    }
    return s;
}

void init() {
    first_free = alloc_heap(DEFAULT_HEAPSIZE);
    /* later we'll add symbols references and stuff here */
}

SExp *alloc_se() {
    SExp *s;
    assert(first_free!=NIL); /* TODO: ``allocate more'' strategy */
    s = first_free;
    first_free = CDR(s);
    return s;
}

SExp *clone_se(SExp *s0) {
    SExp *s1;
    if(s0==NIL) return NIL;
    s1 = alloc_se();
    switch(TYPE(s0)) {
    case CONS: TYPE(s1) = CONS;
               CAR(s1) = clone_se(CAR(s0));
               CDR(s1) = clone_se(CDR(s0));
               break;
    case NUM: TYPE(s1) = NUM;
              NUMVAL(s1) = NUMVAL(s0);
              break;
    case SYM: TYPE(s1) = SYM;
              SYMVAL(s1) = strdup(SYMVAL(s0)); /* !!! */
              break;
    }
    return s1;
}

void free_se(SExp *s) {
    if(s==NIL) return;
    if(TYPE(s)==CONS) {
        /* wonder how much will C's stack allow here */
        free_se(CAR(s));
        free_se(CDR(s));
    } else {
        TYPE(s) = CONS;
    }
    CAR(s) = NIL;
    CDR(s) = first_free;
    first_free = s;
}
/* TODO: reference counting, no? */


SExp *mk_num(int n) {
    SExp *num = alloc_se();
    TYPE(num) = NUM;
    NUMVAL(num) = n;
    return num;
}

SExp *mk_sym(char *s) {
    SExp *sym = alloc_se();
    TYPE(sym) = SYM;
    SYMVAL(sym) = s; ///strdup(s); /// hmm.
    return sym;
}

SExp *mk_cons(SExp *car,SExp *cdr) {
    SExp *cons = alloc_se();
    TYPE(cons) = CONS;
    CAR(cons) = car;
    CDR(cons) = cdr;
    return cons;
}

int are_equal(SExp *s0, SExp *s1) { /* for now super naive one */
    if(s0==NIL && s1==NIL) return 1;
    if(s0==NIL || s1==NIL) return 0;
    if(TYPE(s0)!=TYPE(s1)) return 0;
    if(TYPE(s0)==NUM && NUMVAL(s0)==NUMVAL(s1)) return 1;
    if(TYPE(s0)==SYM && strcmp(SYMVAL(s0),SYMVAL(s1))==0) return 1;
    return 0;
}
