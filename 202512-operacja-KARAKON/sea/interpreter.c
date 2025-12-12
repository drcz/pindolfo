#include <assert.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

#include "se.h"
#include "parser.h"

SE *S__, *S_SYM, *S_EXP, *S_QUOTE, *S_QUASIQUOTE, *S_UNQUOTE, *S_REC, *S_ARR;
SE *S_NO_MATCH, *S_NOT_FOUND;

typedef enum {OFF, ONLY_EXPRS, ALL} DbgLevel;
DbgLevel dbg_level;
SE *program;

#define DEFAULT_HEAPSIZE 64*1024

void init_consts() {
    S__ = mk_sym("_");
    S_SYM = mk_sym("sym");
    S_EXP = mk_sym("exp");
    S_QUOTE = mk_sym("quote");
    S_QUASIQUOTE = mk_sym("quasiquote");
    S_UNQUOTE = mk_sym("unquote");
    S_REC = mk_sym("rec");
    S_ARR = mk_sym("=>");
    S_NO_MATCH = mk_sym("NO-MATCH");
    S_NOT_FOUND = mk_sym("NOT-FOUND");
}


SE *clone(SE *e) { /* it's just a metaphor y'know, nobody gets cloned */
    if(e!=NIL && TYPE(e)==CONS) REFCOUNT(e)++;
    return e;
}

void free_expr(SE *expr, SE *bnd) {
    /* here we want to free all the pieces of expr which are not referenced */
    /* inside the binding, so we first mark them by raising their REFCOUNTs */
    SE *tmp = bnd;
    while(tmp!=NIL) { clone(CDR(CAR(tmp))); tmp = CDR(tmp); }
    /* and then freeing everything with REFCOUNT <1 */
    free_SE(expr);
}

void free_bnd(SE *bnd) {
    /* we don't want to touch its content since keys are from program and */
    /* vals from expr -- we just want to free the conses that hold them!  */
    SE *tmp;
    while(bnd!=NIL) { tmp = bnd; bnd = CDR(bnd);
                      put_back_SE(CAR(tmp)); /* key-val cons */
                      put_back_SE(tmp); } /* cons holding the above */
}

void deep_free_bnd(SE *bnd) {
    /* and this one removes vals with REFCOUNT<1 too */
    SE *tmp;
    while(bnd!=NIL) { tmp = bnd; bnd = CDR(bnd);
                      free_SE(CDR(CAR(tmp))); /* val */
                      put_back_SE(CAR(tmp)); /* key-val cons */
                      put_back_SE(tmp); } /* cons holding the above */
}


SE *lookup(SE *k, SE *bnd) {
    while(bnd!=NIL) { if(equal_SE(CAR(CAR(bnd)), k)) return CDR(CAR(bnd));
                      bnd = CDR(bnd); }
    return S_NOT_FOUND;
}

SE *car(SE *p) { assert(p!=NIL);
                 assert(TYPE(p)==CONS); return CAR(p); }
SE *cdr(SE *p) { assert(p!=NIL);
                 assert(TYPE(p)==CONS); return CDR(p); }


SE *is_matching(SE *lhs, SE *exp, SE *bnd) {
    SE *tmp, *alhs;
    if((lhs==NIL && exp==NIL) || equal_SE(lhs, S__)) return bnd;
    if(lhs==NIL) { free_bnd(bnd); return S_NO_MATCH; }
    alhs = car(lhs);
    if(equal_SE(alhs, S_QUOTE)) { if(equal_SE(car(cdr(lhs)), exp)) return bnd;
                                  free_bnd(bnd); return S_NO_MATCH; }
    if(equal_SE(alhs, S_SYM)) { if(exp==NIL || TYPE(exp)!=SYM) { free_bnd(bnd); return S_NO_MATCH; }
                                tmp = lookup(car(cdr(lhs)), bnd);
                                if(equal_SE(tmp, S_NOT_FOUND)) return mk_cons(mk_cons(CAR(CDR(lhs)), exp), bnd);
                                if(equal_SE(tmp, exp)) return bnd;
                                free_bnd(bnd); return S_NO_MATCH; }
    if(equal_SE(alhs, S_EXP)) { tmp = lookup(car(cdr(lhs)), bnd);
                                if(equal_SE(tmp, S_NOT_FOUND)) return mk_cons(mk_cons(CAR(CDR(lhs)), exp), bnd);
                                if(equal_SE(tmp, exp)) return bnd;
                                free_bnd(bnd); return S_NO_MATCH; }
    if(exp!=NIL &&
       TYPE(exp)==CONS) { tmp = is_matching(CAR(lhs), CAR(exp), bnd);
                          if(equal_SE(tmp, S_NO_MATCH)) return S_NO_MATCH;
                          return is_matching(CDR(lhs), CDR(exp), tmp); }
    free_bnd(bnd); return S_NO_MATCH;
}


SE *value_for(SE *, SE *);
SE *dispatch(SE *);

SE *value_qq(SE *qq, SE *bnd) {
    if(qq!=NIL && TYPE(qq)==CONS) {
        if(equal_SE(CAR(qq),S_UNQUOTE)) return value_for(car(cdr(qq)), bnd);
        return mk_cons(value_qq(CAR(qq), bnd), value_qq(CDR(qq), bnd));
    }
    return qq;
}

SE *value_for(SE *rhs, SE *bnd) {
    SE *val = NIL;
    if(rhs==NIL) return NIL;
    if(TYPE(rhs)==SYM) return clone(lookup(rhs, bnd));
    if(equal_SE(car(rhs), S_QUOTE)) return clone(car(cdr(rhs)));
    if(equal_SE(car(rhs), S_QUASIQUOTE)) return value_qq(car(cdr(rhs)), bnd);
    if(equal_SE(car(rhs), S_REC)) return dispatch(value_qq(car(cdr(rhs)), bnd));
}

SE *dispatch(SE *exp) {
    SE *p0 = program,*lhs,*rhs,*bnd,*res;
    if(dbg_level!=OFF) { printf("<e> "); write_SE(exp); printf("\n"); }
    while(p0!=NIL) { assert(TYPE(p0)==CONS);
                     lhs = car(CAR(p0));
                     bnd = is_matching(lhs, exp, NIL);
                     if(bnd==NIL ||
                        TYPE(bnd)==CONS) { if(dbg_level==ALL) {
                                             printf(" <m> "); write_SE(lhs); printf("\n");
                                             printf(" ~b> "); write_SE(bnd); printf("\n");
                                           }
                                           free_expr(exp, bnd);
                                           rhs = car(cdr(CAR(p0)));
                                           if(equal_SE(rhs, S_ARR)) rhs = car(cdr(cdr(CAR(p0))));
                                           res = value_for(rhs, bnd);
                                           deep_free_bnd(bnd);
                                           return res; }
                     p0 = cdr(p0); }
    printf("NO MATCH FOR "); write_SE(exp); printf("\n");
    free_expr(exp,NIL); return NIL; /* actually it should halt, no? */
}

void print_mem_stat() {
    SE *ss = get_all_symbols();
    UL c = 0; for(;ss!=NIL;ss=CDR(ss), c++) ;
    printf("--- free cells: %lu --- symbols: %lu ---\n", get_free_cells_count(),c);
}

void help(char *myname) {
    printf("usage:\n%s [options] <source file>\n", myname);
    printf("options:\n");
    printf(" -h \t\t print this info and quit\n");
    printf(" -s <uint> \t set heapsize in cells (default %lu)\n", DEFAULT_HEAPSIZE);
    printf(" -d [012]\t set debug level (default 0):\n");
    printf("\t 0 \t off\n");
    printf("\t 1 \t only expressions to be matched\n");
    printf("\t 2 \t all (expressions, matched pattern and binding)\n");
    printf("Auf wiedersehen!\n");
}

int main(int argc, char **argv) {
    SE *exp, *res; int c; FILE *f_src;
    UL heap_size = DEFAULT_HEAPSIZE;
    dbg_level = OFF;
    while((c = getopt(argc, argv, "s:d:h"))!=-1) {
        switch(c) {
        case 'h': help(argv[0]); return 0;
        case 's': heap_size = atol(optarg); break;
        case 'd': switch(optarg[0]) {
                  case '0': dbg_level = OFF; break;
                  case '1': dbg_level = ONLY_EXPRS; break;
                  case '2': dbg_level = ALL; break;
                  default: printf("unknown debug level %c!\n", optarg[0]); return 1;
                  } break;
        default: printf("unknown option %c!\n", c); return 1;
        }
    }
    alloc_heap(heap_size); printf("allocated %lu cells.\n", heap_size);
    init_consts();
    if(f_src = fopen(argv[optind],"r")) { set_input(f_src); program = read_SE(); set_input(stdin);
                                          printf("loaded source file %s.\n", argv[optind]); }
    else { printf("Could not open file %s.\n", argv[optind]); return 1; }
    bookmark_symbols();
    print_mem_stat();
    while(1) { printf("\n? "); exp = read_SE(); printf("\n");
               res = dispatch(exp);
               write_SE(res); printf("\n");
               free_SE(res);
               free_symbols_up_to_bookmark();
               print_mem_stat(); }
}
