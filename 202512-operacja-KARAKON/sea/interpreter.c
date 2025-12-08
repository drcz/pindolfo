#include <assert.h>
#include <stdio.h>
#include "se.h"
#include "parser.h"

SE *S__, *S_SYM, *S_EXP, *S_QUOTE, *S_QUASIQUOTE, *S_UNQUOTE, *S_REC, *S_ARR;
SE *S_NO_MATCH, *S_NOT_FOUND;

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
    if(lhs==NIL) { put_back_SE(bnd); return S_NO_MATCH; }
    alhs = car(lhs);
    if(equal_SE(alhs, S_QUOTE)) { if(equal_SE(car(cdr(lhs)), exp)) return bnd;
                                  put_back_SE(bnd); return S_NO_MATCH; }
    if(equal_SE(alhs, S_SYM)) { if(TYPE(exp)!=SYM) { put_back_SE(bnd); return S_NO_MATCH; }
                                tmp = lookup(car(cdr(lhs)), bnd);
                                if(equal_SE(tmp, S_NOT_FOUND)) return mk_cons(mk_cons(CAR(CDR(lhs)), exp), bnd);
                                if(equal_SE(tmp, exp)) return bnd;
                                put_back_SE(bnd); return S_NO_MATCH; }
    if(equal_SE(alhs, S_EXP)) { tmp = lookup(car(cdr(lhs)), bnd);
                                if(equal_SE(tmp, S_NOT_FOUND)) return mk_cons(mk_cons(CAR(CDR(lhs)), exp), bnd);
                                if(equal_SE(tmp, exp)) return bnd;
                                put_back_SE(bnd); return S_NO_MATCH; }
    if(TYPE(exp)==CONS) { tmp = is_matching(CAR(lhs), CAR(exp), bnd);
                          if(equal_SE(tmp, S_NO_MATCH)) return S_NO_MATCH;
                          return is_matching(CDR(lhs), CDR(exp), tmp); }
    put_back_SE(bnd); return S_NO_MATCH;
}


SE *value_for(SE *, SE *, SE *);
SE *dispatch(SE *, SE *);

SE *clone(SE *e) { /* it's just a metaphor y'know */
    if(e!=NIL) REFCOUNT(e)++;
    return e;
}

SE *value_qq(SE *qq, SE *bnd, SE *prg) {
    if(qq!=NIL && TYPE(qq)==CONS) {
        if(equal_SE(CAR(qq),S_UNQUOTE)) return value_for(car(cdr(qq)), bnd, prg);
        return mk_cons(value_qq(CAR(qq), bnd, prg), value_qq(CDR(qq), bnd, prg));
    }
    return qq;
}

SE *value_for(SE *rhs, SE *bnd, SE *prg) {
    if(rhs==NIL) return NIL;
    //if(TYPE(rhs)==SYM) return clone(lookup(rhs, bnd)); /// hmmmmmm...
    if(TYPE(rhs)==SYM) return lookup(rhs, bnd);
    if(equal_SE(car(rhs), S_QUOTE)) return car(cdr(rhs));
    if(equal_SE(car(rhs), S_QUASIQUOTE)) return value_qq(car(cdr(rhs)), bnd, prg);
    if(equal_SE(car(rhs), S_REC)) return dispatch(value_qq(car(cdr(rhs)), bnd, prg), prg);
}

SE *dispatch(SE *exp, SE *prg) {
    SE *p0 = prg,*lhs,*rhs,*bnd,*res;
    printf("dsp: "); write_SE(exp); printf("\n");
    while(p0!=NIL) { assert(TYPE(p0)==CONS);
                     lhs = car(CAR(p0));
                     printf(" <> "); write_SE(lhs); printf("\n");
                     bnd = is_matching(lhs, exp, NIL);
                     printf(" ->b= "); write_SE(bnd); printf("\n");
                     if(bnd==NIL ||
                        TYPE(bnd)==CONS) { rhs = car(cdr(CAR(p0)));
                                           if(equal_SE(rhs, S_ARR)) rhs = car(cdr(cdr(CAR(p0))));
                                           put_back_SE(exp);
                                           res = clone(value_for(rhs, bnd, prg));
                                           put_back_SE(bnd);
                                           return res; }
                     p0 = cdr(p0); }
    printf("NO MATCH FOR "); write_SE(exp); printf("\n");
    put_back_SE(exp); return NIL;
}

void print_mem_stat() {
    SE *ss = get_all_symbols();
    UL c = 0; for(;ss!=NIL;ss=CDR(ss), c++) ;
    printf("--- free cells: %lu --- symbols: %lu ---\n", get_free_cells_count(),c);
}

void main() {
    SE *exp, *prg, *res;
    alloc_heap(500*1024);    
    init_consts();
    prg = read_SE(); /// todoooo!!!
    print_mem_stat();
    while(1) { printf("\n? "); exp = read_SE(); printf("\n");
               res = dispatch(exp, prg);
               write_SE(res); printf("\n");
               put_back_SE(res);
               print_mem_stat(); }
}
