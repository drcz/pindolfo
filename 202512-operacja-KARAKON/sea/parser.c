#include <stdlib.h> /* atoi */
#include <stdio.h> /* printf, getc, ungetc, stdin */
#include <ctype.h> /* isspace, isdigit */
#include <string.h> /* strdup */
#include <assert.h> /* (it'll go away) */

#include "se.h"
#include "parser.h"

#define TOK_BUFFER_SIZE 255

typedef enum { T_SYM,
               T_LPAR, T_RPAR, T_DOT,
               T_QUOTE, T_QUASIQUOTE, T_UNQUOTE,
               T_SYM_A, T_EXP_A, T_REC,
               T_COMMENT, T_EOF } TOKtype;

struct {  TOKtype type;
    char buffer[TOK_BUFFER_SIZE+1];
} cur_token;

void get_token() {
    int c, i = 0;
    do { c = getc(stdin); } while( isspace(c) );
    switch(c) {
    case EOF: cur_token.type = T_EOF; return; /* hmmm */
    case '\'': cur_token.type = T_QUOTE; return;
    case '`': cur_token.type = T_QUASIQUOTE; return;
    case ',': cur_token.type = T_UNQUOTE; return;
    case '^': cur_token.type = T_REC; return;
    case ';': cur_token.type = T_COMMENT; return;
    case '$': cur_token.type = T_SYM_A; return;
    case '?': cur_token.type = T_EXP_A; return;
    case '(': cur_token.type = T_LPAR; return;
    case ')': cur_token.type = T_RPAR; return;
    case '.': cur_token.type = T_DOT; return;
    default: do { cur_token.buffer[i++] = c; }
             while ( i<TOK_BUFFER_SIZE &&
                     (c = getc(stdin))!=EOF &&
                      !isspace(c) && c!='(' && c!=')') ;
             ungetc(c, stdin);
             cur_token.buffer[i] = '\0';
             cur_token.type = T_SYM;
             return;
    }
}

SE *head_for(TOKtype t) {
    switch(t) {
    case T_QUOTE: return mk_sym("quote");
    case T_QUASIQUOTE: return mk_sym("quasiquote");
    case T_UNQUOTE: return mk_sym("unquote");
    case T_SYM_A: return mk_sym("sym");
    case T_EXP_A: return mk_sym("exp");
    case T_REC: return mk_sym("rec");
    }
    assert(0==1); /* notreached */
}

SE *get_cdr();

SE *get_SE() {
    SE *h;
    switch(cur_token.type) {
    case T_EOF: return NIL; /* TODO: sure? */
    case T_SYM: return mk_sym(strdup(cur_token.buffer));
    case T_LPAR: get_token(); return get_cdr();
    case T_QUOTE: case T_QUASIQUOTE: case T_UNQUOTE:
    case T_SYM_A: case T_EXP_A:
    case T_REC: h = head_for(cur_token.type);
                get_token();
                /* TODO checking for SYM after type annotators?? -> naah */
                return mk_cons(h, mk_cons(get_SE(), NIL));
    case T_COMMENT: get_token(); get_SE(); /* commented expr, ignore */
                    get_token(); return get_SE();
    }
    assert(0==1); /* notreached */
}

SE *get_cdr() {
    SE *car, *cdr;    
    if(cur_token.type==T_RPAR) return NIL;
    car = get_SE();
    get_token();
    if(cur_token.type==T_COMMENT) { get_token(); get_SE();
                                    get_token(); } /* not elegant but... */
    if(cur_token.type==T_RPAR) return mk_cons(car,NIL);
    if(cur_token.type==T_DOT) {
        get_token();
        cdr = get_SE();
        get_token();
        assert(cur_token.type==T_RPAR); /* TODO parse error or sth? */
        return mk_cons(car, cdr);
    }
    cdr = get_cdr();
    return mk_cons(car, cdr);
}

SE *read_SE() { get_token(); return get_SE(); }

void write_cdr(SE *);

void write_SE(SE *s) {
    if(s==NIL) { printf("()"); return; }
    switch(TYPE(s)) {
    case SYM: printf("%s", SYMVAL(s)); return;
    case CONS: printf("(");
               write_SE(CAR(s));
               write_cdr(CDR(s));
               return;
    }
    assert(0==1);
}

void write_cdr(SE *s) {
    while(s!=NIL) { if(TYPE(s)!=CONS) { printf(" . ");
                                        write_SE(s);
                                        printf(")");
                                        return; }
                    printf(" "); write_SE(CAR(s));
                    s = CDR(s); }
   printf(")");
}
