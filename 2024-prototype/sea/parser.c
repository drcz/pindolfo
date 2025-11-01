#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

#include "parser.h"

/* at the very least we'd like to have scheme-ish special syntax:
   '<exp> -> (quote <exp>)
   `<exp> -> (quasiquote <exp>)
   ,<exp> -> (unquote <exp>)

   and out type annotations:
   $<sym> -> (sym <sym>)
   %<sym> -> (num <sym>)
   @<sym> -> (atm <sym>)
   ?<sym> -> (exp <sym>)
   these symbols alone can however stand for usual symbols too, no?
   maybe not... though mixed feelings really. perhaps they should
   be parts of symbols initially and cut off later like in
   pindolf/parser.scm? not sure, let's see how it goes!
   
   oh right, also ``the hot parens'', maybe like:
   !<form> -> (& <form>)
   so that instead of this horrible (& (apd ,xs ,ys)) we could simply
   write !(apd ,xs ,ys) or sth. these things may change later too,
   especially because of ``the pattern vs expression duality'' heh.
   
   no string literals ofc, since who would need such a thing? */

#define TOK_BUFFER_SIZE 255

typedef enum {T_NUM,T_SYM,
              T_LPAR,T_RPAR,T_DOT,
              T_QUOTE,T_QUASIQUOTE,T_UNQUOTE,
              T_SYM_A,T_NUM_A,T_ATM_A,T_EXP_A,
              T_HOT,
              T_EOF} toktype;

struct {
    toktype type;
    char buffer[TOK_BUFFER_SIZE+1];
} cur_token;

FILE *_fp;

/* TODO: keeping track of row&col for err.msg. */
void get_token() {
    int c, i=0;
    do { c = fgetc(_fp); } while( isspace(c) );
    switch(c) {
    case EOF: cur_token.type = T_EOF; return;
    case '\'': cur_token.type = T_QUOTE; return;
    case '`': cur_token.type = T_QUASIQUOTE; return;
    case ',': cur_token.type = T_UNQUOTE; return;
    case '!': cur_token.type = T_HOT; return;
    case '(': cur_token.type = T_LPAR; return;
    case ')': cur_token.type = T_RPAR; return;
    case '.': cur_token.type = T_DOT; return;
    case '@': cur_token.type = T_ATM_A; return;
    case '$': cur_token.type = T_SYM_A; return;
    case '%': cur_token.type = T_NUM_A; return;
    case '?': cur_token.type = T_EXP_A; return;
    default:
        do { cur_token.buffer[i++] = c; }
        while ( i<TOK_BUFFER_SIZE &&
                (c = getc(_fp))!=EOF &&
                !isspace(c) &&
                c!='(' && c!=')' &&
                c!='\'' && c!='`' && c!=',' );
        ungetc(c,_fp);
        cur_token.buffer[i] = '\0';
        if(isdigit(cur_token.buffer[0]) ||
           (cur_token.buffer[0]=='-' &&
            isdigit(cur_token.buffer[1]))) {
            cur_token.type=T_NUM;
        } else {
            cur_token.type=T_SYM;
        }
    }
}

SExp *head_for(toktype t) {
    switch(t) {
    case T_QUOTE: return mk_sym(strdup("quote"));
    case T_QUASIQUOTE: return mk_sym(strdup("quasiquote"));
    case T_UNQUOTE: return mk_sym(strdup("unquote"));
    case T_SYM_A: return mk_sym(strdup("sym"));
    case T_NUM_A: return mk_sym(strdup("num"));
    case T_ATM_A: return mk_sym(strdup("atm"));
    case T_EXP_A: return mk_sym(strdup("exp"));
    case T_HOT: return mk_sym(strdup("&"));
    }
    assert(0==1); /// XD
}

SExp *get_cdr();

SExp *get_se() {
    SExp *h;
    switch(cur_token.type) {
    case T_EOF: return NIL; /* TODO: sure?! */
    case T_NUM: return mk_num(atoi(cur_token.buffer));
    case T_SYM: return mk_sym(strdup(cur_token.buffer));
    case T_LPAR:
        get_token();
        return get_cdr();
    case T_QUOTE:
    case T_QUASIQUOTE:
    case T_UNQUOTE:
    case T_HOT:
    
    case T_SYM_A:
    case T_NUM_A:
    case T_ATM_A:
    case T_EXP_A:
        h = head_for(cur_token.type);        
        get_token();
        /* TODO: checking for SYM after type annotators?? */
        return mk_cons(h, mk_cons(get_se(), NIL));
    default:
        assert(0==1); /// :)
    }
}

SExp *get_cdr() {
    SExp *car,*cdr;    
    if(cur_token.type==T_RPAR) return NIL;
    car = get_se();
    get_token();
    if(cur_token.type==T_RPAR) return mk_cons(car,NIL);
    if(cur_token.type==T_DOT) {
        get_token();
        cdr = get_se();
        get_token();
        assert(cur_token.type==T_RPAR); // lol XD
        return mk_cons(car, cdr);
    }
    cdr = get_cdr();
    return mk_cons(car, cdr);
}


SExp *read_se(FILE *f_in) {
    _fp = f_in;
    get_token();
    return get_se();
}

void write_cdr(SExp *, FILE *);

void write_se(SExp *s, FILE *f_out) {
    if(s==NIL) {
        fprintf(f_out, "()");
        return;
    }
    switch(TYPE(s)) {
    case NUM: fprintf(f_out, "%d", NUMVAL(s)); return;
    case SYM: fprintf(f_out, "%s", SYMVAL(s)); return;
    case CONS: fprintf(f_out, "(");
               write_se(CAR(s), f_out);
               write_cdr(CDR(s), f_out);
    }
}

void write_cdr(SExp *s, FILE *f_out) {
    if(s==NIL) {
        fprintf(f_out, ")");
        return;
    }
    if(TYPE(s)!=CONS) {
        fprintf(f_out, " . ");
        write_se(s, f_out);
        fprintf(f_out, ")");
        return;
    }
    fprintf(f_out," ");
    write_se(CAR(s), f_out);
    write_cdr(CDR(s), f_out);
}
