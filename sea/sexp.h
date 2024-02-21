#ifndef _SEXP_H_
#define _SEXP_H_

typedef enum {NUM, SYM, CONS} SExpType;
typedef struct SExp SExp;

struct SExp {
    SExpType type;
    union { int num;
        char *sym;
        SExp *cons[2]; };
};

#define NIL ((void *)0)

#define TYPE(E)   (E)->type
#define NUMVAL(E) (E)->num
#define SYMVAL(E) (E)->sym
#define CAR(E)    (E)->cons[0]
#define CDR(E)    (E)->cons[1]

int are_equal(SExp *,SExp *);

SExp *mk_num(int);
SExp *mk_sym(char *);
SExp *mk_cons(SExp *,SExp *);

void free_se(SExp *);
SExp *clone_se(SExp *);

void init();

#endif
