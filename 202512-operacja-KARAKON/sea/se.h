#ifndef _SE_H_
#define _SE_H_

typedef unsigned long UL;
typedef enum { CONS, SYM } SEtype;
typedef struct SE SE;

struct SE { SEtype type;
            UL refcount; /* too much??? hunoz! */
            union { char *str;
                    SE *cons[2]; }; };

#define NIL ((void *)0)

#define TYPE(E) (E)->type
#define REFCOUNT(E) (E)->refcount
#define SYMVAL(E) (E)->str
#define CAR(E) (E)->cons[0]
#define CDR(E) (E)->cons[1]

void alloc_heap(UL);
UL get_heap_size();
UL get_free_cells_count();
SE *get_all_symbols();
void bookmark_symbols();
void free_symbols_up_to_bookmark();

SE *mk_cons(SE *, SE *);
SE *mk_sym(char *);
void put_back_SE(SE *);
void free_SE(SE *);

int equal_SE(SE *, SE *);

#endif
