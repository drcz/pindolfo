#include <stdio.h>
#include <stdlib.h> /* atol */
#include <unistd.h> /* getopt */
#include "se.h"
#include "parser.h"

#define DEFAULT_HEAPSIZE 1024*1024
typedef enum {MODE_DESUGAR,
              MODE_DESUGAR_PACKED,
              MODE_EQUALS,
              MODE_COUNTS,
              MODE_SYMBOLS} TBmode;

/* i've no idea yet how i want this to work (or if at all)
   so it's just some random crap packed into single program */

void help(char *myname) {
    printf("usage:\n%s [options] < infile > outfile\n", myname);
    printf("options:\n");
    printf(" -h \t\t print this info and quit\n");
    printf(" -s <uint> \t set heapsize in cells (default %lu)\n", DEFAULT_HEAPSIZE);
    printf(" -m [DPCSE]\t set mode (default d):\n");
    printf("\t D \t reads expr and prints desugared form\n");
    printf("\t P \t as above but packed inside (unLETized _) for preprocessing script\n");
    printf("\t C \t reads expr and prints its memory (heap) use in cells\n");
    printf("\t S \t reads expr and prints its unique symbols\n");
    printf("\t E \t reads two exprs and prints 'EQUALS' if they have same shape, 'NOT EQUALS' otherwise\n");
    printf("Auf wiedersehen!\n");
}

int main(int argc, char **argv) {
  int c; UL n;
  UL heap_size = DEFAULT_HEAPSIZE;
  TBmode mode = MODE_DESUGAR;  
  SE *e0, *e1;
  while((c=getopt(argc, argv,"s:m:h"))!=-1) {
    switch(c) {
    case 'h': help(argv[0]); return 0;
    case 's': heap_size = atol(optarg); break;
    case 'm': switch(optarg[0]) {
              case 'D': mode = MODE_DESUGAR; break;
              case 'P': mode = MODE_DESUGAR_PACKED; break;
              case 'S': mode = MODE_SYMBOLS; break;
              case 'C': mode = MODE_COUNTS; break;
              case 'E': mode = MODE_EQUALS; break;
              default: printf("unknown mode %c!\n", optarg[0]); return 1;
              } break;
    default: printf("unknown option %c!\n", c); return 1;
    }
  }
  alloc_heap(heap_size);
  switch(mode) {
      case MODE_DESUGAR:
          write_SE(read_SE()); printf("\n"); break;
      case MODE_DESUGAR_PACKED:
          write_SE(mk_cons(mk_sym("unLETized"),
                           mk_cons(read_SE(), NIL)));
          printf("\n"); break;
      case MODE_EQUALS:
          e0 = read_SE(); e1 = read_SE();
          if(equal_SE(e0, e1)) printf("EQUAL\n"); 
                          else printf("NOT EQUAL\n");
          break;
      case MODE_COUNTS:
          read_SE();
          heap_size = get_heap_size(); /* never know */
          n = get_free_cells_count();
          printf("heap size: %lu\n",heap_size);
          printf("free cells: %lu\n",n);
          printf("used cells: %lu (incl.symbols list)\n", heap_size - n);
          break;
      case MODE_SYMBOLS:
          read_SE();
          for(e1=get_all_symbols(); e1!=NIL; e1=CDR(e1)) {
              printf("%s\n",SYMVAL(CAR(e1)));
          }
          break;
  }
  return 0;
}
