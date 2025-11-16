#include <stdio.h>
#include "se.h"
#include "parser.h"

void main() { alloc_heap(1024*1024); write_SE(read_SE()); printf("\n"); }
