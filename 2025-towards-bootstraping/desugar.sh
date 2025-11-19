#!/usr/bin/sh

case $# in
0) ./toolbox -mP | guile preprocess.scm
   ;;
1) ./toolbox -mP < $1 | guile preprocess.scm
   ;;
2) ./toolbox -mP < $1 | guile preprocess.scm > $2
   ;;
*)
   echo "usage: $0 [source.pindolfo] [sugarfree.pindolfo]"
   exit 1
esac
