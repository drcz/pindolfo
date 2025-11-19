#!/usr/bin/sh

case $# in
0) ./toolbox -mP | guile preprocess.scm | guile pindolfo2scm-1.scm
   ;;
1) ./toolbox -mP < $1 | guile preprocess.scm | guile pindolfo2scm-1.scm
   ;;
2) ./toolbox -mP < $1 | guile preprocess.scm | guile pindolfo2scm-1.scm > $2
   ;;
*)
   echo "usage: $0 [source.pindolfo] [target.scm]"
   exit 1
esac
