#!/bin/sh

guile pindolfo2scm-0.scm < drc.ppf > drc.scm
echo "(dbl-apd-test (M A) (K U L A) (T U R A))" | guile drc.scm
#rm drc.scm
