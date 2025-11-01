#!/bin/sh

guile pindolfo2scm.scm < drc.ppf > drc.scm
echo "(id (it's alive!))" | guile drc.scm
echo "(dbl-apd-test (M A) (K U L A) (T U R A))" | guile drc.scm
