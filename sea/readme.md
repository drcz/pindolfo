just for the record, a historic moment:

```
 guile proto-compiler.scm > apd-compiled.c 
 mv apd-compiled.c sea/
 cd sea/
 gcc -o apd1 sexp.c parser.c apd-compiled.c 
 echo "(apd (q w e) (a s d) _id_)\n" | ./apd1
 > (q w e a s d)
```

now we can start making it reasonable...
