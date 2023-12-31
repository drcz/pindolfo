(use-modules (grand scheme))
(add-to-load-path "./")
(define pindolf (@ (pindolf interpreter) pindolf))
(define pindolf:value (@ (pindolf interpreter) value))
(define pindolf:matching? (@ (pindolf interpreter) matching?))
(define compiled (@ (pindolf compiler) compiled))
(define run (@ (pindolf vm) run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define t3
'( ;; it's a nice test rly (or should we import tests? naaah)

(('fold-r op e       ()) e)
(('fold-r op e (x . xs)) (& (,op ,x ,(& (fold-r ,op ,e ,xs)))))

(('cons h t) `(,h . ,t))
(('apd xs ys) (& (fold-r cons ,ys ,xs)))

((('cons*f.hd f) h t) (& (cons ,(& (,f ,h)) ,t)))
(('map f xs) (& (fold-r (cons*f.hd ,f) () ,xs)))

(('dup x) `(,x . ,x))
(('dbl n) (+ n n))
))

(e.g. (pindolf '(apd (q w e) (a s d)) t3) ===> (q w e a s d))
(e.g. (pindolf '(map dbl (1 2 3)) t3) ===> (2 4 6))
(e.g. (pindolf '(map dup (- 0 ^)) t3) ===> ((- . -) (0 . 0) (^ . ^)))

(e.g. (run (compiled t3) '(apd (q w e) (a s d))) ===> (q w e a s d))
(e.g. (run (compiled t3) '(map dbl (1 2 3))) ===> (2 4 6))
(e.g. (run (compiled t3) '(map dup (- 0 ^)))
      ===> ((- . -) (0 . 0) (^ . ^)))


#| todo
conceptually:
 - shouldn't only atoms be quoted in patterns?
 - we need some var types (sym, num, any/exp) to write self-interpreter
 - tail call optimization in the compiler [optional but neat]
 - some step debugger seems necessary too though that's tricky
 with these we could move on with pindof in pindolf and then
 abstract interpretation...
|#
