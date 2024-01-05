(use-modules (grand scheme))
(add-to-load-path "./")
(define pindolf (@ (pindolf interpreter) pindolf))
(define parsed (@ (pindolf parser) parsed))
(define compiled (@ (pindolf compiler) compiled))
(define run (@ (pindolf vm) run))

(define pinp
  (with-input-from-file "pindolf-self-interpreter.sexp" read))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define p1
  '((('apd () ?ys) ys)
    (('apd (?x . ?xs) ?ys) `(,x . ,(& (apd ,xs ,ys))))))

(e.g. (pindolf '(apd (q w e) (1 2 3)) p1) ===> (q w e 1 2 3))

(define c1 #;(compiled p1)
  '(((0) (GOTO (0 0)))
    ((0 0) (IF (CONS? *VIEW*) (0 1) ELSE (1 0)))
    ((0 1) (IF (EQ? (CAR *VIEW*) 'apd) (0 2) ELSE (1 0)))
    ((0 2) (IF (CONS? (CDR *VIEW*)) (0 3) ELSE (1 0)))
    ((0 3) (IF (NIL? (CAR (CDR *VIEW*))) (0 4) ELSE (1 0)))
    ((0 4) (IF (CONS? (CDR (CDR *VIEW*))) (0 5) ELSE (1 0)))
    ((0 5) (IF (NIL? (CDR (CDR (CDR *VIEW*)))) (0 6) ELSE (1 0)))
    ((0 6) (LET ys (CAR (CDR (CDR *VIEW*))))
           (RETURN ys))
    ((1 0) (IF (CONS? *VIEW*) (1 1) ELSE (2 0)))
    ((1 1) (IF (EQ? (CAR *VIEW*) 'apd) (1 2) ELSE (2 0)))
    ((1 2) (IF (CONS? (CDR *VIEW*)) (1 3) ELSE (2 0)))
    ((1 3) (IF (CONS? (CAR (CDR *VIEW*))) (1 4) ELSE (2 0)))
    ((1 4) (IF (CONS? (CDR (CDR *VIEW*))) (1 5) ELSE (2 0)))
    ((1 5) (IF (NIL? (CDR (CDR (CDR *VIEW*)))) (1 6) ELSE (2 0)))
    ((1 6) (LET ys (CAR (CDR (CDR *VIEW*))))
           (LET xs (CDR (CAR (CDR *VIEW*))))
           (LET x (CAR (CAR (CDR *VIEW*))))
           (LET *VIEW* (CONS 'apd (CONS xs (CONS ys ()))))
           (LET (V 0) (CALL (0) *VIEW*))
           (RETURN (CONS x (V 0)))))
)

;;; driving time...
