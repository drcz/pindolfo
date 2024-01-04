(use-modules (grand scheme))
(add-to-load-path "./")
(define pindolf (@ (pindolf interpreter) pindolf))
(define parsed (@ (pindolf parser) parsed))
(define compiled (@ (pindolf compiler) compiled))
(define run (@ (pindolf vm) run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pinp
  (with-input-from-file "pindolf-self-interpreter.sexp" read))

(define lip (with-input-from-file "lisp0.sexp" read))
(e.g. (pindolf '(example) lip) ===> 24)

(define drc (with-input-from-file "DRC-machine.sexp" read))
(e.g. (pindolf '(run-test) drc) ===> (q w e a s d))

(define l2d (with-input-from-file "lisp2drc.sexp" read))
(e.g. (pindolf '(compiled* car) l2d) ===> ((CAR)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lisp-test
  '((def ! (lambda (n)
             (if (eq? n 0)
                 1
                 (* n (! (- n 1))))))
    (def !s (lambda (ns)
              (if (eq? ns ())
                  ()
                  (cons (! (car ns))
                        (!s (cdr ns))))))
    (def iot (lambda (n)
               (if (eq? n 0)
                   ()
                   (cons n
                         (iot (- n 1))))))
    (!s (iot 6))))

(e.g. (pindolf `(run ,lisp-test) lip)
      ===> (720 120 24 6 2 1))

(e.g. (run (compiled lip) `(run ,lisp-test))
      ===> (720 120 24 6 2 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(e.g.(pindolf `(compiled ,lisp-test) l2d)
      ===> ((PROC ((NAME n)
                   (CONST 0)
                   (LOOKUP n)
                   (EQ?)
                   (SELECT ((CONST 1))
                           ((CONST 1)
                            (LOOKUP n)
                            (MINUS)
                            (LOOKUP !)
                            (APPLY)
                            (LOOKUP n)
                            (TIMES)))
                   (FORGET n)))
            (NAME !)
            (PROC ((NAME ns)
                   (CONST ())
                   (LOOKUP ns)
                   (EQ?)
                   (SELECT ((CONST ()))
                           ((LOOKUP ns)
                            (CDR)
                            (LOOKUP !s)
                            (APPLY)
                            (LOOKUP ns)
                            (CAR)
                            (LOOKUP !)
                            (APPLY)
                            (CONS)))
                   (FORGET ns)))
            (NAME !s)
            (PROC ((NAME n)
                   (CONST 0)
                   (LOOKUP n)
                   (EQ?)
                   (SELECT ((CONST ()))
                           ((CONST 1)
                            (LOOKUP n)
                            (MINUS)
                            (LOOKUP iot)
                            (APPLY)
                            (LOOKUP n)
                            (CONS)))
                   (FORGET n)))
            (NAME iot)
            (CONST 6)
            (LOOKUP iot)
            (APPLY)
            (LOOKUP !s)
            (APPLY)))

(e.g. (equal? (pindolf `(compiled ,lisp-test) l2d)
              (run (compiled l2d) `(compiled ,lisp-test))))

(e.g. (pindolf `(run-drc ,(pindolf `(compiled ,lisp-test) l2d) ())
               drc) ===> (720 120 24 6 2 1))

(e.g. (run (compiled drc)
           `(run-drc ,(pindolf `(compiled ,lisp-test) l2d) ()))
      ===> (720 120 24 6 2 1))
;;; sweet.


(e.g. (equal?
       (pindolf `(pindolf (compiled ,lisp-test) ,(parsed l2d))
                pinp)
       (run (compiled pinp)
            `(pindolf (compiled ,lisp-test) ,(parsed l2d)))))
;;; takes 15-20s...

(e.g. (pindolf `(pindolf (run-drc ,(pindolf `(compiled ,lisp-test) l2d)
                                  ())
                         ,(parsed drc))
               pinp) ===> (720 120 24 6 2 1))

