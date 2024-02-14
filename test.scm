(use-modules (grand scheme))
(add-to-load-path "./")
(define parsed (@ (pindolf parser) parsed))
(define pindolf (@ (pindolf interpreter) pindolf))
(define cpsized (@ (pindolf transformations) cpsized))
(define compiled (@ (pindolf compiler) compiled))
(define run (@ (pindolf vm) run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define p1
  '( (('APD () (exp ys)) ys)
     (('APD ((exp x) . (exp xs)) (exp ys)) `(,x . ,(& (APD ,xs ,ys))))

     (('MUL 0 _) 0)
     (('MUL 1 (num x)) x)
     (('MUL (num x) (num y)) (+ y (& (MUL ,(- x 1) ,y))))

     (('REV (exp xs)) (& (REV ,xs ())))
     (('REV () (exp rs)) rs)
     (('REV ((exp x) . (exp xs)) (exp rs)) (& (REV ,xs (,x . ,rs))))
     ))
     
(e.g. (pindolf '(APD (q w e) (a s d)) p1) ===> (q w e a s d))
(e.g. (pindolf '(MUL 3 5) p1) ===> 15)
(e.g. (pindolf '(REV (dercz likes pindolf)) p1)
      ===> (pindolf likes dercz))

(e.g. (run (compiled p1) '(APD (q w e) (a s d))) ===> (q w e a s d))
(e.g. (run (compiled p1) '(MUL 3 5)) ===> 15)
(e.g. (run (compiled p1) '(REV (dercz likes pindolf)))
      ===> (pindolf likes dercz))

(pretty-print `(tests for p1 passed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define p2
  '(
(('SUBSTRING (exp a) (exp b)) (& (SUB ,a ,b ,a ,b)))

(('SUB () (exp b) (exp oryg-a) (exp oryg-b))
 'FOUND!)
(('SUB ((sym e) . (exp a*)) (exp b) (exp oryg-a) (exp oryg-b))
 (& (TRY ,e ,a* ,b ,oryg-a ,oryg-b)))

(('TRY (sym e) (exp a) () (exp oryg-a) (exp oryg-b))
 'NOT-FOUND!)
(('TRY (sym e) (exp a) ((sym e) . (exp b)) (exp oryg-a) (exp oryg-b))
 (& (SUB ,a ,b ,oryg-a ,oryg-b)))
(('TRY (sym e) (exp a) (_ . (exp b)) (exp oryg-a) (exp oryg-b))
 (& (NEXT ,oryg-a ,oryg-b)))

(('NEXT (exp a) (_ . (exp b))) (& (SUB ,a ,b ,a ,b)))
(('NEXT (exp a)      ()      ) 'NOT-FOUND!)
))

(e.g. (pindolf '(SUBSTRING (k u l a) (m a k u l a t u r a)) p2)
      ===> FOUND!)
(e.g. (pindolf '(SUBSTRING (k u r a) (m a k u l a t u r a)) p2)
      ===> NOT-FOUND!)

(e.g. (run (compiled p2) '(SUBSTRING (k u l a) (m a k u l a t u r a)))
      ===> FOUND!)
(e.g. (run (compiled p2) '(SUBSTRING (k u r a) (m a k u l a t u r a)))
      ===> NOT-FOUND!)

(pretty-print `(tests for p2 passed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define p3
'( ;; playing with defunctionalized higher-order-procedures

(('fold-r (exp op) (exp e) ()) e)
(('fold-r (exp op) (exp e) ((exp x) . (exp xs)))
 (& (,op ,x ,(& (fold-r ,op ,e ,xs)))))

(('cons (exp h) (exp t)) `(,h . ,t))
(('apd (exp xs) (exp ys)) (& (fold-r cons ,ys ,xs)))

((('cons*f.hd (exp f)) (exp h) (exp t)) (& (cons ,(& (,f ,h)) ,t)))
(('map (exp f) (exp xs)) (& (fold-r (cons*f.hd ,f) () ,xs)))

(('dup (exp x)) `(,x . ,x))
(('dbl (num n)) (+ n n))

(('rev (exp xs)) (& (rev ,xs ())))
(('rev () (exp rs)) rs)
(('rev ((exp x) . (exp xs)) (exp rs)) (& (rev ,xs (,x . ,rs))))

))

(e.g. (pindolf '(apd (q w e) (a s d)) p3) ===> (q w e a s d))
(e.g. (pindolf '(map dbl (1 2 3)) p3) ===> (2 4 6))
(e.g. (pindolf '(map dup (- 0 ^)) p3) ===> ((- . -) (0 . 0) (^ . ^)))
(e.g. (pindolf '(rev (dercz likes pindolf)) p3)
      ===> (pindolf likes dercz))

(e.g. (run (compiled p3) '(apd (q w e) (a s d))) ===> (q w e a s d))
(e.g. (run (compiled p3) '(map dbl (1 2 3))) ===> (2 4 6))
(e.g. (run (compiled p3) '(map dup (- 0 ^)))
      ===> ((- . -) (0 . 0) (^ . ^)))
(e.g. (run (compiled p3) '(rev (dercz likes pindolf)))
      ===> (pindolf likes dercz))

(pretty-print `(tests for p3 passed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define t23
  '(
    (('apd () (exp ys)) ys)
    (('apd ((sym x) . (exp xs)) (exp ys)) `(,x . ,(& (apd ,xs ,ys))))
    (('gen-xs) `(p n d l f))
    (('test) (& (apd ,(& (gen-xs)) ,(& (gen-xs)))))
    (('test2) `(>> ,(& (gen-xs)) <<))))

(e.g. (pindolf '(test) t23) ===> (p n d l f p n d l f))
(e.g. (pindolf '(test2) t23) ===> (>> (p n d l f) <<))

(e.g. (run (compiled t23) '(test)) ===> (p n d l f p n d l f))
(e.g. (run (compiled t23) '(test2)) ===> (>> (p n d l f) <<))

(pretty-print `(tests for t23 passed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; now for some real hardcode, DRC machine...

(define drc
  (with-input-from-file "examples/DRC-machine.pndlf" read))

(e.g. (pindolf '(run-test) drc) ===> (q w e a s d))
(e.g. (pindolf '(run-test2 (it does) (work indeed)) drc)
      ===> (it does work indeed))

(e.g. (run (compiled drc) '(run-test)) ===> (q w e a s d))
(e.g. (run (compiled drc) '(run-test2 (it does) (work indeed)))
      ===> (it does work indeed))

;;; also cf lisp (drcz0) compiled to DRC a few tests below...

;;;;;;; hasiok/trash ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; (maybe these tests will be useful once more, who knows)

;;; testing one procedure at a time:
(e.g. (pindolf '(cat (q w e) (1 2 3) id) drc)
      ===> (q w e 1 2 3))
(e.g. (pindolf '(name J 23 ((ans . 42)) id) drc)
      ===> ((J . 23) (ans . 42)))
(e.g. (pindolf '(forget ans ((J . 23) (ans . 42)) id) drc)
      ===> ((J . 23)))
(e.g. (pindolf '(forget J ((J . 23) (ans . 42)) id) drc)
      ===> ((ans . 42)))
(e.g. (pindolf '(lookup J ((J . 23) (ans . 42)) id) drc)
      ===> 23)
(e.g. (pindolf '(lookup ans ((J . 23) (ans . 42)) id) drc)
      ===> 42)
;;; ok, more like testing one functionality at a time...
(e.g. (pindolf '(run-drc ((CONST 23) (CONST 42) (CONS)) ()) drc)
      ===> (42 . 23))
(e.g. (pindolf '(run-drc ((CONS)) (23 42)) drc)
      ===> (23 . 42))
(e.g. (pindolf '(run-drc ((NAME ans)
                          (LOOKUP ans)
                          (LOOKUP ans)
                          (CONS))
                         (42)) drc) ===> (42 . 42))
(e.g. (pindolf '(run-drc ((NAME ans)
                          (LOOKUP ans)
                          (LOOKUP ans)
                          (EQ?))
                         (42)) drc) ===> T)
(e.g. (pindolf '(run-drc ((NAME ans)
                          (LOOKUP ans)
                          (LOOKUP ans)
                          (LOOKUP ans)
                          (CONS)
                          (EQ?))
                         (42)) drc) ===> ())
(e.g. (pindolf '(run-drc ((NAME a)
                          (LOOKUP a)
                          (FORGET a))
                         (42)) drc) ===> 42)
(e.g. (pindolf '(run-drc ((PROC ((NAME a)
                                 (LOOKUP a)
                                 (LOOKUP a)
                                 (CONS)
                                 (FORGET a)))
                          (APPLY))
                         (he)) drc) ===> (he . he))

(e.g. (pindolf '(run-drc ((PROC ((NAME a)
                                 (LOOKUP a)
                                 (LOOKUP a)
                                 (CONS)
                                 (FORGET a)))
                          (NAME dbl)
                          (LOOKUP dbl)
                          (APPLY))
                         (hi)) drc) ===> (hi . hi))

;;; finally! silly errors in silly old repos lol.
(e.g. (pindolf '(run-drc ((PROC ((NAME xs)
                                 (NAME ys)
                                 (LOOKUP xs)
                                 (CONST ())
                                 (EQ?)
                                 (SELECT ((LOOKUP ys))
                                         ((LOOKUP ys)
                                          (LOOKUP xs)
                                          (CDR)
                                          (LOOKUP apd)
                                          (APPLY)
                                          (LOOKUP xs)
                                          (CAR)
                                          (CONS)))
                                 (FORGET ys)
                                 (FORGET xs)))
                          (NAME apd)
                          (LOOKUP apd)
                          (APPLY)) ((q w e) (a s d))) drc)
      ===> (q w e a s d))


(pretty-print `(tests for DRC machine passed))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; and just a little more hardcode: pindolf in pindolf

(define pinp ;; all of the sudden: PINDOLF in PINDOLF!!!
  (with-input-from-file
      "examples/pindolf-self-interpreter.pndlf" read))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; look!

(define t3 (parsed '(
;; ------------------------------------------------------------
(('fold-r ?op ?e    ()     ) e)
(('fold-r ?op ?e (?x . ?xs)) (& (,op ,x ,(& (fold-r ,op ,e ,xs)))))

(('cons ?h ?t) `(,h . ,t))
(('apd ?xs ?ys) (& (fold-r cons ,ys ,xs)))

((('cons*f.hd ?f) ?h ?t) (& (cons ,(& (,f ,h)) ,t)))
(('map ?f ?xs) (& (fold-r (cons*f.hd ,f) () ,xs)))

(('dup ?x) `(,x . ,x))
(('dbl %n) (+ n n))

(('rev ?xs) (& (rev ,xs ())))
(('rev    ()      ?rs) rs)
(('rev (?x . ?xs) ?rs) (& (rev ,xs (,x . ,rs))))
;; ------------------------------------------------------------
)))

(e.g. (pindolf `(pindolf (apd (q w e) (a s d)) ,t3) pinp)
      ===> (q w e a s d))

(e.g. (pindolf `(pindolf (map dbl (1 2 3)) ,t3) pinp)
      ===> (2 4 6))

(e.g. (pindolf `(pindolf (map dup (- 0 ^)) ,t3) pinp)
      ===> ((- . -) (0 . 0) (^ . ^)))

(e.g. (pindolf `(pindolf (rev (dercz likes pindolf)) ,t3) pinp)
      ===> (pindolf likes dercz))

(e.g. (run (compiled pinp) `(pindolf (apd (q w e) (a s d)) ,t3))
      ===> (q w e a s d))

(e.g. (run (compiled pinp) `(pindolf (map dbl (1 2 3)) ,t3))
      ===> (2 4 6))

(e.g. (run (compiled pinp) `(pindolf (map dup (- 0 ^)) ,t3))
      ===> ((- . -) (0 . 0) (^ . ^)))

(e.g. (run (compiled pinp)  `(pindolf (rev (dercz likes pindolf)) ,t3))
      ===> (pindolf likes dercz))
;;; how cool is that?!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; + tons of tests which got us there...

(e.g. (pindolf '(match? () ()) pinp) ===> ())
(e.g. (pindolf '(match? _ (hi)) pinp) ===> ())
(e.g. (pindolf '(match? 2 2) pinp) ===> ())
(e.g. (pindolf '(match? 2 3) pinp) ===> NO-MATCH)
(e.g. (pindolf '(match? (num n) 23) pinp) ===> ((n . 23)))
(e.g. (pindolf '(match? ((sym x) (num a) (num b)) (j 2 3)) pinp)
      ===> ((b . 3) (a . 2) (x . j)))
(e.g. (pindolf '(match? ('match?! (sym x) 997) (match?! hi! 997)) pinp)
      ===> ((x . hi!)))
(e.g. (pindolf '(match? ('match?! (exp x) 997) (match?! hi! 997)) pinp)
      ===> ((x . hi!)))

(e.g. (pindolf '(value `(hi ,(+ 2 3)) () ()) pinp) ===> (hi 5))
(e.g. (pindolf '(value x ((x . 23)) ()) pinp) ===> 23)
(e.g. (pindolf '(value (+ (- 9 6) n) ((n . 2)) ()) pinp) ===> 5)

(e.g. (pindolf '(pindolf (hi! 23)
                         (( ('hi! (exp x)) `(,x . ,x) ))) pinp)
      ===> (23 . 23))

(e.g. (pindolf '(pindolf (wow!)
                         (( ('hi! (exp x)) `(,x . ,x) )
                          ( ('wow!) (& (hi! 42))))) pinp)
      ===> (42 . 42))

(e.g. (pindolf '(pindolf (apd (q w e) (1 2 3))
                         ( (('apd () (exp ys)) ys)
                           (('apd ((exp x) . (exp xs)) (exp ys))
                            `(,x . ,(& (apd ,xs ,ys)))) ))
               pinp)
      ===> (q w e 1 2 3)) ;;; yes!

(pretty-print `(tests for pinp (pindolf in pindolf) passed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lip (with-input-from-file "examples/lisp0.pndlf" read))

(e.g. (pindolf '(example) lip) ===> 24)

(e.g. (pindolf '(run ((def ! (lambda (n) (if (eq? n 0) 1 (* n (! (- n 1))))))
                      (def !s (lambda (ns)
                                (if (eq? ns ())
                                    ()
                                    (cons (! (car ns)) (!s (cdr ns))))))
                      (def iot (lambda (n)
                                 (if (eq? n 0)
                                     ()
                                     (cons n (iot (- n 1))))))
                      (!s (iot 6))))
               lip) ===> (720 120 24 6 2 1))

(e.g. (run (compiled lip)
           '(run ((def ! (lambda (n) (if (eq? n 0) 1 (* n (! (- n 1))))))
                  (def !s (lambda (ns)
                            (if (eq? ns ())
                                ()
                                (cons (! (car ns)) (!s (cdr ns))))))
                  (def iot (lambda (n)
                             (if (eq? n 0)
                                 ()
                                 (cons n (iot (- n 1))))))
                  (!s (iot 6)))))
      ===> (720 120 24 6 2 1))

(pretty-print `(tests for lip (LISP in pindolf) passed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e.g. (pindolf
       `(pindolf 
         (run-drc ((PROC ((NAME xs)
                          (NAME ys)
                          (LOOKUP xs)
                          (CONST ())
                          (EQ?)
                          (SELECT ((LOOKUP ys))
                                  ((LOOKUP ys)
                                   (LOOKUP xs)
                                   (CDR)
                                   (LOOKUP apd)
                                   (APPLY)
                                   (LOOKUP xs)
                                   (CAR)
                                   (CONS)))
                          (FORGET ys)
                          (FORGET xs)))
                   (NAME apd)
                   (LOOKUP apd)
                   (APPLY)) ((q w e) (a s d)))
       ,(parsed drc))
       pinp) ===> (q w e a s d))

(e.g. (run (compiled pinp)
           `(pindolf 
             (run-drc ((PROC ((NAME xs)
                              (NAME ys)
                              (LOOKUP xs)
                              (CONST ())
                              (EQ?)
                              (SELECT ((LOOKUP ys))
                                      ((LOOKUP ys)
                                       (LOOKUP xs)
                                       (CDR)
                                       (LOOKUP apd)
                                       (APPLY)
                                       (LOOKUP xs)
                                       (CAR)
                                       (CONS)))
                              (FORGET ys)
                              (FORGET xs)))
                       (NAME apd)
                       (LOOKUP apd)
                       (APPLY)) ((q w e) (a s d)))
             ,(parsed drc)))
      ===> (q w e a s d))
;;; ok just to make sure you're following: the above is guile scheme
;;; running FCL* interpreter running compiled pindolf interpreting
;;; [another] pindolf interpreting DRC machine running apd program.
;;; such language towers (towels???) make sense since they reveal
;;; pretty non-trivial bugs.

(pretty-print `(tests for DRC in pinp (!!) passed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(e.g. (pindolf
       `(pindolf 
         (run ((def ! (lambda (n)
                        (if (eq? n 0)
                            1
                            (* n (! (- n 1))))))
               (def !s (lambda (ns)
                         (if (eq? ns ())
                             ()
                             (cons (! (car ns)) (!s (cdr ns))))))
               (def iot (lambda (n)
                          (if (eq? n 0)
                              ()
                              (cons n (iot (- n 1))))))
               (!s (iot 6))))
         ,(parsed lip))
       pinp) ===> (720 120 24 6 2 1))

(e.g. (run (compiled pinp)
           `(pindolf 
             (run ((def ! (lambda (n)
                            (if (eq? n 0)
                                1
                                (* n (! (- n 1))))))
                   (def !s (lambda (ns)
                             (if (eq? ns ())
                                 ()
                                 (cons (! (car ns)) (!s (cdr ns))))))
                   (def iot (lambda (n)
                              (if (eq? n 0)
                                  ()
                                  (cons n (iot (- n 1))))))
                   (!s (iot 3))))
             ,(parsed lip)))
      ===> (6 2 1))
;;; ok, again: guile's running FCL* running compiled pindolf, interpreting
;;; [another] pindolf, interpreting LISP interpreter running some program.
;;; yes, it takes ~1.5min.
(pretty-print `(test for lip in pinp (!!!) passed))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l2d (with-input-from-file "examples/lisp2DRC.pndlf" read))
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
    (!s (iot 4))))

(e.g. (pindolf `(run ,lisp-test) lip)
      ===> (24 6 2 1))

(e.g. (run (compiled lip) `(run ,lisp-test))
      ===> (24 6 2 1))

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
            (CONST 4)
            (LOOKUP iot)
            (APPLY)
            (LOOKUP !s)
            (APPLY)))

(e.g. (equal? (pindolf `(compiled ,lisp-test) l2d)
              (run (compiled l2d) `(compiled ,lisp-test))))

(e.g. (pindolf `(run-drc ,(pindolf `(compiled ,lisp-test) l2d) ())
               drc) ===> (24 6 2 1))

(e.g. (run (compiled drc)
           `(run-drc ,(pindolf `(compiled ,lisp-test) l2d) ()))
      ===> (24 6 2 1))
;;; sweet.


(e.g. (equal?
       (pindolf `(pindolf (compiled ,lisp-test) ,(parsed l2d))
                pinp)
       (run (compiled pinp)
            `(pindolf (compiled ,lisp-test) ,(parsed l2d)))))
;;; takes a few seconds...

(e.g. (pindolf `(pindolf (run-drc ,(pindolf `(compiled ,lisp-test) l2d)
                                  ())
                         ,(parsed drc))
               pinp) ===> (24 6 2 1))

(e.g. (run (compiled pinp)
           `(pindolf (run-drc ,(pindolf `(compiled ,lisp-test) l2d)
                              ())
                     ,(parsed drc)))
      ===> (24 6 2 1))
;;; takes ~2min

(write `(tests for lisp->DRC and its execution, in pinp (!!) and compiled pinp (!!!) passed))
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pretty-print `(testing cps-like transformation!))

(define p3-cps (cpsized p3)) ;; parsed?!

(e.g. (pindolf '(apd (q w e) (a s d) _id_) p3-cps)
      ===> (q w e a s d))

(e.g. (pindolf '(map dbl (1 2 3) _id_) p3-cps)
      ===> (2 4 6))

(e.g. (pindolf '(map dup (- 0 ^) _id_) p3-cps)
      ===> ((- . -) (0 . 0) (^ . ^)))

(e.g. (pindolf '(rev (dercz likes CPS) _id_) p3-cps)
      ===> (CPS likes dercz))

(define cmp-p3-cps (compiled p3-cps))

(e.g. (run cmp-p3-cps '(apd (q w e) (a s d) _id_))
      ===> (q w e a s d))
(e.g. (run cmp-p3-cps '(map dbl (1 2 3) _id_))
      ===> (2 4 6))
(e.g. (run cmp-p3-cps '(map dup (- 0 ^) _id_))
      ===> ((- . -) (0 . 0) (^ . ^)))
(e.g. (run cmp-p3-cps '(rev (dercz likes cps) _id_))
      ===> (cps likes dercz))

(pretty-print `(tests for cpsized p3 passed))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cps-pinp (cpsized pinp)) ;; PINP in CPS form!
(define src p3) 

(e.g. (pindolf `(pindolf (apd (q w e) (a s d)) ,src _id_) cps-pinp)
      ===> (q w e a s d))

(e.g. (pindolf `(pindolf (map dbl (1 2 3)) ,src _id_) cps-pinp)
      ===> (2 4 6))

(e.g. (pindolf `(pindolf (map dup (- 0 ^)) ,src _id_) cps-pinp)
      ===> ((- . -) (0 . 0) (^ . ^)))

(e.g. (pindolf `(pindolf (rev (dercz likes CPS)) ,src _id_) cps-pinp)
      ===> (CPS likes dercz))

(pretty-print `(tests for cpsized pinp running p3 passed!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stupidly long computations

(define peano
  (parsed 
   (with-input-from-file "examples/peano.pndlf" read)))

(e.g. (pindolf '(test1) peano) ===> yes)

(e.g. (pindolf '((S S S Z) + (S S Z)) peano) ===> (S S S S S Z))
(e.g. (pindolf '((S S S Z) * (S S Z)) peano) ===> (S S S S S S Z))
(e.g. (pindolf '(! (Z)) peano) ===> (S Z))
(e.g. (pindolf '(! (S S S Z)) peano) ===> (S S S S S S Z))

(e.g. (pindolf '((S S Z) <= (S S S Z)) peano) ===> yes)
(e.g. (pindolf '((S S Z) <= (S S Z)) peano) ===> yes)
(e.g. (pindolf '((S S Z) <= (S Z)) peano) ===> no)

(e.g. (pindolf '((does (S S S S Z) = (S S Z) *) (S S Z))
               peano) ===> yes)
(e.g. (pindolf '((does (S S S S Z) = (S S S Z) *) (S Z))
               peano) ===> no)
(e.g. (pindolf '((S S Z) divides (S S S S Z)) peano) ===> yes)
(e.g. (pindolf '((S S Z) divides (S S S S S Z)) peano) ===> no)
(e.g. (pindolf '((is (S S S S Z) divisible by) (S S Z))
               peano) ===> yes)

(e.g. (pindolf '(not yes) peano) ===> no)
(e.g. (pindolf '(not no) peano) ===> yes)

(e.g. (pindolf '(pr* yes (S S S Z) (is (S S S Z) divisible by)) peano)
      ===> yes)

(e.g. (pindolf '(test2) peano) ===> yes)
(e.g. (pindolf '(test3) peano) ===> yes)
(e.g. (pindolf '(test4) peano) ===> no)


(e.g. (pindolf `(pindolf (test1) ,peano) pinp) ===> yes)
(e.g. (pindolf `(pindolf (test2) ,peano) pinp) ===> yes)
(e.g. (pindolf `(pindolf (test3) ,peano) pinp) ===> yes)
;;; super slooow
(e.g. (pindolf `(pindolf (test4) ,peano) pinp) ===> no)


(pretty-print `(tests for stupidly long computations passed!))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(newline) (display '\o/) (newline)
