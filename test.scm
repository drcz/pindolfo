(use-modules (grand scheme))
(add-to-load-path "./")
(define pindolf (@ (pindolf interpreter) pindolf))
(define parsed (@ (pindolf parser) parsed))
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

(define p4 (with-input-from-file "DRC-machine.sexp" read))

(e.g. (pindolf '(run-test) p4) ===> (q w e a s d))
(e.g. (pindolf '(run-test2 (it does) (work indeed)) p4)
      ===> (it does work indeed))

(e.g. (run (compiled p4) '(run-test)) ===> (q w e a s d))
(e.g. (run (compiled p4) '(run-test2 (it does) (work indeed)))
      ===> (it does work indeed))

;;; todo: actually lisp (drcz0) compiled to DRC might be funny


;;;;;;; hasiok/trash ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (maybe these tests will be useful once more, who knows)

;;; testing one procedure at a time:
(e.g. (pindolf '(cat (q w e) (1 2 3) id) p4)
      ===> (q w e 1 2 3))
(e.g. (pindolf '(name J 23 ((ans . 42)) id) p4)
      ===> ((J . 23) (ans . 42)))
(e.g. (pindolf '(forget ans ((J . 23) (ans . 42)) id) p4)
      ===> ((J . 23)))
(e.g. (pindolf '(forget J ((J . 23) (ans . 42)) id) p4)
      ===> ((ans . 42)))
(e.g. (pindolf '(lookup J ((J . 23) (ans . 42)) id) p4)
      ===> 23)
(e.g. (pindolf '(lookup ans ((J . 23) (ans . 42)) id) p4)
      ===> 42)
;;; ok, more like testing one functionality at a time...
(e.g. (pindolf '(run-drc ((CONST 23) (CONST 42) (CONS)) ()) p4)
      ===> (42 . 23))
(e.g. (pindolf '(run-drc ((CONS)) (23 42)) p4)
      ===> (23 . 42))
(e.g. (pindolf '(run-drc ((NAME ans)
                          (LOOKUP ans)
                          (LOOKUP ans)
                          (CONS))
                         (42)) p4) ===> (42 . 42))
(e.g. (pindolf '(run-drc ((NAME ans)
                          (LOOKUP ans)
                          (LOOKUP ans)
                          (EQ?))
                         (42)) p4) ===> T)
(e.g. (pindolf '(run-drc ((NAME ans)
                          (LOOKUP ans)
                          (LOOKUP ans)
                          (LOOKUP ans)
                          (CONS)
                          (EQ?))
                         (42)) p4) ===> ())
(e.g. (pindolf '(run-drc ((NAME a)
                          (LOOKUP a)
                          (FORGET a))
                         (42)) p4) ===> 42)
(e.g. (pindolf '(run-drc ((PROC ((NAME a)
                                 (LOOKUP a)
                                 (LOOKUP a)
                                 (CONS)
                                 (FORGET a)))
                          (APPLY))
                         (he)) p4) ===> (he . he))

(e.g. (pindolf '(run-drc ((PROC ((NAME a)
                                 (LOOKUP a)
                                 (LOOKUP a)
                                 (CONS)
                                 (FORGET a)))
                          (NAME dbl)
                          (LOOKUP dbl)
                          (APPLY))
                         (hi)) p4) ===> (hi . hi))

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
                          (APPLY)) ((q w e) (a s d))) p4)
      ===> (q w e a s d))


(pretty-print `(tests for p4 (DRC machine) passed))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; and just a little more hardcode: pindolf in pindolf

(define pinp ;; all of the sudden: PINDOLF in PINDOLF!!!
  (with-input-from-file "pindolf-self-interpreter.sexp" read))

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
(pretty-print '\o/)
