(use-modules (grand scheme))
(add-to-load-path "./")
(define pindolf (@ (pindolf interpreter) pindolf))
(define compiled (@ (pindolf compiler) compiled))
(define run (@ (pindolf vm) run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define p1
  '( (('APD () ys) ys)
     (('APD (x . xs) ys) `(,x . ,(& (APD ,xs ,ys))))

     (('MUL 0 x) 0)
     (('MUL 1 x) x)
     (('MUL x y) (+ y (& (MUL ,(- x 1) ,y))))

     (('REV xs) (& (REV ,xs ())))
     (('REV () rs) rs)
     (('REV (x . xs) rs) (& (REV ,xs (,x . ,rs))))
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
(('SUBSTRING a b) (& (SUB ,a ,b ,a ,b)))

(('SUB       () b oryg-a oryg-b) 'FOUND!)
(('SUB (e . a*) b oryg-a oryg-b) (& (TRY ,e ,a* ,b ,oryg-a ,oryg-b)))

(('TRY e a      () oryg-a oryg-b) 'NOT-FOUND!)
(('TRY e a (e . b) oryg-a oryg-b) (& (SUB ,a ,b ,oryg-a ,oryg-b)))
(('TRY e a (_ . b) oryg-a oryg-b) (& (NEXT ,oryg-a ,oryg-b)))

(('NEXT a (_ . b)) (& (SUB ,a ,b ,a ,b)))
(('NEXT a      ()) 'NOT-FOUND!)
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

(('fold-r op e       ()) e)
(('fold-r op e (x . xs)) (& (,op ,x ,(& (fold-r ,op ,e ,xs)))))

(('cons h t) `(,h . ,t))
(('apd xs ys) (& (fold-r cons ,ys ,xs)))

((('cons*f.hd f) h t) (& (cons ,(& (,f ,h)) ,t)))
(('map f xs) (& (fold-r (cons*f.hd ,f) () ,xs)))

(('dup x) `(,x . ,x))
(('dbl n) (+ n n))
))

(e.g. (pindolf '(apd (q w e) (a s d)) p3) ===> (q w e a s d))
(e.g. (pindolf '(map dbl (1 2 3)) p3) ===> (2 4 6))
(e.g. (pindolf '(map dup (- 0 ^)) p3) ===> ((- . -) (0 . 0) (^ . ^)))

(e.g. (run (compiled p3) '(apd (q w e) (a s d))) ===> (q w e a s d))
(e.g. (run (compiled p3) '(map dbl (1 2 3))) ===> (2 4 6))
(e.g. (run (compiled p3) '(map dup (- 0 ^)))
      ===> ((- . -) (0 . 0) (^ . ^)))

(pretty-print `(tests for p3 passed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define t23
  '(
    (('apd () ys) ys)
    (('apd (x . xs) ys) `(,x . ,(& (apd ,xs ,ys))))
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

(define p4
  '(
;;; abstract machine from my b.sc thesis,
;;; in CPS (stolen&redacted from CONSpiracy repo)

(('id x) x) ;; for testing only

#;( -- yup, cats are the bosses -- )
(('cat ()       ys K) (& (,K ,ys)))
(('cat (x . xs) ys K) (& (cat ,xs ,ys (catK ,x ,K))))
((('catK x k) v) (& (,k (,x . ,v))))

#;( -- operations on dictionary (the D register) -- )
(('name k v dict K) (& (,K ((,k . ,v) . ,dict))))

(('forget k ((k . _) . dict) K) (& (,K ,dict)))
(('forget k (     kv . dict) K) (& (forget ,k ,dict (forgetK ,kv ,K))))
((('forgetK kv K) x) (& (,K (,kv . ,x))))

(('lookup k ((k . v) . _) K) (& (,K ,v)))
(('lookup k (_ . dict)    K) (& (lookup ,k ,dict ,K)))

#;( -- the machine proper starts here -- )
(('run-drc program inputs) (& (=>* () ,inputs ,program)))

#;(- STORE, or operating on D -)
(('=>* D (e . R) (('NAME n)   . C)) (& (name ,n ,e ,D (=>*DK ,R ,C))))
(('=>* D    R    (('FORGET n) . C)) (& (forget ,n ,D (=>*DK ,R ,C))))
(('=>* D    R    (('LOOKUP n) . C)) (& (lookup ,n ,D (=>*RK ,D ,R ,C))))

#;(- ...plus its three little continuations -)
((('=>*DK R C) D) (& (=>* ,D ,R ,C)))
((('=>*CK R D) C) (& (=>* ,D ,R ,C)))
((('=>*RK D R C) r) (& (=>* ,D (,r . ,R) ,C)))

#;(- CONSTANTS AND OPS, or operating on R -)
(('=>* D R (('CONST e) . C)) (& (=>* ,D (,e . ,R) ,C)))
(('=>* D R (('PROC p)  . C)) (& (=>* ,D (,p . ,R) ,C)))

(('=>* D (h t . R)     (('CONS) . C)) (& (=>* ,D ((,h . ,t) . ,R) ,C)))
(('=>* D ((h . t) . R) (('CAR) . C)) (& (=>* ,D (,h . ,R) ,C)))
(('=>* D ((h . t) . R) (('CDR) . C)) (& (=>* ,D (,t . ,R) ,C)))

(('=>* D (e e . R)     (('EQ?) . C)) (& (=>* ,D (T  . ,R) ,C)))
(('=>* D (_ _ . R)     (('EQ?) . C)) (& (=>* ,D (() . ,R) ,C)))
(('=>* D ((h . t) . R) (('ATOM?) . C)) (& (=>* ,D (() . ,R) ,C)))
(('=>* D (_       . R) (('ATOM?) . C)) (& (=>* ,D (T  . ,R) ,C)))

#;(- CONTROL FLOW, or operating on C -)
(('=>* D (() . R) (('SELECT p p*) . C)) (& (cat ,p* ,C (=>*CK ,R ,D))))
(('=>* D (_  . R) (('SELECT p p*) . C)) (& (cat ,p ,C (=>*CK ,R ,D))))
(('=>* D (p . R) (('APPLY) . C)) (& (cat ,p ,C (=>*CK ,R ,D))))

#;(- halting? not a problem! -)
(('=>* D (e . R) ()) e) ;;; could require (e) in R position actually?

#;(- example program, ofc it concatenates two lists -)
(('mk-example) `((PROC ((NAME xs)
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
                 (APPLY)))

(('run-test) (& (run-drc ,(& (mk-example)) ((q w e) (a s d)))))
(('run-test2 xs ys) (& (run-drc ,(& (mk-example)) (,xs ,ys))))

))

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


(pretty-print `(tests for p4 passed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pretty-print '\o/)
