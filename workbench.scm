(use-modules (grand scheme))
(add-to-load-path "./")
(define pindolf (@ (pindolf interpreter) pindolf))
(define parsed (@ (pindolf parser) parsed))
(define compiled (@ (pindolf compiler) compiled))
(define run (@ (pindolf vm) run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tiny (dynamically-scoped) lisp in pindolf.

(define d0
  '(
; ------------------------------------------------------------
(('apd    ()      ?ys) ys)
(('apd (?x . ?xs) ?ys) `(,x . ,(& (apd ,xs ,ys))))

(('pair () ()) ())
(('pair (?x . ?xs) (?y . ?ys)) `((,x . ,y) . ,(& (pair ,xs ,ys))))
; ------------------------------------------------------------
(('lookup $k (($k . ?v) .   _ )) v)
(('lookup $k (( _ . _ ) . ?env)) (& (lookup ,k ,env)))

(('updated $k ?v (($k . _) . ?env)) `((,k . ,v) . ,env))
(('updated $k ?v (   ?kv   . ?env)) `(,kv . ,(& (updated ,k ,v ,env))))
(('updated $k ?v ()) `((,k . ,v)))
; ------------------------------------------------------------
(('mul  0  _) 0)
(('mul  1 %n) n)
(('mul %m %n) (+ n (& (mul ,(- m 1) ,n))))
; ------------------------------------------------------------
(('eval () _) ())
(('eval %n _) n)
(('eval $s ?env) (& (lookup ,s ,env)))
(('eval ('quote ?e) _) e)
(('eval ('if ?p ?c ?a) ?env) (& (ev-if ,(& (eval ,p ,env)) ,c ,a ,env)))
(('eval ('lambda ?vs ?e) ?env) `(lambda ,vs ,e)) ;; nb no closures!
(('eval ?ap ?env) (& (apply ,(& (evlis ,ap ,env)) ,env)))

(('ev-if ()  _ ?a ?env) (& (eval ,a ,env)))
(('ev-if _  ?c  _ ?env) (& (eval ,c ,env)))

(('evlis () ?env) ())
(('evlis (?e . ?es) ?env) `(,(& (eval ,e ,env)) . ,(& (evlis ,es ,env))))

(('apply ('cons ?h ?t)     _) `(,h . ,t))
(('apply ('car (?h . _ )) _) h)
(('apply ('cdr ( _ . ?t)) _) t)
(('apply ('eq? ?x ?x) _)  T)
(('apply ('eq?  _ _ ) _) ())
(('apply ('num? %n) _)  T)
(('apply ('num?  _) _) ())
(('apply ('sym? $s) _)  T)
(('apply ('sym?  _) _) ())
(('apply ('atm? @a) _)  T)
(('apply ('atm?  _) _) ())
(('apply ('+ %n %m) _) (+ n m))
(('apply ('- %n %m) _) (- n m))
(('apply ('* %n %m) _) (& (mul ,n ,m)))
(('apply (('lambda ?vs ?e) . ?as) ?env)
 (& (eval ,e ,(& (apd ,(& (pair ,vs ,as)) ,env)))))

; ------------------------------------------------------------
(('init-env ?self-evs) (& (pair ,self-evs ,self-evs)))

(('run ?program) (& (run ,program
                         ,(& (init-env (T cons car cdr + - *
                                        eq? num? sym? atm?))))))
(('run () _)
 'the-end?!) ;; should not happen...
(('run (('def $s ?e) . ?prg) ?top)
 (& (run ,prg ,(& (updated ,s ,(& (eval ,e ,top)) ,top)))))
(('run (?form) ?top)
 (& (eval ,form ,top)))
; ------------------------------------------------------------
))

(e.g. (pindolf '(run ((+ (* 7 8) 3))) d0) ===> 59)
(e.g. (pindolf '(run ((def sq (lambda (x) (* x x)))
                      (sq (+ 2 3)))) d0) ===> 25)
(e.g. (pindolf '(run ((def ! (lambda (n) (if (eq? n 0) 1 (* n (! (- n 1))))))
                      (def x (+ 2 3))
                      (! x))) d0) ===> 120)

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
               d0) ===> (720 120 24 6 2 1))


(e.g. (run (compiled d0)
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

;;; ...ok, and now pindolf running pindolf running lisp omg
(define pinp (with-input-from-file "pindolf-self-interpreter.sexp" read))

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
         ,(parsed d0))
       pinp) ===> (720 120 24 6 2 1))

;;; whoa! didn't even expect it to work!

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
             ,(parsed d0)))
      ===> (6 2 1))
;;; the above takes ~1.5min hehe.
;;; btw notice there's no multiplication in pindolf, so it acutally
;;; uses Peano/Dedekind recursive formula on each step!
