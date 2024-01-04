(
; ------------------------------------------------------------
;;; tiny (dynamically-scoped) lisp in pindolf...

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
(('apply ('* %n %m) _) (* n m))
(('apply (('lambda ?vs ?e) . ?as) ?env)
 (& (eval ,e ,(& (apd ,(& (pair ,vs ,as)) ,env)))))

; ------------------------------------------------------------
(('init-env ?self-evs) (& (pair ,self-evs ,self-evs)))

(('run ?program) (& (run ,program
                         ,(& (init-env (T cons car cdr + - *
                                        eq? num? sym? atm?))))))

(('run () _) 'the-end?!) ;; should not happen...

(('run (('def $s ?e) . ?prg) ?top)
 (& (run ,prg ,(& (updated ,s ,(& (eval ,e ,top)) ,top)))))

(('run (?form) ?top) (& (eval ,form ,top)))

; ------------------------------------------------------------
(('example) (& (run ((def sq (lambda (n) (* n n)))
                     (def ! (lambda (n)
                              (if (eq? n 0)
                                  1
                                  (* n (! (- n 1))))))
                     (! (sq 2))))))
; ------------------------------------------------------------
)