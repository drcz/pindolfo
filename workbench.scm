(use-modules (grand scheme))
(add-to-load-path "./")
(define pindolf (@ (pindolf interpreter) pindolf))
(define parsed (@ (pindolf parser) parsed))
(define compiled (@ (pindolf compiler) compiled))
(define run (@ (pindolf vm) run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pinp ;; all of the sudden: PINDOLF in PINDOLF!!!
  (parsed '(
;; --- matcher -------------------------------------------------
(('match? ?pattern ?expression)
 (& (match? ,pattern ,expression ())))

(('match? () () ?bnd) bnd)
(('match? 'T 'T ?bnd) bnd)
(('match? '_  _ ?bnd) bnd)
(('match? %n %n ?bnd) bnd)
(('match? ('quote ?e) ?e ?bnd) bnd) ; maybe $e???
(('match? ('num $nv)  %e ?bnd) (& (match-var ,nv ,e ,bnd)))
(('match? ('num _ )    _   _ ) 'NO-MATCH)
(('match? ('sym $sv)  $e ?bnd) (& (match-var ,sv ,e ,bnd)))
(('match? ('sym _ )    _   _ ) 'NO-MATCH)
(('match? ('atm $av)  @e ?bnd) (& (match-var ,av ,e ,bnd)))
(('match? ('atm _ )    _   _ ) 'NO-MATCH)
(('match? ('exp $ev)  ?e ?bnd) (& (match-var ,ev ,e ,bnd)))
(('match? (?p . ?ps) (?e . ?es) ?bnd)
 (& (match?* ,(& (match? ,p ,e ,bnd)) ,ps ,es)))
(('match? _ _ _) 'NO-MATCH)

(('match?* 'NO-MATCH _ _) 'NO-MATCH)
(('match?* ?bnd ?p ?e) (& (match? ,p ,e ,bnd)))

(('match-var $v ?e ?bnd)
 (& (match-var* ,(& (lookup ,v ,bnd)) ,v ,e ,bnd)))

(('match-var* ('NONE)    $v ?e ?bnd) `((,v . ,e) . ,bnd))
(('match-var* ('JUST ?e) $v ?e ?bnd) bnd)
(('match-var* ('JUST _)   _  _   _ ) 'NO-MATCH)

;; --- bindings ------------------------------------------------
(('lookup $k (($k . ?v) .   _ )) `(JUST ,v))
(('lookup $k (( _ . _ ) . ?bnd)) (& (lookup ,k ,bnd)))
(('lookup  _ ()                ) '(NONE))

;; --- evaluator -----------------------------------------------
(('value () _ _) ())
(('value T  _ _) 'T) ;; can be unquoted, no?
(('value %n _ _) n)

(('value ('quote ?e) _ _) e) ;; again, could be $e hmm?
(('value ('quasiquote ?qq) ?bnd ?app) (& (val-qq ,qq ,bnd ,app)))

(('value ('+ ?e ?e*) ?bnd ?app) (+ (& (value ,e ,bnd ,app))
                                   (& (value ,e* ,bnd ,app))))
(('value ('- ?e ?e*) ?bnd ?app) (- (& (value ,e ,bnd ,app))
                                   (& (value ,e* ,bnd ,app))))

(('value ('& ?a) ?bnd ?app) (& (,app ,(& (val-qq ,a ,bnd ,app)))))

(('value $k ?bnd _) (& (val-lo ,k ,(& (lookup ,k ,bnd)))))
(('val-lo $k   ('NONE) ) `(unbound ,k))
(('val-lo  _ ('JUST ?v)) v)

(('val-qq ('unquote ?e) ?bnd ?app) (& (value ,e ,bnd ,app)))
(('val-qq (?e . ?es)    ?bnd ?app) `(,(& (val-qq ,e ,bnd ,app))
                                     . ,(& (val-qq ,es ,bnd ,app))))
(('val-qq ?e _ _) e)

;; --- main interpreter ----------------------------------------
(('pindolf ?e ?prg) (& (pindolf ,e ,prg ,prg)))

(('pindolf ?e () _) `(NO-MATCH ,e))
(('pindolf ?e ((?pat ?exp) . ?prg*) ?prg)
 (& (pindolf* ,e ,(& (match? ,pat ,e)) ,exp ,prg* ,prg)))

(('pindolf* ?e 'NO-MATCH  _  ?prg* ?prg) (& (pindolf ,e ,prg* ,prg)))
(('pindolf*  _    ?bnd   ?e*   _   ?prg) (& (value ,e* ,bnd (app ,prg))))

((('app ?prg) ?e) (& (pindolf ,e ,prg)))
;; ------------------------------------------------------------
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; look!

(define t3
  (parsed '(
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

;; ...ok let's see if that new stuff would prevent me from sin
(e.g. (pindolf '(abc yolo)
               '((('abc ?x) (& (qwe ,x)))
                 (('qwe ?x $x) `(,x . ,y))))
      ===> (PARSE ERROR! in cluase 1 
                  (inconsistent types for x (sym exp))
                  (unbound variable y)))
;;; ok
(e.g. (pindolf '(abc yolo)
               '((('abc ?x) (& (qwe ,x)))
                 (('qwe ?x ?x) `(,x . ,y))))
      ===> (PARSE ERROR! in cluase 1 (unbound variable y)))
;;; good!
(e.g. (pindolf '(abc yolo)
               '((('abc ?x) (& (qwe ,x)))
                 (('qwe ?x ?y) `(,x . ,y))))
      ===> (NO MATCH for (qwe yolo)))
;;; nice.

