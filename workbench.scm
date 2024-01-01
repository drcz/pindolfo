(use-modules (grand scheme))
(add-to-load-path "./")
(define pindolf (@ (pindolf interpreter) pindolf))
;(define og:value (@ (pindolf interpreter) value))
;(define og:matching? (@ (pindolf interpreter) matching?))
;(define lookup (@ (pindolf interpreter) lookup))
(define compiled (@ (pindolf compiler) compiled))
(define run (@ (pindolf vm) run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pinp ;; all of the sudden: PINDOLF in PINDOLF!!!
  '(
;; --- matcher -------------------------------------------------
(('match? (exp pattern) (exp expression))
 (& (match? ,pattern ,expression ())))

(('match?    ()     ()    (exp bnd)) bnd)
(('match?    'T     'T    (exp bnd)) bnd)
(('match?    '_     _     (exp bnd)) bnd)
(('match? (num n) (num n) (exp bnd)) bnd)
(('match? ('quote (exp e)) (exp e) (exp bnd)) bnd)
(('match? ('num (sym nv)) (num e) (exp bnd)) (& (match-var ,nv ,e ,bnd)))
(('match? ('num _ )          _       _     ) 'NO-MATCH)
(('match? ('sym (sym sv)) (sym e) (exp bnd)) (& (match-var ,sv ,e ,bnd)))
(('match? ('sym _ )          _        _    ) 'NO-MATCH)
(('match? ('atm (sym av)) (atm e) (exp bnd)) (& (match-var ,av ,e ,bnd)))
(('match? ('atm _ )          _        _    ) 'NO-MATCH)
(('match? ('exp (sym ev)) (exp e) (exp bnd)) (& (match-var ,ev ,e ,bnd)))
(('match? ((exp p) . (exp ps))
          ((exp e) . (exp es))
          (exp bnd)) (& (match?* ,(& (match? ,p ,e ,bnd)) ,ps ,es)))
(('match? _ _ _) 'NO-MATCH)

(('match?* 'NO-MATCH _ _) 'NO-MATCH)
(('match?* (exp bnd) (exp p) (exp e)) (& (match? ,p ,e ,bnd)))

(('match-var (sym v) (exp e) (exp bnd))
 (& (match-var* ,(& (lookup ,v ,bnd)) ,v ,e ,bnd)))

(('match-var* ('NONE) (exp v) (exp e) (exp bnd)) `((,v . ,e) . ,bnd))
(('match-var* ('JUST (exp e*)) (sym v) (exp e*) (exp bnd)) bnd)
(('match-var* ('JUST _) _ _ _) 'NO-MATCH)

;; --- bindings ------------------------------------------------
(('lookup (exp k) (((exp k) . (exp v)) . _)) `(JUST ,v))
(('lookup (exp k) (_ . (exp bnd))) (& (lookup ,k ,bnd)))
(('lookup _ ()) '(NONE))

;; --- evaluator -----------------------------------------------
(('value ()      _ _) ())
(('value T       _ _) 'T)
(('value (num n) _ _) n)

(('value ('quote (exp e)) _ _) e)
(('value ('quasiquote (exp qq)) (exp bnd) (exp app))
 (& (val-qq ,qq ,bnd ,app)))

(('value ('+ (exp e) (exp e*)) (exp bnd) (exp app))
 (+ (& (value ,e ,bnd ,app)) (& (value ,e* ,bnd ,app))))
(('value ('- (exp e) (exp e*)) (exp bnd) (exp app))
 (- (& (value ,e ,bnd ,app)) (& (value ,e* ,bnd ,app))))

(('value ('& (exp a)) (exp bnd) (exp app))
 (& (,app ,(& (val-qq ,a ,bnd ,app)))))

(('value (exp k) (exp bnd) _) (& (val-lo ,k ,(& (lookup ,k ,bnd)))))
(('val-lo (exp k) ('NONE)) `(unbound ,k))
(('val-lo _ ('JUST (exp v))) v)

(('val-qq ('unquote (exp e)) (exp bnd) (exp app))
 (& (value ,e ,bnd ,app)))
(('val-qq ((exp e) . (exp es)) (exp bnd) (exp app))
 `(,(& (val-qq ,e ,bnd ,app)) . ,(& (val-qq ,es ,bnd ,app))))
(('val-qq (exp e) _ _) e)

;; --- main interpreter ----------------------------------------
(('pindolf (exp e) (exp prg)) (& (pindolf ,e ,prg ,prg)))
(('pindolf (exp e) () _) `(NO-MATCH ,e))
(('pindolf (exp e) (((exp p) (exp e*)) . (exp prg*)) (exp prg))
 (& (pindolf* ,e ,(& (match? ,p ,e)) ,e* ,prg* ,prg)))

(('pindolf* (exp e) 'NO-MATCH _ (exp prg*) (exp prg))
 (& (pindolf ,e ,prg* ,prg)))

(('pindolf* _ (exp bnd) (exp e*) _ (exp prg))
 (& (value ,e* ,bnd (app ,prg))))

((('app (exp prg)) (exp e)) (& (pindolf ,e ,prg)))

))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; look!

(define t3
'(
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

