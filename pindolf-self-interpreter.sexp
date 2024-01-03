(
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
(('value 'T _ _) T)
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
)