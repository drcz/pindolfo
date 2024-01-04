(
; ------------------------------------------------------------
; lisp0 -> DRC compiler in pindolf
; ------------------------------------------------------------

(('rev ?xs) (& (rev ,xs ())))
(('rev    ()      ?rs) rs)
(('rev (?x . ?xs) ?rs) (& (rev ,xs (,x . ,rs))))

(('apd    ()      ?ys) ys)
(('apd (?x . ?xs) ?ys) `(,x . ,(& (apd ,xs ,ys))))

(('map*apd ?prc    ()     ) '())
(('map*apd ?prc (?x . ?xs)) (& (apd ,(& (,prc ,x))
                                    ,(& (map*apd ,prc ,xs)))))

; ------------------------------------------------------------
(('compiled* ()) '((CONST ())))
(('compiled* 'T) '((CONST T)))
(('compiled* %n) `((CONST ,n)))
(('compiled* 'car) '((CAR)))
(('compiled* 'cdr) '((CDR)))
(('compiled* 'cons) '((CONS)))
(('compiled* 'eq?) '((EQ?)))
(('compiled* 'num?) '((NUM?)))
(('compiled* 'sym?) '((SYM?)))
(('compiled* 'atm?) '((ATOM?)))
(('compiled* '+) '((PLUS)))
(('compiled* '-) '((MINUS)))
(('compiled* '*) '((TIMES)))

(('compiled* $sym) `((LOOKUP ,sym)))

(('compiled* ('quote ?e)) `((CONST ,e)))

(('compiled* ('if ?p ?c ?a))
 (& (apd ,(& (compiled* ,p))
         ((SELECT ,(& (compiled* ,c))
                  ,(& (compiled* ,a)))))))

(('compiled* ('lambda ?vs ?body))
 `((PROC ,(& (apd ,(& (name-block ,vs))
                  ,(& (apd ,(& (compiled* ,body))
                           ,(& (forget-block ,vs)))))))))

(('compiled* ('def $v ?e))
 (& (apd ,(& (compiled* ,e)) ((NAME ,v)))))

(('compiled* (?rator . ?rands))
 (& (apd ,(& (map*apd compiled* ,(& (rev (,rator . ,rands)))))
         ,(& (maybe-apply ,rator)))))

(('name-block ()) ())
(('name-block ($n . ?ns)) `((NAME ,n)
                            . ,(& (name-block ,ns))))

(('forget-block ()) ())
(('forget-block ($n . ?ns)) `((FORGET ,n)
                              . ,(& (forget-block ,ns))))

(('maybe-apply 'car ) '())
(('maybe-apply 'cdr ) '())
(('maybe-apply 'cons) '())
(('maybe-apply 'eq? ) '())
(('maybe-apply 'num?) '())
(('maybe-apply 'sym?) '())
(('maybe-apply 'atm?) '())
(('maybe-apply '+   ) '())
(('maybe-apply '-   ) '())
(('maybe-apply '*   ) '())
(('maybe-apply _) '((APPLY)))

(('compiled ?program) (& (map*apd compiled* ,program)))

; ------------------------------------------------------------
)
