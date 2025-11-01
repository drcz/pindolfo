(
; ------------------------------------------------------------------
; abstract machine DRC from my b.sc thesis, in CPS
; (stolen&redacted from CONSpiracy repo)

#;( -- yup, cats are the bosses -- )
(('cat    ()      ?ys ?K) (& (,K ,ys)))
(('cat (?x . ?xs) ?ys ?K) (& (cat ,xs ,ys (catK ,x ,K))))
((('catK ?x ?k) ?v) (& (,k (,x . ,v))))

#;( -- operations on dictionary (the D register) -- )
(('name $k ?v ?dict ?K) (& (,K ((,k . ,v) . ,dict))))

(('forget $k (($k . _) . ?dict) ?K) (& (,K ,dict)))
(('forget $k (   ?kv   . ?dict) ?K) (& (forget ,k ,dict (forgetK ,kv ,K))))
((('forgetK ?kv ?K) ?x) (& (,K (,kv . ,x))))

(('lookup $k (($k . ?v) . _) ?K) (& (,K ,v)))
(('lookup $k (    _ . ?dict) ?K) (& (lookup ,k ,dict ,K)))

#;( -- the machine proper starts here -- )
(('run-drc ?program ?inputs) (& (=>* () ,inputs ,program)))

#;(- STORE, or operating on D -)
(('=>* ?D (?e . ?R) (  ('NAME $n) . ?C)) (& (name ,n ,e ,D (=>*DK ,R ,C))))
(('=>* ?D    ?R     (('FORGET $n) . ?C)) (& (forget ,n ,D (=>*DK ,R ,C))))
(('=>* ?D    ?R     (('LOOKUP $n) . ?C)) (& (lookup ,n ,D (=>*RK ,D ,R ,C))))

#;(- ...plus its three little continuations -)
((('=>*DK ?R ?C) ?D) (& (=>* ,D ,R ,C)))
((('=>*CK ?R ?D) ?C) (& (=>* ,D ,R ,C)))
((('=>*RK ?D ?R ?C) ?r) (& (=>* ,D (,r . ,R) ,C)))

#;(- CONSTANTS AND OPS, or operating on R -)
(('=>* ?D ?R (('CONST ?e) . ?C)) (& (=>* ,D (,e . ,R) ,C)))
(('=>* ?D ?R ( ('PROC ?p) . ?C)) (& (=>* ,D (,p . ,R) ,C)))

(('=>* ?D ( ?h ?t . ?R) (('CONS) . ?C)) (& (=>* ,D ((,h . ,t) . ,R) ,C)))

(('=>* ?D ((?h . ?t) . ?R) (('CAR) . ?C)) (& (=>* ,D (,h . ,R) ,C)))
(('=>* ?D ((?h . ?t) . ?R) (('CDR) . ?C)) (& (=>* ,D (,t . ,R) ,C)))

(('=>* ?D (?e ?e . ?R) (('EQ?) . ?C)) (& (=>* ,D (T  . ,R) ,C)))
(('=>* ?D ( _  _ . ?R) (('EQ?) . ?C)) (& (=>* ,D (() . ,R) ,C)))

(('=>* ?D ((?h . ?t) . ?R) (('ATOM?) . ?C)) (& (=>* ,D (() . ,R) ,C)))
(('=>* ?D (    _     . ?R) (('ATOM?) . ?C)) (& (=>* ,D (T  . ,R) ,C)))

(('=>* ?D ( %n . ?R) (('NUM?) . ?C)) (& (=>* ,D ( T . ,R) ,C)))
(('=>* ?D (  _ . ?R) (('NUM?) . ?C)) (& (=>* ,D (() . ,R) ,C)))

(('=>* ?D ( $s . ?R) (('SYM?) . ?C)) (& (=>* ,D ( T . ,R) ,C)))
(('=>* ?D (  _ . ?R) (('SYM?) . ?C)) (& (=>* ,D (() . ,R) ,C)))

(('=>* ?D ( ?n ?m . ?R) ( ('PLUS) . ?C)) (& (=>* ,D (,(+ n m) . ,R) ,C)))
(('=>* ?D ( ?n ?m . ?R) (('MINUS) . ?C)) (& (=>* ,D (,(- n m) . ,R) ,C)))
(('=>* ?D ( ?n ?m . ?R) (('TIMES) . ?C)) (& (=>* ,D (,(* n m) . ,R) ,C)))

#;(- CONTROL FLOW, or operating on C -)
(('=>* ?D (() . ?R) (('SELECT ?p ?p*) . ?C)) (& (cat ,p* ,C (=>*CK ,R ,D))))
(('=>* ?D (_  . ?R) (('SELECT ?p ?p*) . ?C)) (& (cat ,p ,C (=>*CK ,R ,D))))

(('=>* ?D (?p . ?R) (('APPLY) . ?C)) (& (cat ,p ,C (=>*CK ,R ,D))))

#;(- halting? not a problem! -)
(('=>* ?D (?e . ?R) ()) e) ;;; could require (?e) in R position actually...

#;(- that's all! -)

;; ------------------------------------------------------------------
#;(- an example program, ofc it concatenates two lists -)
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
(('run-test2 ?xs ?ys) (& (run-drc ,(& (mk-example)) (,xs ,ys))))
(('id ?x) x) ;; for more granular testing y'know

; ------------------------------------------------------------------
)
