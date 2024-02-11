;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -= a prototype for CPS-conversion of pindolf programs =-
;;; so that the VM could perform (GOTO 0) instead of (CALL ...)
;;; it's not even a good idea to do that, let alone do it this way!

(use-modules (grand scheme))
(add-to-load-path "./")
(define parsed (@ (pindolf parser) parsed))
(define pindolf (@ (pindolf interpreter) pindolf))

(define test1
  '(
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
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ok so we'd like something like
'( (('fold-r ?op ?e ()) e)
   (('fold-r ?op ?e (?x . ?xs)) (& (,op ,x ,(& (fold-r ,op ,e ,xs))))) )
;;; to turn into
'(  (('fold-rK ?op ?e () ?k)
     (& (,k ,e)))
    (('fold-rK ?op ?e (?x . ?xs) ?k)
     (& (fold-rK ,op ,e ,xs (fold-rK* ,k))))
    ((('fold-rK* ?k) ?res)
     (& (,op ,x ,res ,k))) )
;;; nb op will now be in cps form too, e.g.
'(  ('consK ?h ?t ?k) (& (,k (,h . ,t)))  )

;;; it gets a bit more tricky with multiple applications on rhs tho:
'((('f %x) (* x x))
  (('h %x %y) (+ (& (f ,x)) (& (f ,y)))))
;;; would have to turn into:
'((('fK %x ?k) (& (,k ,(* x x))))
  (('hK %x %y ?k) (& (fK ,x (hK* ,y ,k))))
  ((('hK* %y ?k) ?res) (& (fK ,y (hK** ,res ,k)))) ;; nb could be %res?
  ((('hK** ?res ?k) ?res*) (& (+K ,res ,res* ,k)))) ;; a.a. %res*
;;; with
'( (('+K %n %m ?k) (& (,k ,(+ n m))))
   (('*K %n %m ?k) (& (,k ,(* n m)))) )
;;; or sth? actually the implementation could already know these for prims
;;; just cons might be that thing, AND this is especially tricky now
;;; since we should have something like CPS quasiquote, no?!
;;; NOOOOO, it's much simpler, look!
'((('f %x) (* x x))
  (('h %x %y) `(+ ,(& (f ,x)) ,(& (f ,y)))))
;;; turns into:
'((('fK %x ?k) (& (,k ,(* x x))))
  (('hK %x %y ?k) (& (fK ,x (hK* ,y ,k))))
  ((('hK* %y ?k) ?res) (& (fK ,y (hK** ,res ,k))))
  ((('hK** ?res ?k) ?res*) (& (,k (+ ,res ,res*)))))
;;; the structure-building operations are invisible.

(e.g. 
 (pindolf '(hK 2 3 id)
          '((('fK %x ?k) (& (,k ,(* x x))))
            (('hK %x %y ?k) (& (fK ,x (hK* ,y ,k))))
            ((('hK* %y ?k) ?res) (& (fK ,y (hK** ,res ,k))))
            ((('hK** ?res ?k) ?res*) (& (,k (+ ,res ,res*))))
            (('id ?x) x))) ===> (+ 4 9)) ;; \o/

;;; the ,,type-checking'' of ?res and ?res* being %res and %res* is
;;; interesting to think about, BUT mind that at least for now that'd
;;; only produce `num?` test in the decision tree (FCL*). it's redundant
;;; since the types can't go wrong on the generated continuations...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ok some ugly conventions to get it running ; we might append 'K'
;;; to the names, and use stars Peano-style... ooor we could
;;; just convert `label` to `(label 0)`, `(label 1)`
;;; and similarly with res being (res 0) (res 1) etc. cool?

;;; ah! one more thing, quite disturbing: what if the patterns do not
;;; stick to the ,,prefix convention'', e.g. we could have (%m '+ %n)
;;; instead of ('+ %m %n)... lol
;;; since this is only a prototype we can ignore it for now, but
;;; if we ever seriously need to automagically convert to CPS, it'd
;;; better use some more abstract representation... we'll get there
;;; but indeed, after all pindol(f) is more low-level than languages
;;; with procedures, therefore CPS is a foreign idea to it.

;;; another nasty assumption: k is not bound in any pattern.
;;; one more reason to start thinking about abstract representation...

(define (cps-pattern p kont) ;;; ,,for now''...
  (match p
    ((label . es) `((,label 0) ,@es (exp ,kont)))
    ;; fails otherwise
    ))

(define var? symbol?)
(define (primop? x) (member x '(+ - *)))

;;; ok, deep breath...

;;; (+ n m) -> (& (KONT ,(+ n m))) easy.
;;; (+ n (* 2 m)) -> (& (KONT ,(+ n (* 2 m))))
;;; and _not_ -> (& ((KONT2 ,n KONT) ,(* 2 m))) with a new clause
;;; [ (('KONT2 ?n ?k) ?res) (& (,k (+ ,n ,res))) ] since we don't want
;;; that granularity, only slice/grow on applications:
;;; (& (f ,m)) -> (& (fK ,m KONT))
;;; (& (f ,(+ m n))) -> (& (fK ,(+ m n) KONT))
;;; and the first harder case:
;;; (+ n (& (f ,m))) -> (& (fK ,m (K2 ,n KONT)))
;;; _plus_ a new clause [ (('K2 ?n ?k) res) (& (,k ,(+ n res))) ]
;;; then ofc somewhere fK was already constructed, or will be soon.

;;; so it actually should consume clause and return n+1 ones, where n
;;; is the number of applications in the expression.
;;; borrowing from the compiler then:

(define (applications-in expression)
  (define (applications-in-qq qq)
    (match qq
      (('unquote e) (applications-in e))
      ((e . e*) (append (applications-in-qq e) (applications-in-qq e*)))
      (_ '())))
  (match expression
    (() '())
    ('T '())
    ((? symbol?) '())
    ((? number?) '())
    (('quote _) '())
    (('quasiquote qq) (applications-in-qq qq))
    (((? primop?) e e*) (append (applications-in e) (applications-in e*)))
    (('& e) (append (applications-in-qq e) `(,expression)))))

;;; right. and it seems we should be doing roughly what the compiler did:
;;; number the applications in their order insde-out (as the above),
;;; and form the continuation-glue between them
(e.g. (applications-in '(+ (& (f ,n)) (& (f ,m))))
      ===> (#;A0 (& (f ,n))
            #;A1 (& (f ,m))) )
;;; so then kont for A0 must enclose m and its kont k, while
;;; kont for A1 must enclose A0's result r0 and k, and then the outermost
;;; expression would be applying k to `(+ ,r0 ,m). now, how do we keep
;;; track of the things which should get enclosed???
