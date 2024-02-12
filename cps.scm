;;; it's not even a good idea! \o/
(use-modules (grand scheme))
(add-to-load-path "./")
(define parsed (@ (pindolf parser) parsed))
(define pindolf (@ (pindolf interpreter) pindolf))
;;; the rest of stuff we'll just copy'n'paste since we alter it a bit

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
;;; since ,,the structure-building operations are invisible''
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


;;; ah! one more thing, quite disturbing: what if the patterns do not
;;; stick to the ,,prefix convention'', e.g. we could have (%m '+ %n)
;;; instead of ('+ %m %n)... lol
;;; since this is only a prototype we can ignore it for now, but
;;; if we ever seriously need to automagically convert to CPS, it'd
;;; better use some more abstract representation... we'll get there
;;; but indeed, after all pindol[f] is more low-level than languages
;;; with procedures, therefore CPS is a foreign idea to it.

;;; a nasty assumption: k is not bound in any pattern
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
;;; borrowing from the compiler then (primop? used tho):

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
;;; track of the things which should get enclosed??? they should fall
;;; into one the three categories only:
;;; (1) surely the continuation of the outermost clause, since we need
;;;     to eventually apply it.
;;; (2) result(s) of earlier application(s), since we need to use them,
;;;     but then stop passing them once they're used?
;;; (3) every variable boud by the outermost clause but not yet used.
;;; well, (1) is trivial but for (2) and (3) another example would help:
'( (('f ?x) (* x x))
   (('g ?x) (+ x 1))
   (('h ?x ?y) (+ (& (f ,x)) (& (f ,(& (g ,y)))))) )
;;; soo
(e.g. (applications-in '(+ (& (f ,x)) (& (f ,(& (g ,y))))))
      ===> (#;A0 (& (f ,x))
            #;A1 (& (g ,y))
            #;A2 (& (f ,(& (g ,y))))) )
;;; which would turn into something like
'((res0 . (& (f ,x))) ;; keep y and k
  (res1 . (& (g ,y))) ;; x no longer needed, BUT keep res0 and k
  (res2 . (& (f ,res1)))) ;;; y and res1 no longer needed, keep res0 and k
;;; with outermost (+ res0 res2), right. one more hand-written CPS:
'( (('fK ?x ?k) (& (,k ,(* x x))))
   (('gK ?x ?k) (& (,k ,(+ x 1))))
   (('hK ?x ?y ?k) (& (fK ,x (hK* ,y ,k))))
   ((('hK* ?y ?k) ?res0) (& (gK ,y (hK** ,res0 ,k))))
   ((('hK** ?res0 ?k) ?res1) (& (fK ,res1 (hK*** ,res0 ,k))))
   ((('hK*** ?res0 ?k) ?res2) (& (,k ,(+ res0 res2)))) )
;;; like that?
(e.g.
 (pindolf '(hK 2 3 id)
          '( (('fK ?x ?k) (& (,k ,(* x x))))
             (('gK ?x ?k) (& (,k ,(+ x 1))))
             (('hK ?x ?y ?k) (& (fK ,x (hK* ,y ,k))))
             ((('hK* ?y ?k) ?res0) (& (gK ,y (hK** ,res0 ,k))))
             ((('hK** ?res0 ?k) ?res1) (& (fK ,res1 (hK*** ,res0 ,k))))
             ((('hK*** ?res0 ?k) ?res2) (& (,k ,(+ res0 res2))))
             (('id ?x) x) )) ===> 20)
;;; ok. it all kind of makes sense, but how did i know that all?
;;; what do i have to see to figure out what's needed?
;;; or MAYBE we'll be fine keeping it all enclosed and just hoping
;;; that the partial evaluator will get rid of dead subexpressions?
;;; like this:
;;; res0: keep x,y,k
;;; res1: keep x,y,k,res0
;;; res2: keep x,y,k,res0, res1 ??? 
(e.g.
 (pindolf '(hK 2 3 id)
          '( (('fK ?x ?k) (& (,k ,(* x x))))
             (('gK ?x ?k) (& (,k ,(+ x 1))))
             (('hK ?x ?y ?k)
              #;--> (& (fK ,x (hK* ,x ,y ,k))))
             ((('hK* ?x ?y ?k) ?res0)
              #;--> (& (gK ,y (hK** ,x ,y ,k ,res0))))
             ((('hK** ?x ?y ?k ?res0) ?res1)
              #;--> (& (fK ,res1 (hK*** ,x ,y ,k ,res0 ,res1))))
             ((('hK*** ?x ?y ?k ?res0 ?res1) ?res2)
              #;--> (& (,k ,(+ res0 res2))))
             (('id ?x) x) )) ===> 20)
;;; hmmm, well at least that works and feels manageable.

;;; just for now all types will be ? (exp) and we won't check for
;;; unique names of generated labels m'kay? it's going to be a mess
;;; but just to figure out what it takes... let's do this!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; i'll repaste from the above just-in-case...

(define member? member) ;; he_he
(define (primop? x) (member? x '(+ - *)))

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

;;; surely also:
(define (lookup key #;in binding)
  (match binding
    (() #f)
    (((k . expr) . binding*) (if (equal? k key) expr
                              #;otherwise (lookup key binding*)))))

;;; now borrowing from parser (dropped massage):
(define (vartype? x) (member? x '(num sym atm exp)))

(define (vars-in-pattern p)
  (match p
    (((? vartype?) v) `(,v))
    (('quote _) '())
    ((p . ps) (union (vars-in-pattern p) (vars-in-pattern ps)))
    (_ '())))


;;; yeah let's make it pretty since otherwise it can blow the interpreter
(define (mk-label (l id))
  (string->symbol (string-append (symbol->string l)
                                 (number->string id))))
(e.g. (mk-label '(res 0)) ===> res0)
(e.g. (mk-label '(KONT 5)) ===> KONT5)

;;; ...and from compiler again (applications injected and 'res thing):
(define (application->res-map applications)
  (map (lambda (app index) `(,app . ,(mk-label `(res ,index))))
       applications (iota (length applications))))

;;; aaand now! replacing applications with res-vars INSIDE applications:
(define (reduced-subexpression expr #;wrt res-for-apps)
  (define (reduced-qq qq)
    (match qq
      (() '())
      (('unquote e) ;; nb mindfuck ahead
       `(,'unquote ,(reduced-subexpression e res-for-apps)))
      ((q . q*) `(,(reduced-qq q) . ,(reduced-qq q*)))
      (e e))) ;; sure?
  (let rdcd ((expr expr))
    (match expr
      (() '())
      ('T 'T)
      ((? symbol? sym) sym)
      ((? number? num) num)
      (('quote e) expr)
      (('quasiquote qq) (reduced-qq qq))
      (((? primop? p) e e*) `(,p ,(rdcd e) ,(rdcd e*)))
      (('& e) (or (lookup expr res-for-apps) ;;; !!!!!!
                  `(& ,(reduced-qq e))))
      #;err??)))

(e.g. (let* ((expr '(+ (& (f ,x)) (& (f ,(& (g ,y))))))
             (apps (applications-in expr))
             (res4apps (application->res-map apps)))        
        (reduced-subexpression expr res4apps))
      ===> (+ res0 res2))
;;; only we'll have to build new res4apps for consecutive members
;;; of apps with just ones above it, and in the end the source expr
;;; like above. got it.

(e.g. (let* ((expr '(+ (& (f ,x)) (& (f ,(& (g ,y))))))
             (apps (applications-in expr))
             (res4apps (application->res-map apps)))
        (reduced-subexpression (third apps) (take res4apps 2)))
      ===> (& (f ,res1)))
;;; yeeesssss!

(define (expression2steps expression)
  (let* ((applications (applications-in expression))
         (res4apps (application->res-map applications)))
    (let loop ((steps '())
               (todo `(,@applications ,expression)))
    (match todo
      (() steps)
      ((e . todo*) (let* ((index (length steps)) ;; :D
                          (res4as (take res4apps index))
                          (my-res (mk-label `(res ,index))) ;;; he_he
                          (my-kont (mk-label `(KONT ,index)))
                          (steps* `(,@steps
                                    (,my-res
                                     ,my-kont
                                    ,(reduced-subexpression e res4as)))))
                     (loop steps* todo*)))))))

(e.g. (expression2steps '(+ (& (f ,x)) (& (f ,(& (g ,y))))))
      ===> ((res0 KONT0 (& (f ,x)))
            (res1 KONT1 (& (g ,y)))
            (res2 KONT2 (& (f ,res1)))
            (res3 KONT3 (+ res0 res2))))
;;; and this should turn into:
;; * <massaged pattern> (& (f ,x (KONT0 <pattern vars w/ k>))) 
;; * (('KONT0 <pvwk>) ?res0) (& (g ,y (KONT1 <pvwk> ,res0)))
;; * (('KONT1 <pvwk> ?res0) ?res1) (& (f ,res1 (KONT2 <pv> ,res0 ,res1)))
;; * (('KONT2 <pv> ?res0 ?res1) ?res2) (& (,k ,(+ res0 res1)))

(define (cps-step e kont)
  (match e
    (('& a) `(& (,@a ,kont)))
    (e `(& (,kont (,'unquote ,e))))))

(e.g. (cps-step '(& (f ,x)) 'k)
      ===> (& (f ,x k)))
(e.g. (cps-step '(+ (res 0) (res 2)) 'k)
      ===> (& (k ,(+ (res 0) (res 2)))))

(define (cps-pattern p kont) ;;; ,,for now''...
  `(,@p (exp ,kont)))

(e.g. (cps-pattern '('h (exp x) (exp y)) 'k)
      ===> ('h (exp x) (exp y) (exp k)))

(define (vars2pat vs) (map (lambda (v) `(exp ,v)) vs))
  (e.g. (vars2pat '(x y k res0 res1))
        ===> ((exp x) (exp y) (exp k) (exp res0) (exp res1)))

(define (vars2exp vs) (map (lambda (v) `(,'unquote ,v)) vs))
  (e.g. (vars2exp '(x y k res0 res1))
        ===> (,x ,y ,k ,res0 ,res1))

;;; looks quite ok? so now...

(define (cps clause kont) ;; ensure its massaged (parsed)!
  (let* (((pattern expression) clause)
         (vars (vars-in-pattern pattern))
         (pattern* (cps-pattern pattern kont))
         (steps (expression2steps expression))
         (last-kont kont))
    (let loop ((todo steps)
               (done '())
               (pat pattern*)
               (keeping `(,@vars ,kont)))
      (match todo
        (() done)
        (((res kont step) . todo*)
         (let* ((kont* (if (= 0 (length todo*))
                           last-kont
                           `(,kont ,@(vars2exp keeping))))
                (step* (cps-step step kont*))
                (pat* `((',kont ,@(vars2pat keeping)) (exp ,res)))
                (keeping* `(,@keeping ,res))
                (done* `(,@done (,pat ,step*))))
           (loop todo* done* pat* keeping*)))))))

(e.g.
 (equal?
  (cps '(('h (exp x) (exp y)) (+ (& (f ,x)) (& (f ,(& (g ,y)))))) 'K)
  (parsed
   '( (('h ?x ?y ?K)
       #;--> (& (f ,x (KONT0 ,y ,x ,K))))
      ((('KONT0 ?y ?x ?K) ?res0)
       #;--> (& (g ,y (KONT1 ,y ,x ,K ,res0))))
      ((('KONT1 ?y ?x ?K ?res0) ?res1)
       #;--> (& (f ,res1 (KONT2 ,y ,x ,K ,res0 ,res1))))
      ((('KONT2 ?y ?x ?K ?res0 ?res1) ?res2)
       #;--> (& (K ,(+ res0 res2)))) ))))

;;; wooooooooow

        
