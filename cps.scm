;;; some attempt at CPS-conversion

(use-modules (grand scheme))
(add-to-load-path "./")
(define parsed (@ (pindolf parser) parsed))
(define pindolf (@ (pindolf interpreter) pindolf))
;;; the rest of stuff differs from compiler/parser versions a bit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define member? member) ;; he_he

(define (lookup key #;in binding)
  (match binding
    (() #f)
    (((k . expr) . binding*) (if (equal? k key) expr
                              #;otherwise (lookup key binding*)))))

(e.g. (lookup 'x '((x . y) ((complex key) . 23))) ===> y)
(e.g. (lookup '(complex key) '((x . y) ((complex key) . 23))) ===> 23)


;;; let's make it pretty since otherwise it can blow the interpreter
(define (mk-label (l id))
  (string->symbol (string-append (symbol->string l)
                                 (number->string id))))

(e.g. (mk-label '(res 0)) ===> res0)
(e.g. (mk-label '(KONT 5)) ===> KONT5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (primop? x) (member? x '(+ - *)))
(define (vartype? x) (member? x '(num sym atm exp)))

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

(e.g. (applications-in '(+ (& (f ,x)) (& (f ,(& (g ,y))))))
      ===> ( (& (f ,x))
             (& (g ,y))
             (& (f ,(& (g ,y)))) ))

(e.g. (applications-in '(& (apd ,(& (rev ,xs)) ,(& (rev ,ys)))))
      ===> ( (& (rev ,xs))
             (& (rev ,ys))
             (& (apd ,(& (rev ,xs)) ,(& (rev ,ys)))) ))

;;; just for convenience...
(define (application->res-map applications)
  (map (lambda (app index) `(,app . ,(mk-label `(res ,index))))
       applications (iota (length applications))))

(e.g. (application->res-map
       (applications-in '(& (apd ,(& (rev ,xs)) ,(& (rev ,ys))))))
      ===> (((& (rev ,xs)) . res0)
            ((& (rev ,ys)) . res1)
            ((& (apd ,(& (rev ,xs)) ,(& (rev ,ys)))) . res2)))

;;; aaand now! replacing applications with res-vars INSIDE applications:
(define (reduced-subexpression expr #;wrt res-for-apps)
  (define (reduced-qq qq)
    (match qq
      (() '())
      (('unquote e) `(,'unquote ,(reduced-subexpression e res-for-apps)))
      ((q . q*) `(,(reduced-qq q) . ,(reduced-qq q*)))
      (e e)))
  (let reduced ((expr expr))
    (match expr
      (() '())
      ('T 'T)
      ((? symbol? sym) sym)
      ((? number? num) num)
      (('quote e) expr)
      (('quasiquote qq) `(,'quasiquote ,(reduced-qq qq)))
      (((? primop? p) e e*) `(,p ,(reduced e) ,(reduced e*)))
      (('& e) (or (lookup expr res-for-apps) ;;; !!!
                  `(& ,(reduced-qq e))))
      (_ 'err???))))

(e.g. (let* ((expr '(+ (& (f ,x)) (& (f ,(& (g ,y))))))
             (apps (applications-in expr))
             (res4apps (application->res-map apps)))
        (reduced-subexpression expr res4apps))
      ===> (+ res0 res2))

(e.g. (let* ((expr '(+ (& (f ,x)) (& (f ,(& (g ,y))))))
             (apps (applications-in expr))
             (res4apps (application->res-map apps)))
        (reduced-subexpression (third apps) ;; that nested one...
                               (take res4apps 2)))
      ===> (& (f ,res1)))

;;; now splitting expression into steps (one step per application)
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

;;; and some magic, because there are 3 possibilities:
;; 1) the step is an application, should just pass the continuation,
;; 2) the step is primop application, it should pass its result to
;;    the continuation,
;; 3) the step is a structure-building expression (quote or quasi-quote)
;;    which is basically just like primop ap but with the twist of
;;    not escaping itself since we convert it to application of the
;;    continuation and now this description is longer than the examples
;;    below so just check them out.
(define (cps-step e kont)
  (match e
    (('& a) `(& (,@a ,kont)))
    (('quote e) `(& ((,'unquote ,kont) ,e)))
    (('quasiquote e) `(& ((,'unquote ,kont) ,e)))
    (e `(& ((,'unquote ,kont) (,'unquote ,e))))))

(e.g. (cps-step '(& (f ,x)) 'k)
      ===> (& (f ,x k)))
(e.g. (cps-step '(+ res0 res2) 'k)
      ===> (& (,k ,(+ res0 res2))))
(e.g. (cps-step '`(boom ,res0 ,res2) 'k)
      ===> (& (,k (boom ,res0 ,res2))))
(e.g. (cps-step ''(hi there!) 'k)
      ===> (& (,k (hi there!))))

;;; finally, the most REVOLUTIONARY discovery of this prototype:
;;; we can identify and represent pattern with itself!
;;; this way we avoid the initial problem of ,,what if the pattern does
;;; NOT stick to prefix convention'' eg instead of ('+ %n %m) if it's
;;; something like (%n + %m)... this should actually work now, only
;;; for that ingenious representation thing we have to generate some
;;; structures out of original pattern. namely:

;;; - a new pattern (just stick one more ,,argument'' like FP people do):
(define (cps-pattern p kont) `(,@p (exp ,kont)))

(e.g. (cps-pattern '('h (exp x) (exp y)) 'k)
      ===> ('h (exp x) (exp y) (exp k)))

(e.g. (cps-pattern '('value ('quasiquote (exp qq)) (exp bnd) (exp app))
                   'k)
      ===> ('value ('quasiquote (exp qq)) (exp bnd) (exp app) (exp k)))

;;; - an expression which will fit given pattern:
(define (pattern2passing pat)
  (match pat
    (() '())
    ('_ '_)    
    ((? number? n) n)
    (('quote 'quasiquote) ','quasiquote) ;;; LOL
    (('quote 'unquote) ','unquote) ;;; LOL
    (((? vartype?) v) `(,'unquote ,v))
    (('quote e) e)
    ((p . p*) `(,(pattern2passing p) . ,(pattern2passing p*)))
    #;(e e)))

(e.g. (pattern2passing '('h (exp x) (exp y)))
      ===> (h ,x ,y))
(e.g. (pattern2passing '('h (exp elo) ((num n) '+ (num m))))
      ===> (h ,elo (,n + ,m)))
(e.g.
 (pattern2passing '('value ('quasiquote (exp qq)) (exp bnd) (exp app)))
 ===> (value (,'quasiquote ,qq) ,bnd ,app))


;;; - accumulated ,,variables'' (keeping partial results on LHS):
(define (vars2pat vs) (map (lambda (v) `(exp ,v)) vs))

(e.g. (vars2pat '(x y k res0 res1))
      ===> ((exp x) (exp y) (exp k) (exp res0) (exp res1)))

;;; - accumulated ,,variables'' as an expression (as above but on RHS):
(define (vars2exp vs) (map (lambda (v) `(,'unquote ,v)) vs))

(e.g. (vars2exp '(x y k res0 res1)) ===> (,x ,y ,k ,res0 ,res1))


;;; ...and this should be enough to go with this damn thing, look:

(define (cps clause kont)
  (let* (((pattern expression) clause)
         (pattern* (cps-pattern pattern kont))
         (pat-pass (pattern2passing pattern)) ;; some better name...?
         (steps (expression2steps expression))
         (last-kont kont))
    (let loop ((todo steps)
               (done '())
               (pat pattern*)
               (keeping `(,kont)))
      (match todo
        (() done)
        (((res kont step) . todo*)
         (let* ((kont* (if (= 0 (length todo*))
                           last-kont
                           `(,kont ,pat-pass ,@(vars2exp keeping))))
                (step* (cps-step step kont*))
                (pat* `((',kont ,pattern ,@(vars2pat keeping))
                        (exp ,res)))
                (keeping* `(,@keeping ,res))
                (done* `(,@done (,pat ,step*))))
           (loop todo* done* pat* keeping*)))))))


(e.g.
 (equal?
  (cps '(('h (exp x) (exp y)) (+ (& (f ,x)) (& (f ,(& (g ,y)))))) 'K)
  (parsed
   '( (('h ?x ?y ?K)
       #;--> (& (f ,x (KONT0 (h ,x ,y) ,K))))
      ((('KONT0 ('h ?x ?y) ?K) ?res0)
       #;--> (& (g ,y (KONT1 (h ,x ,y) ,K ,res0))))
      ((('KONT1 ('h ?x ?y) ?K ?res0) ?res1)
       #;--> (& (f ,res1 (KONT2 (h ,x ,y) ,K ,res0 ,res1))))
      ((('KONT2 ('h ?x ?y) ?K ?res0 ?res1) ?res2)
       #;--> (& (,K ,(+ res0 res2)))) ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the final procedure, wipe your shoes:

(define (cpsized program)
  (let* ((last-clause '(('_id_ (exp x)) x)) ;; TODO check for uniqueness!
         (outermost-kont 'K))
    (apply append
           `(,@(map (lambda (c) (cps c outermost-kont)) (parsed program))
             (,last-clause)))))

(e.g.
 (equal?
  (cpsized
   '( (('apd () ?ys) ys)
      (('apd (?x . ?xs) ?ys) `(,x . ,(& (apd ,xs ,ys)))) ))
  (parsed
   '( (('apd () ?ys ?K)
       #;--> (& (,K ,ys)))
      (('apd (?x . ?xs) ?ys ?K)
       #;--> (& (apd ,xs ,ys (KONT0 (apd (,x . ,xs) ,ys) ,K))))
      ((('KONT0 ('apd (?x . ?xs) ?ys) ?K) ?res0)
       #;--> (& (,K (,x . ,res0))))
      (('_id_ ?x) x) ))))

;;; and it actually works!

(e.g. 
 (pindolf '(apd (q w e) (a s d))
          '( (('apd () ?ys) ys)
             (('apd (?x . ?xs) ?ys) `(,x . ,(& (apd ,xs ,ys)))) ))
 ===> (q w e a s d))

(e.g. 
 (pindolf '(apd (q w e) (a s d) _id_)
          (cpsized 
           '( (('apd () ?ys) ys)
              (('apd (?x . ?xs) ?ys) `(,x . ,(& (apd ,xs ,ys)))) )))
 ===> (q w e a s d))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define larger-example
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

(define large-cps (cpsized larger-example))

(e.g. (pindolf '(apd (q w e) (a s d) _id_) large-cps)
      ===> (q w e a s d))

(e.g. (pindolf '(map dbl (1 2 3) _id_) large-cps)
      ===> (2 4 6))

(e.g. (pindolf '(map dup (- 0 ^) _id_) large-cps)
      ===> ((- . -) (0 . 0) (^ . ^)))

(e.g. (pindolf '(rev (dercz likes CPS) _id_) large-cps)
      ===> (CPS likes dercz))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; even more hardcore test, one level deeper:

(define pinp ;; pindolf-in-pindolf...
  (with-input-from-file "pindolf-self-interpreter.sexp" read))
(define cps-pinp (cpsized pinp)) ;; ...in CPS form...
(define src (parsed larger-example)) ;; ...with some stuff to do

(e.g. (pindolf `(pindolf (apd (q w e) (a s d)) ,src _id_) cps-pinp)
      ===> (q w e a s d))

(e.g. (pindolf `(pindolf (map dbl (1 2 3)) ,src _id_) cps-pinp)
      ===> (2 4 6))

(e.g. (pindolf `(pindolf (map dup (- 0 ^)) ,src _id_) cps-pinp)
      ===> ((- . -) (0 . 0) (^ . ^)))

(e.g. (pindolf `(pindolf (rev (dercz likes CPS)) ,src _id_) cps-pinp)
      ===> (CPS likes dercz))

;;; that is truly amazing, time to go to bed tho.
