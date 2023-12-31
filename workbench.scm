(use-modules (grand scheme))
(add-to-load-path "./")
(define og:pindolf (@ (pindolf interpreter) pindolf))
(define og:value (@ (pindolf interpreter) value))
(define og:matching? (@ (pindolf interpreter) matching?))
(define lookup (@ (pindolf interpreter) lookup))
(define compiled (@ (pindolf compiler) compiled))
(define run (@ (pindolf vm) run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define t3
'( ;; it's a nice test rly (or should we import tests? naaah)

(('fold-r op e       ()) e)
(('fold-r op e (x . xs)) (& (,op ,x ,(& (fold-r ,op ,e ,xs)))))

(('cons h t) `(,h . ,t))
(('apd xs ys) (& (fold-r cons ,ys ,xs)))

((('cons*f.hd f) h t) (& (cons ,(& (,f ,h)) ,t)))
(('map f xs) (& (fold-r (cons*f.hd ,f) () ,xs)))

(('dup x) `(,x . ,x))
(('dbl n) (+ n n))
))

(e.g. (og:pindolf '(apd (q w e) (a s d)) t3) ===> (q w e a s d))
(e.g. (og:pindolf '(map dbl (1 2 3)) t3) ===> (2 4 6))
(e.g. (og:pindolf '(map dup (- 0 ^)) t3) ===> ((- . -) (0 . 0) (^ . ^)))

(e.g. (run (compiled t3) '(apd (q w e) (a s d))) ===> (q w e a s d))
(e.g. (run (compiled t3) '(map dbl (1 2 3))) ===> (2 4 6))
(e.g. (run (compiled t3) '(map dup (- 0 ^)))
      ===> ((- . -) (0 . 0) (^ . ^)))


#| todo
conceptually:
 x- shouldn't only atoms be quoted in patterns? ~> meh
 .- we need some var types (sym, num, any/exp) to write self-interpreter
 - tail call optimization in the compiler [optional but neat]
 - some step debugger seems necessary too though that's tricky
 with these we could move on with pindof in pindolf and then
 abstract interpretation...
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (atom? x) (not (pair? x))) ;; hehe

(define (matching? expression #;against pattern)
  (define (symbol-match? sym expr binding)
    (match (lookup sym #;in binding)
      (#f `((,sym . ,expr) . ,binding))
      (expr* (and (equal? expr expr*) binding))))
  (let try ((pattern pattern)
            (expression expression)
            (binding '()))    
    (match `(,pattern ,expression)
      (('_ _) binding)
      ((() ()) binding)
      (('T 'T) binding)
      (((? number? n) n) binding)
      ((('num (? symbol? s)) (? number? n)) (symbol-match? s n binding))
      ((('num _) _) #f)
      ((('sym (? symbol? s)) (? symbol? s*)) (symbol-match? s s* binding))
      ((('sym _) _) #f)
      ((('atm (? symbol? s)) (? atom? a)) (symbol-match? s a binding))
      ((('atm _) _) #f)
      ((('exp (? symbol? s)) e) (symbol-match? s e binding))
      ((('quote e) e) binding)
      ((('quote e) _) #f) ;; hmmm?
      (((p . ps) (e . es)) (and-let* ((binding* (try p e binding))
                                      (binding** (try ps es binding*)))
                             binding**))
      (_ #f))))
      
(e.g. (matching? '(a b c) '('a . (exp xs))) ===> ((xs . (b c))))
(e.g. (matching? '(a b c) '(_ 'b (atm x))) ===> ((x . c)))
(e.g. (matching? '(q w e) '((exp x) (exp y) 'z)) ===> #f)
(e.g. (matching? '(q (1 2 3) w) '((sym x) (_ (num n) 3) (exp x*)))
      ===> ((x* . w) (n . 2) (x . q)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (value #;of expression #;wrt binding #;and app)
  (let value* ((expression expression))
    (define (value-qq expr)
      (match expr
        (('unquote expr*) (value* expr*))
        ((e . e*) `(,(value-qq e) . ,(value-qq e*)))
        (e e)))
    (match expression
      (() '())
      ((? symbol? sym) (lookup sym #;in binding))
      ((? number? num) num)
      (('quote expr) expr)
      (('quasiquote expr) (value-qq expr))
      (('+ e e*) (+ (value* e) (value* e*)))
      (('- e e*) (- (value* e) (value* e*)))
      (('& expr) (app (value-qq expr)))
     (_ 'ERROR))))

(e.g. (value '23 '() '_) ===> 23)
(e.g. (value ''bonjour '((x . whatever)) '_) ===> bonjour)
(e.g. (value '(+ 2 (- 100 x)) '((x . 97)) '_) ===> 5)
(e.g. (value '`(,a ,b) '((a . hi) (b . there)) 'whatever)
      ===> (hi there))


(define (pindolf init-expression #;wrt program)
  (let try ((program* program))
    (match program*
      (() 'pndlf:NO-MATCH)
      (((pattern expression) . program**) ;[pretty-print `(try ,pattern)]
       (match (matching? init-expression #;against pattern)
         (#f (try program**))
         (binding;(pretty-print `(matched ,init-expression with ,pattern))
                 ;(pretty-print `(binding: ,binding))
                  (value #;of expression
                         #;wrt binding
                         #;and (lambda (e) (pindolf e program)))))))))


(define t1
  '( (('apd () (exp ys)) ys)
     (('apd ((exp x) . (exp xs)) (exp ys)) `(,x . ,(& (apd ,xs ,ys)))) )
)

(e.g. (pindolf '(apd (q w e) (1 2 3)) t1) ===> (q w e 1 2 3))

(define t2
'((('lookup (exp k) (((exp k) . (exp v)) . _)) `(JUST ,v))
  (('lookup (exp k) (_ . (exp bnd))) (& (lookup ,k ,bnd)))
  (('lookup _ ()) '(NONE))))

(e.g. (pindolf '(lookup x ((x . 23))) t2) ===> (JUST 23))
(e.g. (pindolf '(lookup y ((x . 23) (y . 42))) t2) ===> (JUST 42))
(e.g. (pindolf '(lookup z ((x . 23) (y . 42))) t2) ===> (NONE))


;;; i know they look nasty but just as (quote x) we write 'x
;;; in a few steps we'll be able to write (num n) as #n or (sym s) as $s
;;; and MAYBE we'll allow (exp x) as x (worst case as ?x).
;;; the deal though is that everything is quite regular so that
;;; we won't get lost in space once we start metaprogramming...

(define t3
'(
(('fold-r (exp op) (exp e)       ())
 e)
(('fold-r (exp op) (exp e) ((exp x) . (exp xs)))
 (& (,op ,x ,(& (fold-r ,op ,e ,xs)))))

(('cons (exp h) (exp t)) `(,h . ,t))
(('apd (exp xs) (exp ys)) (& (fold-r cons ,ys ,xs)))

((('cons*f.hd (exp f)) (exp h) (exp t)) (& (cons ,(& (,f ,h)) ,t)))
(('map (exp f) (exp xs)) (& (fold-r (cons*f.hd ,f) () ,xs)))

(('dup (exp x)) `(,x . ,x))
(('dbl (num n)) (+ n n))

))

(e.g. (pindolf '(apd (q w e) (a s d)) t3) ===> (q w e a s d))
(e.g. (pindolf '(map dbl (1 2 3)) t3) ===> (2 4 6))
(e.g. (pindolf '(map dup (- 0 ^)) t3) ===> ((- . -) (0 . 0) (^ . ^)))


(define hohoho
  '(
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

(('lookup (exp k) (((exp k) . (exp v)) . _)) `(JUST ,v))
(('lookup (exp k) (_ . (exp bnd))) (& (lookup ,k ,bnd)))
(('lookup _ ()) '(NONE))
))

(e.g. (pindolf '(match? () ()) hohoho) ===> ())
(e.g. (pindolf '(match? _ (hi)) hohoho) ===> ())
(e.g. (pindolf '(match? 2 2) hohoho) ===> ())
(e.g. (pindolf '(match? 2 3) hohoho) ===> NO-MATCH)
(e.g. (pindolf '(match? (num n) 23) hohoho) ===> ((n . 23)))
(e.g. (pindolf '(match? ((sym x) (num a) (num b)) (j 2 3)) hohoho)
      ===> ((b . 3) (a . 2) (x . j)))
(e.g. (pindolf '(match? ('match?! (sym x) 997) (match?! hi! 997)) hohoho)
      ===> ((x . hi!)))


