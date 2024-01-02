(define-module (pindolf interpreter)
  #:use-module (grand scheme)
  #:use-module (pindolf parser)
  #:export (pindolf value matching? lookup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; an ultra-fast and dirty re-implementation of PINDOLF

(define (lookup sym #;in binding)
  (match binding
    (() #f)
    (((s . expr) . binding*) (if (equal? s sym) expr
                              #;otherwise (lookup sym binding*)))))

(e.g. (lookup 'y '((x . 23) (y . 42) (z . (he he)))) ===> 42)
(e.g. (lookup 'q '((x . 23) (y . 42) (z . ho!))) ===> #f)

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
(define (pindolf init-expression #;wrt program)
  (match (parsed program)
    (('PARSE-ERROR . msg) `(PARSE ERROR! . ,msg)) ;; XD
    (program* (pindolf* init-expression program*))))
;;; whatever works, anything goes...

(define (pindolf* init-expression #;wrt program)
  (let try ((program* program))
    (match program*
      (() `(NO MATCH for ,init-expression)) ;; !
      (((pattern expression) . program**)
       (match (matching? init-expression #;against pattern)
         (#f (try program**))
         (binding (value #;of expression
                         #;wrt binding
                         #;and (lambda (e) (pindolf* e program)))))))))


(define t1
  '( (('apd () (exp ys)) ys)
     (('apd ((exp x) . (exp xs)) (exp ys)) `(,x . ,(& (apd ,xs ,ys)))) )
)

(e.g. (pindolf* '(apd (q w e) (1 2 3)) t1) ===> (q w e 1 2 3))

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
))

(e.g. (pindolf '(apd (q w e) (a s d)) t3) ===> (q w e a s d))
(e.g. (pindolf '(map dbl (1 2 3)) t3) ===> (2 4 6))
(e.g. (pindolf '(map dup (- 0 ^)) t3) ===> ((- . -) (0 . 0) (^ . ^)))
(e.g. (pindolf '(rev (dercz likes pindolf)) t3)
      ===> (pindolf likes dercz))

