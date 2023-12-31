(define-module (pindolf interpreter)
  #:use-module (grand scheme)
  #:export (pindolf value matching?))

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
(define (value #;of expression #;wrt binding #;and program)
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
      (('& expr) (pindolf (value-qq expr) program))
     (_ 'ERROR))))

(e.g. (value '23 '() '_) ===> 23)
(e.g. (value ''bonjour '((x . whatever)) '_) ===> bonjour)
(e.g. (value '(+ 2 (- 100 x)) '((x . 97)) '_) ===> 5)
(e.g. (value '`(,a ,b) '((a . hi) (b . there)) 'whatever)
      ===> (hi there))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (matching? expression #;against pattern)
  (let try ((pattern pattern)
            (expression expression)
            (binding '()))
    (match `(,pattern ,expression)
      ((() ()) binding)
      (('_ _) binding)
      (((? number? n) n) binding)
      (((? symbol? s) e) (match (lookup s #;in binding)
                           (#f `((,s . ,e) . ,binding))
                           (e* (and (equal? e e*) binding))))
      ((('quote e) e) binding)
      ((('quote e) _) #f) ;; !!! otherwise it could bind sth to quote !!!
      (((p . ps) (e . es)) (and-let* ((binding* (try p e binding))
                                      (binding** (try ps es binding*)))
                             binding**))
      (_ #f))))
      
(e.g. (matching? '(a b c) '('a . xs)) ===> ((xs . (b c))))
(e.g. (matching? '(a b c) '(_ 'b x)) ===> ((x . c)))
(e.g. (matching? '(q w e) '(x y 'z)) ===> #f)
(e.g. (matching? '(q (1 2 3) w) '(x (_ n 3) x*))
      ===> ((x* . w) (n . 2) (x . q)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pindolf init-expression #;wrt program)
  (let try ((program* program))
    (match program*
      (() 'NO-MATCH)
      (((pattern expression) . program**)
       (match (matching? init-expression #;against pattern)
         (#f (try program**))
         (binding ;(pretty-print `(matched ,init-expression with ,pattern))
                  ;(pretty-print `(binding: ,binding))
                  (value #;of expression
                         #;wrt binding #;and program)))))))

(define p1 ;;; a bigger test y'know
  '( (('APD () ys) ys)
     (('APD (x . xs) ys) `(,x . ,(& (APD ,xs ,ys))))

     (('MUL 0 x) 0)
     (('MUL 1 x) x)
     (('MUL x y) (+ y (& (MUL ,(- x 1) ,y))))

     (('REV xs) (& (REV ,xs ())))
     (('REV () rs) rs)
     (('REV (x . xs) rs) (& (REV ,xs (,x . ,rs))))
     ))
     
(e.g. (pindolf '(APD (q w e) (a s d)) p1) ===> (q w e a s d))
(e.g. (pindolf '(MUL 3 5) p1) ===> 15)
(e.g. (pindolf '(REV (dercz likes pindolf)) p1)
      ===> (pindolf likes dercz))
