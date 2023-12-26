;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; an ultra-fast and dirty re-implementation of PINDOLF
;;; i just turned 39 btw.
(use-modules (grand scheme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    (match expression
      (() '())
      ((? symbol? sym) (lookup sym #;in binding))
      ((? number? num) num)
      (('quote expr) expr)
      (('quasiquote expr) (let qq ((expr expr))
                            (match expr
                              (('unquote expr*) (value* expr*))
                              ((e . e*) `(,(qq e) . ,(qq e*)))
                              (e e))))
      (('+ e e*) (+ (value* e) (value* e*)))
      (('- e e*) (- (value* e) (value* e*)))
      (('& expr) (pindolf (value* expr) program))
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
      ((('quote e) e) binding)
      (((? number? n) n) binding)
      (((? symbol? s) e) (match (lookup s #;in binding)
                           (#f `((,s . ,e) . ,binding))
                           (e* (and (equal? e e*) binding))))
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
         (binding (value #;of expression
                         #;wrt binding #;and program)))))))

(define p1 ;;; a bigger test y'know
  '( (('APD () ys) ys)
     (('APD (x . xs) ys) `(,x . ,(& `(APD ,xs ,ys))))

     (('MUL 0 x) 0)
     (('MUL 1 x) x)
     (('MUL x y) (+ y (& `(MUL ,(- x 1) ,y))))

     (('REV xs) (& `(REV ,xs ())))
     (('REV () rs) rs)
     (('REV (x . xs) rs) (& `(REV ,xs (,x . ,rs))))
     ))
     
(e.g. (pindolf '(APD (q w e) (a s d)) p1) ===> (q w e a s d))
(e.g. (pindolf '(MUL 3 5) p1) ===> 15)
(e.g. (pindolf '(REV (dercz likes pindolf)) p1)
      ===> (pindolf likes dercz))

;; TODO: perhaps (& `(...)) could be written as  (& ...) alone?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ok, no time Toulouse -- it's compilation time!

#| soo this program:
( (('APD () ys) ys)
  (('APD (x . xs) ys) `(,x . ,(& `(APD ,xs ,ys)))) )

should compile to something like:

(  (0) ((let VIEW input) ;; sure?
        (if (cons? VIEW) (0 1) else (1))))
((0 1) (if (eq? (car VIEW) 'APD) (0 2) else (1)))
((0 2) (if (cons? (cdr VIEW)) (0 3) else (1)))
((0 3) (if (nil? (car (cdr VIEW))) (0 4) else (1)))
((0 4) (if (cons? (cdr (cdr VIEW))) (0 5) else (1)))
((0 5) (if (nil? (cdr (cdr (cdr VIEW)))) (0 6) else (1)))
((0 6) (let ys (car (cdr (cdr VIEW))))
       (return ys))
(  (1) (if (cons? VIEW) (1 1) else (2)))
((1 1) (if (eq? (car VIEW) 'APD) (1 2) else (2)))
((1 2) (if (cons? (cdr VIEW)) (1 3) else (2)))
((1 3) (if (cons? (car (cdr VIEW))) (1 4) else (2)))
((1 4) (if (cons? (cdr (cdr VIEW))) (1 5) else (2)))
((1 5) (if (nil? (cdr (cdr (cdr VIEW)))) (1 6) else (2)))
((1 6) (let x (car (car (cdr VIEW))))
       (let xs (cdr (car (cdr VIEW))))
       (let ys (car (cdr (cdr VIEW))))
       (let input (cons 'APD (cons xs (cons ys '())))) ;; or sth...
       (let res (call (0) input))
       (return (cons x res)))
(  (2) (return 'NO-MATCH))

...more or less? |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (compiled-expression expression)
  `(RETURN ,expression)) ;;; tmp!!! todo: rec.call &
;;; also decompose qq etc...

(define (compiled-clause (pattern expression) index)
  (let loo ((binding '())
            (code '())
            (subindex 0)
            (pending `((,pattern *VIEW*))))
    (match pending
      (()
       `(,@code
         ((,index ,subindex)
          ,@(map (lambda ((s . v)) `(LET ,s ,v)) binding)
          ,(compiled-expression expression))))

      ((('_ v) . pending*) (loo binding code subindex pending*))

      (((('quote e) v) . pending*)
       (let* ((code* `(,@code
                       ((,index ,subindex) (IF (EQ? ,v ,e)
                                               (,index ,(1+ subindex))
                                               ELSE (,(1+ index) 0)))))
              (subindex* (1+ subindex)))
         (loo binding code* subindex* pending*)))

      ((((? number? n) v) . pending*)
       (let* ((code* `(,@code
                       ((,index ,subindex) (IF (EQ? ,v ,n)
                                               (,index ,(1+ subindex))
                                               ELSE (,(1+ index) 0)))))
              (subindex* (1+ subindex)))
         (loo binding code* subindex* pending*)))

      (((() v) . pending*)
       (let* ((code* `(,@code
                       ((,index ,subindex) (IF (NIL? ,v)
                                               (,index ,(1+ subindex))
                                               ELSE (,(1+ index) 0)))))
              (subindex* (1+ subindex)))
         (loo binding code* subindex* pending*)))

      ((((? symbol? s) v) . pending*)
       (match (lookup s #;in binding)
         (#f (let* ((binding* `((,s . ,v) . ,binding)))
               (loo binding* code subindex pending*)))
         (v* (let* ((code*
                     `(,@code
                       ((,index ,subindex) (IF (EQ? ,v ,v*)
                                               (,index ,(1+ subindex))
                                               ELSE (,(1+ index) 0)))))
                    (subindex* (1+ subindex)))
               (loo binding code* subindex* pending*)))))

      ((((p . ps) v) . pending*)
       (let* ((pending** `((,p (CAR ,v)) (,ps (CDR ,v)) . ,pending*))
              (code* `(,@code
                       ((,index ,subindex) (IF (CONS? ,v)
                                               (,index ,(1+ subindex))
                                               ELSE (,(1+ index) 0)))))
              (subindex* (1+ subindex)))
         (loo binding code* subindex* pending**))))))



(e.g. (compiled-clause '(('APD () ys) ys) 1)
      ===>
      (((1 0) (IF (CONS? *VIEW*) (1 1) ELSE (2 0)))
       ((1 1) (IF (EQ? (CAR *VIEW*) APD) (1 2) ELSE (2 0)))
       ((1 2) (IF (CONS? (CDR *VIEW*)) (1 3) ELSE (2 0)))
       ((1 3) (IF (NIL? (CAR (CDR *VIEW*))) (1 4) ELSE (2 0)))
       ((1 4) (IF (CONS? (CDR (CDR *VIEW*))) (1 5) ELSE (2 0)))
       ((1 5) (IF (NIL? (CDR (CDR (CDR *VIEW*)))) (1 6) ELSE (2 0)))
       ((1 6) (LET ys (CAR (CDR (CDR *VIEW*))))
              (RETURN ys))))

(e.g. (compiled-clause '(('APD (x . xs) ys) `(,x . ,(& `(APD ,xs ,ys))))
                       2)
      ===>
      (((2 0) (IF (CONS? *VIEW*) (2 1) ELSE (3 0)))
       ((2 1) (IF (EQ? (CAR *VIEW*) APD) (2 2) ELSE (3 0)))
       ((2 2) (IF (CONS? (CDR *VIEW*)) (2 3) ELSE (3 0)))
       ((2 3) (IF (CONS? (CAR (CDR *VIEW*))) (2 4) ELSE (3 0)))
       ((2 4) (IF (CONS? (CDR (CDR *VIEW*))) (2 5) ELSE (3 0)))
       ((2 5) (IF (NIL? (CDR (CDR (CDR *VIEW*)))) (2 6) ELSE (3 0)))
       ((2 6) (LET ys (CAR (CDR (CDR *VIEW*))))
              (LET xs (CDR (CAR (CDR *VIEW*))))
              (LET x (CAR (CAR (CDR *VIEW*))))
              (RETURN `(,x . ,(& `(APD ,xs ,ys)))))))

;;; wow!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
