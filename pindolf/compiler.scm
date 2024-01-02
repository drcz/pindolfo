(define-module (pindolf compiler)
  #:use-module (grand scheme)
  #:use-module (pindolf parser)
  #:export (compiled))

(define (member? x xs) (and (member x xs) #t)) ;; :)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; even faster and dirtier pindolf->FCL* compiler

(define (lookup sym #;in binding)
  (match binding
    (() #f)
    (((s . expr) . binding*) (if (equal? s sym) expr
                              #;otherwise (lookup sym binding*)))))

(e.g. (lookup 'y '((x . 23) (y . 42) (z . (he he)))) ===> 42)
(e.g. (lookup 'q '((x . 23) (y . 42) (z . ho!))) ===> #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (applications-in expression)
  (define (applications-in-qq qq)
    (match qq
      (('unquote e) (applications-in e))
      ((e . e*) (append (applications-in-qq e) (applications-in-qq e*)))
      (_ '())))
  (match expression
    (() '())
    ((? symbol?) '())
    ((? number?) '())
    (('quote _) '())
    (('quasiquote qq) (applications-in-qq qq))
    (('+ e e*) (append (applications-in e) (applications-in e*)))
    (('- e e*) (append (applications-in e) (applications-in e*)))
    (('& e) (append (applications-in-qq e) `(,expression)))))

(e.g. (applications-in '`(,x . ,(& (APD ,xs ,ys))))
      ===> ( (& (APD (unquote xs) (unquote ys))) ))

(e.g. (applications-in '(& (PLUS ,(& (VAL ,a)) ,(& (VAL ,b)))))
      ===> ( (& (VAL ,a))
             (& (VAL ,b))
             (& (PLUS ,(& (VAL ,a)) ,(& (VAL ,b)))) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (application->vars-map #;for expression)
  (let ((applications (applications-in expression)))
    (map (lambda (app index) `(,app . (V ,index)))
         applications (iota (length applications)))))

(e.g. (application->vars-map '(& (PLUS ,(& (VALUE ,a)) ,(& (VALUE ,b)))))
      ===>( ((& (VALUE ,a)) . (V 0))
            ((& (VALUE ,b)) . (V 1))
            ((& (PLUS ,(& (VALUE ,a)) ,(& (VALUE ,b)))) . (V 2)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cons<-quasiquote qq #;for-subexpressions-using comp-sub)
  (match qq
    (() '())
    (('unquote e) (comp-sub e))
    ((q . q*) `(CONS ,(cons<-quasiquote q comp-sub)
                     ,(cons<-quasiquote q* comp-sub)))
    (e `(quote ,e))))
;;; that comp-sub thing is a bit of afterthought for now but the point
;;; is to be able to compile unquoted expressions with whomever called
;;; cons<-quasiquote (it should get inside compiled-subexpression...).

(e.g. (cons<-quasiquote '(+ ,x ,y) (lambda (x) x)) ;; just trivial comp-sub
      ===> (CONS '+ (CONS x (CONS y ()))))
(e.g. (cons<-quasiquote '(a (b ,c) d) (lambda (x) x))
      ===> (CONS 'a (CONS (CONS 'b (CONS c ())) (CONS 'd ()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (compiled-subexpression expr #;wrt vars-for-apps)
  (let cmpld ((expr expr))
    (match expr
      (() '())
      ((? symbol? sym) sym)
      ((? number? num) num)
      (('quote e) expr)
      (('quasiquote qq) (cons<-quasiquote qq cmpld))
      (('+ e e*) `(+ ,(cmpld e) ,(cmpld e*)))
      (('- e e*) `(- ,(cmpld e) ,(cmpld e*)))
      (('& e) (lookup expr vars-for-apps))
      #;err??)))

(e.g. (compiled-subexpression '`(hi there (+ ,x ,y)) '())
      ===> (CONS 'hi
                 (CONS 'there
                       (CONS (CONS '+ (CONS x (CONS y ())))
                             ()))))

(e.g. (let* ((expression '(+ (& (VAL ,a)) (& (VAL ,b))))
             (vars4apps (application->vars-map expression)))
        (compiled-subexpression expression vars4apps))
      ===> (+ (V 0) (V 1)))

(e.g. (let* ((expression '`(,x . ,(& (APD ,xs ,ys))))
             (vars4apps (application->vars-map expression)))
        (compiled-subexpression expression vars4apps))
      ===> (CONS x (V 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (compiled-expression expression)
  (let* ((vars4apps (application->vars-map expression))
         (apps-code
          (map (lambda (((& e) . var))
                 `((LET *VIEW*
                        ,(compiled-subexpression `(,'quasiquote ,e)
                                                 vars4apps))
                   (LET ,var
                        (CALL (0) *VIEW*))))
               vars4apps))
         (ret-code
          `((RETURN ,(compiled-subexpression expression
                                             vars4apps)))))
    (fold-right append ret-code apps-code)))

(e.g. (compiled-expression '(+ (& (VAL ,a)) (& (VAL ,b))))
      ===> ( (LET *VIEW* (CONS 'VAL (CONS a ())))
             (LET  (V 0) (CALL (0) *VIEW*))
             (LET *VIEW* (CONS 'VAL (CONS b ())))
             (LET  (V 1) (CALL (0) *VIEW*))
             (RETURN (+ (V 0) (V 1)))) )

(e.g. (compiled-expression '`(,x . ,(& (APD ,xs ,ys))))
      ===> ( (LET *VIEW* (CONS 'APD (CONS xs (CONS ys ()))))
             (LET (V 0) (CALL (0) *VIEW*))
             (RETURN (CONS x (V 0))) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (vartype? x) (member? x '(num sym atm exp)))

(define (compiled-clause (pattern expression) index)
  (let loop ((binding '())
             (code '())
             (subindex 0)
             (pending `((,pattern *VIEW*))))
    (match pending
      (()
       `(,@code
         ((,index ,subindex)
          ,@(map (lambda ((s . v)) `(LET ,s ,v)) binding)
          ,@(compiled-expression expression))))

      ((('_ v) . pending*) (loop binding code subindex pending*))

      (((('quote e) v) . pending*)
       (let* ((code* `(,@code
                       ((,index ,subindex) (IF (EQ? ,v ',e)
                                               (,index ,(1+ subindex))
                                               ELSE (,(1+ index) 0)))))
              (subindex* (1+ subindex)))
         (loop binding code* subindex* pending*)))

      ((((? number? n) v) . pending*)
       (let* ((code* `(,@code
                       ((,index ,subindex) (IF (EQ? ,v ,n)
                                               (,index ,(1+ subindex))
                                               ELSE (,(1+ index) 0)))))
              (subindex* (1+ subindex)))
         (loop binding code* subindex* pending*)))

      (((() v) . pending*)
       (let* ((code* `(,@code
                       ((,index ,subindex) (IF (NIL? ,v)
                                               (,index ,(1+ subindex))
                                               ELSE (,(1+ index) 0)))))
              (subindex* (1+ subindex)))
         (loop binding code* subindex* pending*)))

      ;;; i must admit it's not a very friendly procedure
      ;;; and this section is extremely unfriendly. TODO
      (((((? vartype? t) (? symbol? s)) v) . pending*)
       (match (lookup s #;in binding)
         (#f (match t
               ('num
                (let* ((binding* `((,s . ,v) . ,binding))
                       (code*
                        `(,@code
                          ((,index ,subindex) (IF (NUM? ,v)
                                                  (,index ,(1+ subindex))
                                                  ELSE (,(1+ index) 0)))))
                    (subindex* (1+ subindex)))
                  (loop binding* code* subindex* pending*)))
               ('sym
                (let* ((binding* `((,s . ,v) . ,binding))
                       (code*
                        `(,@code
                          ((,index ,subindex) (IF (SYM? ,v)
                                                  (,index ,(1+ subindex))
                                                  ELSE (,(1+ index) 0)))))
                       (subindex* (1+ subindex)))
                  (loop binding* code* subindex* pending*)))
               ('atm
                (let* ((binding* `((,s . ,v) . ,binding))
                       (code*
                        `(,@code
                          ((,index ,subindex) (IF (ATM? ,v)
                                                  (,index ,(1+ subindex))
                                                  ELSE (,(1+ index) 0)))))
                       (subindex* (1+ subindex)))
                  (loop binding* code* subindex* pending*)))
               ('exp
                (let* ((binding* `((,s . ,v) . ,binding)))
                  (loop binding* code subindex pending*)))))
         (v* (let* ((code*
                     `(,@code
                       ((,index ,subindex) (IF (EQ? ,v ,v*)
                                               (,index ,(1+ subindex))
                                               ELSE (,(1+ index) 0)))))
                    (subindex* (1+ subindex)))
               (loop binding code* subindex* pending*)))))

      ((((p . ps) v) . pending*)
       (let* ((pending** `((,p (CAR ,v)) (,ps (CDR ,v)) . ,pending*))
              (code* `(,@code
                       ((,index ,subindex) (IF (CONS? ,v)
                                               (,index ,(1+ subindex))
                                               ELSE (,(1+ index) 0)))))
              (subindex* (1+ subindex)))
         (loop binding code* subindex* pending**))))))


(e.g. (compiled-clause '(('APD () (exp ys)) ys) 1)
      ===>
      (((1 0) (IF (CONS? *VIEW*) (1 1) ELSE (2 0)))
       ((1 1) (IF (EQ? (CAR *VIEW*) 'APD) (1 2) ELSE (2 0)))
       ((1 2) (IF (CONS? (CDR *VIEW*)) (1 3) ELSE (2 0)))
       ((1 3) (IF (NIL? (CAR (CDR *VIEW*))) (1 4) ELSE (2 0)))
       ((1 4) (IF (CONS? (CDR (CDR *VIEW*))) (1 5) ELSE (2 0)))
       ((1 5) (IF (NIL? (CDR (CDR (CDR *VIEW*)))) (1 6) ELSE (2 0)))
       ((1 6) (LET ys (CAR (CDR (CDR *VIEW*))))
              (RETURN ys))))

(e.g. (compiled-clause '(('APD ((exp x) . (exp xs)) (exp ys))
                         `(,x . ,(& (APD ,xs ,ys))))
                       2)
      ===>
      (((2 0) (IF (CONS? *VIEW*) (2 1) ELSE (3 0)))
       ((2 1) (IF (EQ? (CAR *VIEW*) 'APD) (2 2) ELSE (3 0)))
       ((2 2) (IF (CONS? (CDR *VIEW*)) (2 3) ELSE (3 0)))
       ((2 3) (IF (CONS? (CAR (CDR *VIEW*))) (2 4) ELSE (3 0)))
       ((2 4) (IF (CONS? (CDR (CDR *VIEW*))) (2 5) ELSE (3 0)))
       ((2 5) (IF (NIL? (CDR (CDR (CDR *VIEW*)))) (2 6) ELSE (3 0)))
       ((2 6) (LET ys (CAR (CDR (CDR *VIEW*))))
              (LET xs (CDR (CAR (CDR *VIEW*))))
              (LET x (CAR (CAR (CDR *VIEW*))))
              (LET *VIEW* (CONS 'APD (CONS xs (CONS ys ()))))
              (LET (V 0) (CALL (0) *VIEW*))
              (RETURN (CONS x (V 0))))))

(e.g. (compiled-clause '(('ADD (num n) (sym x)) (+ n x)) 5)
      ===> (((5 0) (IF (CONS? *VIEW*) (5 1) ELSE (6 0)))
            ((5 1) (IF (EQ? (CAR *VIEW*) 'ADD) (5 2) ELSE (6 0)))
            ((5 2) (IF (CONS? (CDR *VIEW*)) (5 3) ELSE (6 0)))
            ((5 3) (IF (NUM? (CAR (CDR *VIEW*))) (5 4) ELSE (6 0)))
            ((5 4) (IF (CONS? (CDR (CDR *VIEW*))) (5 5) ELSE (6 0)))
            ((5 5) (IF (SYM? (CAR (CDR (CDR *VIEW*)))) (5 6) ELSE (6 0)))
            ((5 6) (IF (NIL? (CDR (CDR (CDR *VIEW*)))) (5 7) ELSE (6 0)))
            ((5 7) (LET x (CAR (CDR (CDR *VIEW*))))
                   (LET n (CAR (CDR *VIEW*)))
                   (RETURN (+ n x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (compiled program)
  (match (parsed program)
    (('PARSE-ERROR . msg) `(PARSE ERROR! . ,msg)) ;; XD
    (program*
     (let* ((boot `(((0) (GOTO (0 0))))) ;; you can insert stuff there!
            (compiled-clauses (map compiled-clause
                                   program*
                                   (iota (length program)))))
       (apply append `(,boot . ,compiled-clauses))))))

(e.g. (compiled '( (('APD    ()      ?ys) ys)
                   (('APD (?x . ?xs) ?ys) `(,x . ,(& (APD ,xs ,ys))))
                   ))
      ===> ( ((0) (GOTO (0 0)))

             ((0 0) (IF (CONS? *VIEW*) (0 1) ELSE (1 0)))
             ((0 1) (IF (EQ? (CAR *VIEW*) 'APD) (0 2) ELSE (1 0)))
             ((0 2) (IF (CONS? (CDR *VIEW*)) (0 3) ELSE (1 0)))
             ((0 3) (IF (NIL? (CAR (CDR *VIEW*))) (0 4) ELSE (1 0)))
             ((0 4) (IF (CONS? (CDR (CDR *VIEW*))) (0 5) ELSE (1 0)))
             ((0 5) (IF (NIL? (CDR (CDR (CDR *VIEW*)))) (0 6) ELSE (1 0)))
             ((0 6) (LET ys (CAR (CDR (CDR *VIEW*))))
                    (RETURN ys))

             ((1 0) (IF (CONS? *VIEW*) (1 1) ELSE (2 0)))
             ((1 1) (IF (EQ? (CAR *VIEW*) 'APD) (1 2) ELSE (2 0)))
             ((1 2) (IF (CONS? (CDR *VIEW*)) (1 3) ELSE (2 0)))
             ((1 3) (IF (CONS? (CAR (CDR *VIEW*))) (1 4) ELSE (2 0)))
             ((1 4) (IF (CONS? (CDR (CDR *VIEW*))) (1 5) ELSE (2 0)))
             ((1 5) (IF (NIL? (CDR (CDR (CDR *VIEW*)))) (1 6) ELSE (2 0)))
             ((1 6) (LET ys (CAR (CDR (CDR *VIEW*))))
                    (LET xs (CDR (CAR (CDR *VIEW*))))
                    (LET x (CAR (CAR (CDR *VIEW*))))
                    (LET *VIEW* (CONS 'APD (CONS xs (CONS ys ()))))
                    (LET (V 0) (CALL (0) *VIEW*))
                    (RETURN (CONS x (V 0)))) ))
;;;; brilliaaaaaaaaaaaaaaaaant \o/

(e.g. (compiled '((('MUL  0  _) 0)
                  (('MUL  1 %y) y)
                  (('MUL %x %y) (+ y (& (MUL ,(- x 1) ,y))))))
      ===> (((0) (GOTO (0 0)))

            ((0 0) (IF (CONS? *VIEW*) (0 1) ELSE (1 0)))
            ((0 1) (IF (EQ? (CAR *VIEW*) 'MUL) (0 2) ELSE (1 0)))
            ((0 2) (IF (CONS? (CDR *VIEW*)) (0 3) ELSE (1 0)))
            ((0 3) (IF (EQ? (CAR (CDR *VIEW*)) 0) (0 4) ELSE (1 0)))
            ((0 4) (IF (CONS? (CDR (CDR *VIEW*))) (0 5) ELSE (1 0)))
            ((0 5) (IF (NIL? (CDR (CDR (CDR *VIEW*)))) (0 6) ELSE (1 0)))
            ((0 6) (RETURN 0))

            ((1 0) (IF (CONS? *VIEW*) (1 1) ELSE (2 0)))
            ((1 1) (IF (EQ? (CAR *VIEW*) 'MUL) (1 2) ELSE (2 0)))
            ((1 2) (IF (CONS? (CDR *VIEW*)) (1 3) ELSE (2 0)))
            ((1 3) (IF (EQ? (CAR (CDR *VIEW*)) 1) (1 4) ELSE (2 0)))
            ((1 4) (IF (CONS? (CDR (CDR *VIEW*))) (1 5) ELSE (2 0)))
            ((1 5) (IF (NUM? (CAR (CDR (CDR *VIEW*)))) (1 6) ELSE (2 0)))
            ((1 6) (IF (NIL? (CDR (CDR (CDR *VIEW*)))) (1 7) ELSE (2 0)))
            ((1 7) (LET y (CAR (CDR (CDR *VIEW*))))
                   (RETURN y))

            ((2 0) (IF (CONS? *VIEW*) (2 1) ELSE (3 0)))
            ((2 1) (IF (EQ? (CAR *VIEW*) 'MUL) (2 2) ELSE (3 0)))
            ((2 2) (IF (CONS? (CDR *VIEW*)) (2 3) ELSE (3 0)))
            ((2 3) (IF (NUM? (CAR (CDR *VIEW*))) (2 4) ELSE (3 0)))
            ((2 4) (IF (CONS? (CDR (CDR *VIEW*))) (2 5) ELSE (3 0)))
            ((2 5) (IF (NUM? (CAR (CDR (CDR *VIEW*)))) (2 6) ELSE (3 0)))
            ((2 6) (IF (NIL? (CDR (CDR (CDR *VIEW*)))) (2 7) ELSE (3 0)))
            ((2 7) (LET y (CAR (CDR (CDR *VIEW*))))
                   (LET x (CAR (CDR *VIEW*)))
                   (LET *VIEW* (CONS 'MUL (CONS (- x 1) (CONS y ()))))
                   (LET (V 0) (CALL (0) *VIEW*))
                   (RETURN (+ y (V 0))))))
