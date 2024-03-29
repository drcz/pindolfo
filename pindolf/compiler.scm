(define-module (pindolf compiler)
  #:use-module (grand scheme)
  #:use-module (pindolf parser)
  #:export (compiled))

(define (member? x xs) (and (member x xs) #t)) ;; :)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; even faster and dirtier pindolf->FCL* compiler

(define (lookup key #;in binding)
  (match binding
    (() #f)
    (((k . expr) . binding*) (if (equal? k key) expr
                              #;otherwise (lookup key binding*)))))

(e.g. (lookup 'y '((x . 23) (y . 42) (z . (he he)))) ===> 42)
(e.g. (lookup 'q '((x . 23) (y . 42) (z . ho!))) ===> #f)
(e.g. (lookup '(1 3) '(((0 3) . 23) ((1 3) . 42) ((3 3) . ?))) ===> 42)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    (('+ e e*) (append (applications-in e) (applications-in e*)))
    (('- e e*) (append (applications-in e) (applications-in e*)))
    (('* e e*) (append (applications-in e) (applications-in e*)))
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
      ('T 'T)
      ((? symbol? sym) sym)
      ((? number? num) num)
      (('quote e) expr)
      (('quasiquote qq) (cons<-quasiquote qq cmpld))
      (('+ e e*) `(+ ,(cmpld e) ,(cmpld e*)))
      (('- e e*) `(- ,(cmpld e) ,(cmpld e*)))
      (('* e e*) `(* ,(cmpld e) ,(cmpld e*)))
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
                                             vars4apps))))
         (compilate (fold-right append ret-code apps-code)))
    (match compilate 
      ((code ...
          ('LET v ('CALL lbl '*VIEW*))
          ('RETURN v)) ;;; a tail-call! optimize! o/
       `(,@code (GOTO ,lbl)))
      (_ compilate))))

          

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

;;; tail-call optimization ftw!
(e.g. (compiled-expression '(& (tail-call ,xs)))
      ===> ((LET *VIEW* (CONS 'tail-call (CONS xs ())))
            (GOTO (0)))) ;;; \o/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (vartype? x) (member? x '(num sym atm exp)))

(define (compiled-clause (pattern expression) index)
  (let loop ((binding '())
             (code '())
             (current `(,index 0))
             (pending `((,pattern *VIEW*))))

    (let* (((_ subidx) current)
           (next     `(,index ,(1+ subidx)))
           (fallback `(,(1+ index) 0)))

      (define (code-for cond)
        `(,@code
          (,current (IF ,cond ,next ELSE ,fallback))))

      (match pending
        (() `(,@code
              (,current ,@(map (lambda ((s . v)) `(LET ,s ,v)) binding)
                        ,@(compiled-expression expression))))

        ((('_ v) . pending*) (loop binding code current pending*))

        (((('quote e) v) . pending*)
         (loop binding (code-for `(EQ? ,v ',e)) next pending*))

        ((((? number? n) v) . pending*)
         (loop binding (code-for `(EQ? ,v ,n)) next pending*))

        (((() v) . pending*)
         (loop binding (code-for `(NIL? ,v)) next pending*))

        (((((? vartype? t) (? symbol? s)) v) . pending*)         
         (match (lookup s #;in binding)           
           (#f
            (let ((binding* `((,s . ,v) . ,binding)))
              (match t
                ('num (loop binding* (code-for `(NUM? ,v)) next pending*))
                ('sym (loop binding* (code-for `(SYM? ,v)) next pending*))
                ('atm (loop binding* (code-for `(ATM? ,v)) next pending*))
                ('exp (loop binding* code current pending*)))))
           (v* (loop binding (code-for `(EQ? ,v ,v*)) next pending*))))
        
        ((((p . ps) v) . pending*)
         (loop binding (code-for `(CONS? ,v)) next `((,p (CAR ,v))
                                                     (,ps (CDR ,v))
                                                     . ,pending*)))))))


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
                                   (iota (length program))))
            (fallback `(((,(length program) 0) (RETURN 'no-match)))))
       (apply append `(,boot ,@compiled-clauses ,fallback))))))

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
                    (RETURN (CONS x (V 0))))

             ((2 0) (RETURN 'no-match))))

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
                   (RETURN (+ y (V 0))))

            ((3 0) (RETURN 'no-match))))

