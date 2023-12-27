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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ok, no time Toulouse -- it's compilation time!

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
;czyli tera tak: listuj je, i wtedy po kolei każdej buduj ten rytualny
;(LET *VIEW* <kompilat formy>)
;(CALL (0) *VIEW*)
;no i teraz ino jak jest kompilat to on sam w sobie musi podstawiać,
;wiadomo. więc mapa tych cudów jeszcze potrzebna prosta wiesz, jechane

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
(define (compiled-expression expression) ;; a codeblock...
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
                       ((,index ,subindex) (IF (EQ? ,v ,e)
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

      ((((? symbol? s) v) . pending*)
       (match (lookup s #;in binding)
         (#f (let* ((binding* `((,s . ,v) . ,binding)))
               (loop binding* code subindex pending*)))
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

(e.g. (compiled-clause '(('APD (x . xs) ys) `(,x . ,(& (APD ,xs ,ys))))
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
              (LET *VIEW* (CONS 'APD (CONS xs (CONS ys ()))))
              (LET (V 0) (CALL (0) *VIEW*))
              (RETURN (CONS x (V 0))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (compiled program)
  (let* ((boot `(((0) (GOTO (0 0))))) ;; you can insert stuff there
         (compiled-clauses (map compiled-clause
                                program
                                (iota (length program)))))
    (apply append `(,boot . ,compiled-clauses))))

(e.g. (compiled '( (('APD () ys) ys)
                   (('APD (x . xs) ys) `(,x . ,(& (APD ,xs ,ys)))) ))
      ===> ( ((0) (GOTO (0 0)))

             ((0 0) (IF (CONS? *VIEW*) (0 1) ELSE (1 0)))
             ((0 1) (IF (EQ? (CAR *VIEW*) APD) (0 2) ELSE (1 0)))
             ((0 2) (IF (CONS? (CDR *VIEW*)) (0 3) ELSE (1 0)))
             ((0 3) (IF (NIL? (CAR (CDR *VIEW*))) (0 4) ELSE (1 0)))
             ((0 4) (IF (CONS? (CDR (CDR *VIEW*))) (0 5) ELSE (1 0)))
             ((0 5) (IF (NIL? (CDR (CDR (CDR *VIEW*)))) (0 6) ELSE (1 0)))
             ((0 6) (LET ys (CAR (CDR (CDR *VIEW*))))
                    (RETURN ys))

             ((1 0) (IF (CONS? *VIEW*) (1 1) ELSE (2 0)))
             ((1 1) (IF (EQ? (CAR *VIEW*) APD) (1 2) ELSE (2 0)))
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
