;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; an ultra-fast and dirty re-implementation of PINDOLF
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
       ((1 1) (IF (EQ? (CAR *VIEW*) 'APD) (1 2) ELSE (2 0)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; now the fast and dirty (re-)implementation of FLC* variant
;;; (flowcharts with fun.calls similar to Glueck'2012 or sth)

;;; we presume it always starts at (0) and the input is *VIEW*?
;;; primops should be CONS,CAR,CDR,+,-,CONS?,NIL?,EQ? + const ()
;;; aaand the calls should be limited to *VIEW*-modifying ones 4now
;;; ----------------------------------------------------------------

(define (updated binding #;with sym #;bound-to val)
  (match binding
    (() `((,sym . ,val)))
    (((sym* . val*) . binding*)
     (if (equal? sym* sym)
         `((,sym . ,val) . ,binding*)
         `((,sym* . ,val*) . ,(updated binding* sym val))))))

(e.g. (updated '((x . 23)) #;with 'y #;bound-to '42)
      ===> ((x . 23) (y . 42)))
(e.g. (updated '((x . 23) (y . 997)) 'y '42)
      ===> ((x . 23) (y . 42)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fcl/true 'T)
(define fcl/false '())

(define (fcl/value expression binding)
  (match expression
    (() '())
    ('T 'T) ;; he_he
    ((? number? n) n)
    ((? symbol? s) (lookup s binding))
    (('V (? number?)) (lookup expression binding)) ;;; !!!!!
    (('quote e) e)
    (('+ e e*) (+ (fcl/value e binding) (fcl/value e* binding)))
    (('- e e*) (- (fcl/value e binding) (fcl/value e* binding)))
    (('CONS e e*) (cons (fcl/value e binding) (fcl/value e* binding)))
    (('CAR e) (car (fcl/value e binding)))
    (('CDR e) (cdr (fcl/value e binding)))
    (('NIL? e) (if (null? (fcl/value e binding)) fcl/true fcl/false))
    (('CONS? e) (if (pair? (fcl/value e binding)) fcl/true fcl/false))
    (('EQ? e e*) (if (equal? (fcl/value e binding)
                             (fcl/value e* binding))
                     fcl/true fcl/false))
    (_ 'error)))

(e.g. (fcl/value '(+ 2 3) '()) ===> 5)
(e.g. (fcl/value '(CONS 'hi (CONS x ())) '((x . there)))
      ===> (hi there))
(e.g. (fcl/value '(EQ? x (CONS (CAR x) (CDR x))) '((x . (q w e))))
      ===> T)
(e.g. (fcl/value '(EQ? (NIL? a) T) '((a . ()))) ===> T)
(e.g. (fcl/value '(EQ? (NIL? a) T) '((a . (1 2)))) ===> ())

(e.g. (fcl/value '(+ (V 0) (V 1)) '(((V 0) . 2) ((V 1) . 3))) ===> 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (FCL* program input)
  (let loop ((binding `((*VIEW* . ,input)))
             (cur-block (lookup '(0) program)))
    ;[pretty-print binding]
    (match cur-block
      ((('LET v ('CALL pp v*)) . cur-block*)
       (let ((val (loop `((*VIEW* . ,(lookup v* binding)))
                        (lookup pp program))))
         (loop (updated binding v val) cur-block*)))

      ((('LET v e) . cur-block*)
       (let ((val (fcl/value e binding)))
         (loop (updated binding v val) cur-block*)))

      ((('GOTO pp)) (loop binding (lookup pp program)))

      ((('RETURN e)) (fcl/value e binding))       

      ((('IF e pp-t 'ELSE pp-f))
       (if (equal? (fcl/value e binding) fcl/false)
           (loop binding (lookup pp-f program))
           (loop binding (lookup pp-t program)))))))

(e.g. (FCL* (compiled p1) '(APD (q w e) (1 2 3)))
      ===> (q w e 1 2 3))
(e.g. (FCL* (compiled p1) '(MUL 3 5)) ===> 15)
(e.g. (FCL* (compiled p1) '(REV (drcz likes fcl*)))
      ===> (fcl* likes drcz))

;;; incredible!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define p2
  '(
(('SUBSTRING a b) (& (SUB ,a ,b ,a ,b)))

(('SUB       () b oryg-a oryg-b) 'FOUND!)
(('SUB (e . a*) b oryg-a oryg-b) (& (TRY ,e ,a* ,b ,oryg-a ,oryg-b)))

(('TRY e a      () oryg-a oryg-b) 'NOT-FOUND!)
(('TRY e a (e . b) oryg-a oryg-b) (& (SUB ,a ,b ,oryg-a ,oryg-b)))
(('TRY e a (_ . b) oryg-a oryg-b) (& (NEXT ,oryg-a ,oryg-b)))

(('NEXT a (_ . b)) (& (SUB ,a ,b ,a ,b)))
(('NEXT a      ()) 'NOT-FOUND!)
))

(e.g. (pindolf '(SUBSTRING (k u l a) (m a k u l a t u r a)) p2)
      ===> FOUND!)
(e.g. (pindolf '(SUBSTRING (k u r a) (m a k u l a t u r a)) p2)
      ===> NOT-FOUND!)

(e.g. (FCL* (compiled p2)
            '(SUBSTRING (k u l a) (m a k u l a t u r a)))
      ===> FOUND!)

(e.g. (FCL* (compiled p2)
            '(SUBSTRING (k u r a) (m a k u l a t u r a)))
      ===> NOT-FOUND!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define p3
'( ;; playing with defunctionalized higher-order-procedures

(('fold-r op e       ()) e)
(('fold-r op e (x . xs)) (& (,op ,x ,(& (fold-r ,op ,e ,xs)))))

(('cons h t) `(,h . ,t))
(('apd xs ys) (& (fold-r cons ,ys ,xs)))

((('cons*f.hd f) h t) (& (cons ,(& (,f ,h)) ,t)))
(('map f xs) (& (fold-r (cons*f.hd ,f) () ,xs)))

(('dup x) `(,x . ,x))
(('dbl n) (+ n n))
))

(e.g. (pindolf '(apd (q w e) (a s d)) p3) ===> (q w e a s d))
(e.g. (pindolf '(map dbl (1 2 3)) p3) ===> (2 4 6))
(e.g. (pindolf '(map dup (- 0 ^)) p3) ===> ((- . -) (0 . 0) (^ . ^)))

(e.g. (FCL* (compiled p3) '(apd (q w e) (a s d))) ===> (q w e a s d))
(e.g. (FCL* (compiled p3) '(map dbl (1 2 3))) ===> (2 4 6))
(e.g. (FCL* (compiled p3) '(map dup (- 0 ^)))
      ===> ((- . -) (0 . 0) (^ . ^)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tmp i thought maybe there was something wrong with nested
;;; applications but nope, all good:
(define t23
  '(
    (('apd () ys) ys)
    (('apd (x . xs) ys) `(,x . ,(& (apd ,xs ,ys))))
    (('gen-xs) `(p n d l f))
    (('test) (& (apd ,(& (gen-xs)) ,(& (gen-xs)))))
    (('test2) `(>> ,(& (gen-xs)) <<))))

(e.g. (pindolf '(test) t23) ===> (p n d l f p n d l f))
(e.g. (pindolf '(test2) t23) ===> (>> (p n d l f) <<))
;;; this could get to some test suite or sth anyway...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; now for some real hardcode, DRC machine...

(define p4
  '(
;;; abstract machine from my b.sc thesis,
;;; in CPS (stolen&redacted from CONSpiracy repo)

(('id x) x) ;; for testing only

#;( -- yup, cats are the bosses -- )
(('cat ()       ys K) (& (,K ,ys)))
(('cat (x . xs) ys K) (& (cat ,xs ,ys (catK ,x ,K))))
((('catK x k) v) (& (,k (,x . ,v))))

#;( -- operations on dictionary (the D register) -- )
(('name k v dict K) (& (,K ((,k . ,v) . ,dict))))

(('forget k ((k . _) . dict) K) (& (,K ,dict)))
(('forget k (     kv . dict) K) (& (forget ,k ,dict (forgetK ,kv ,K))))
((('forgetK kv K) x) (& (,K (,kv . ,x))))

(('lookup k ((k . v) . _) K) (& (,K ,v)))
(('lookup k (_ . dict)    K) (& (lookup ,k ,dict ,K)))

#;( -- the machine proper starts here -- )
(('run-drc program inputs) (& (=>* () ,inputs ,program)))

#;(- STORE, or operating on D -)
(('=>* D (e . R) (('NAME n)   . C)) (& (name ,n ,e ,D (=>*DK ,R ,C))))
(('=>* D    R    (('FORGET n) . C)) (& (forget ,n ,D (=>*DK ,R ,C))))
(('=>* D    R    (('LOOKUP n) . C)) (& (lookup ,n ,D (=>*RK ,D ,R ,C))))

#;(- ...plus its three little continuations -)
((('=>*DK R C) D) (& (=>* ,D ,R ,C)))
((('=>*CK R D) C) (& (=>* ,D ,R ,C)))
((('=>*RK D R C) r) (& (=>* ,D (,r . ,R) ,C)))

#;(- CONSTANTS AND OPS, or operating on R -)
(('=>* D R (('CONST e) . C)) (& (=>* ,D (,e . ,R) ,C)))
(('=>* D R (('PROC p)  . C)) (& (=>* ,D (,p . ,R) ,C)))

(('=>* D (h t . R)     (('CONS) . C)) (& (=>* ,D ((,h . ,t) . ,R) ,C)))
(('=>* D ((h . t) . R) (('CAR) . C)) (& (=>* ,D (,h . ,R) ,C)))
(('=>* D ((h . t) . R) (('CDR) . C)) (& (=>* ,D (,t . ,R) ,C)))

(('=>* D (e e . R)     (('EQ?) . C)) (& (=>* ,D (T  . ,R) ,C)))
(('=>* D (_ _ . R)     (('EQ?) . C)) (& (=>* ,D (() . ,R) ,C)))
(('=>* D ((h . t) . R) (('ATOM?) . C)) (& (=>* ,D (() . ,R) ,C)))
(('=>* D (_       . R) (('ATOM?) . C)) (& (=>* ,D (T  . ,R) ,C)))

#;(- CONTROL FLOW, or operating on C -)
(('=>* D (() . R) (('SELECT p p*) . C)) (& (cat ,p* ,C (=>*CK ,R ,D))))
(('=>* D (_  . R) (('SELECT p p*) . C)) (& (cat ,p ,C (=>*CK ,R ,D))))
(('=>* D (p . R) (('APPLY) . C)) (& (cat ,p ,C (=>*CK ,R ,D))))

#;(- halting? not a problem! -)
(('=>* D (e . R) ()) e) ;;; could have R empty actually...?

#;(- example program, ofc it concatenates two lists -)
(('mk-example) `((PROC ((NAME xs)
                        (NAME ys)
                        (LOOKUP xs)
                        (CONST ())
                        (EQ?)
                        (SELECT ((LOOKUP ys))
                                ((LOOKUP ys)
                                 (LOOKUP xs)
                                 (CDR)
                                 (LOOKUP apd)
                                 (APPLY)
                                 (LOOKUP xs)
                                 (CAR)
                                 (CONS)))
                        (FORGET ys)
                        (FORGET xs)))
                 (NAME apd)
                 (LOOKUP apd)
                 (APPLY)))

(('run-test) (& (run-drc ,(& (mk-example)) ((q w e) (a s d)))))
(('run-test2 xs ys) (& (run-drc ,(& (mk-example)) (,xs ,ys))))

))

; --------------------------------------------------------------

;;; testing one procedure at a time:
(e.g. (pindolf '(cat (q w e) (1 2 3) id) p4)
      ===> (q w e 1 2 3))
(e.g. (pindolf '(name J 23 ((ans . 42)) id) p4)
      ===> ((J . 23) (ans . 42)))
(e.g. (pindolf '(forget ans ((J . 23) (ans . 42)) id) p4)
      ===> ((J . 23)))
(e.g. (pindolf '(forget J ((J . 23) (ans . 42)) id) p4)
      ===> ((ans . 42)))
(e.g. (pindolf '(lookup J ((J . 23) (ans . 42)) id) p4)
      ===> 23)
(e.g. (pindolf '(lookup ans ((J . 23) (ans . 42)) id) p4)
      ===> 42)
;;; ok, more like testing one functionality at a time...
(e.g. (pindolf '(run-drc ((CONST 23) (CONST 42) (CONS)) ()) p4)
      ===> (42 . 23))
(e.g. (pindolf '(run-drc ((CONS)) (23 42)) p4)
      ===> (23 . 42))
(e.g. (pindolf '(run-drc ((NAME ans)
                          (LOOKUP ans)
                          (LOOKUP ans)
                          (CONS))
                         (42)) p4) ===> (42 . 42))
(e.g. (pindolf '(run-drc ((NAME ans)
                          (LOOKUP ans)
                          (LOOKUP ans)
                          (EQ?))
                         (42)) p4) ===> T)
(e.g. (pindolf '(run-drc ((NAME ans)
                          (LOOKUP ans)
                          (LOOKUP ans)
                          (LOOKUP ans)
                          (CONS)
                          (EQ?))
                         (42)) p4) ===> ())
(e.g. (pindolf '(run-drc ((NAME a)
                          (LOOKUP a)
                          (FORGET a))
                         (42)) p4) ===> 42)
(e.g. (pindolf '(run-drc ((PROC ((NAME a)
                                 (LOOKUP a)
                                 (LOOKUP a)
                                 (CONS)
                                 (FORGET a)))
                          (APPLY))
                         (he)) p4) ===> (he . he))

(e.g. (pindolf '(run-drc ((PROC ((NAME a)
                                 (LOOKUP a)
                                 (LOOKUP a)
                                 (CONS)
                                 (FORGET a)))
                          (NAME dbl)
                          (LOOKUP dbl)
                          (APPLY))
                         (hi)) p4) ===> (hi . hi))

;;; finally! silly errors in silly old repos lol.
(e.g. (pindolf '(run-drc ((PROC ((NAME xs)
                                 (NAME ys)
                                 (LOOKUP xs)
                                 (CONST ())
                                 (EQ?)
                                 (SELECT ((LOOKUP ys))
                                         ((LOOKUP ys)
                                          (LOOKUP xs)
                                          (CDR)
                                          (LOOKUP apd)
                                          (APPLY)
                                          (LOOKUP xs)
                                          (CAR)
                                          (CONS)))
                                 (FORGET ys)
                                 (FORGET xs)))
                          (NAME apd)
                          (LOOKUP apd)
                          (APPLY)) ((q w e) (a s d))) p4)
      ===> (q w e a s d))

;;; and it works like a charm
(e.g. (pindolf '(run-test) p4) ===> (q w e a s d))
(e.g. (pindolf '(run-test2 (it does) (work indeed)) p4)
      ===> (it does work indeed))
;;; and compiles correctly too
(e.g. (FCL* (compiled p4) '(run-test)) ===> (q w e a s d))
(e.g. (FCL* (compiled p4) '(run-test2 (it does) (work indeed)))
      ===> (it does work indeed))
