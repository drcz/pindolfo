(define-module (pindolf vm)
  #:use-module (grand scheme)
  #:export (run))

(define (atom? x) (not (pair? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; now the fast and dirty (re-)implementation of FLC* variant
;;; (flowcharts with fun.calls similar to Glueck'2012 or sth)

;;; we presume it always starts at (0) and the input is *VIEW*?
;;; primops should be CONS,CAR,CDR,+,-,CONS?,NIL?,EQ? + consts (),T
;;; aaand the calls should be limited to *VIEW*-modifying ones (4now?)


(define (lookup sym #;in binding)
  (match binding
    (() #f)
    (((s . expr) . binding*) (if (equal? s sym) expr
                              #;otherwise (lookup sym binding*)))))

(e.g. (lookup 'y '((x . 23) (y . 42) (z . (he he)))) ===> 42)
(e.g. (lookup 'q '((x . 23) (y . 42) (z . ho!))) ===> #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(define (value expression binding)
  (match expression
    (() '())
    ('T 'T) ;; he_he
    ((? number? n) n)
    ((? symbol? s) (lookup s binding))
    (('V (? number?)) (lookup expression binding)) ;;; !!!!!
    (('quote e) e)
    (('+ e e*) (+ (value e binding) (value e* binding)))
    (('- e e*) (- (value e binding) (value e* binding)))
    (('CONS e e*) (cons (value e binding) (value e* binding)))
    (('CAR e) (car (value e binding)))
    (('CDR e) (cdr (value e binding)))
    (('NIL? e) (if (null? (value e binding)) 'T '()))
    (('NUM? e) (if (number? (value e binding)) 'T '()))
    (('SYM? e) (if (symbol? (value e binding)) 'T '()))
    (('ATM? e) (if (atom? (value e binding)) 'T '()))
    (('CONS? e) (if (pair? (value e binding)) 'T '()))
    (('EQ? e e*) (if (equal? (value e binding) (value e* binding))
                     'T '()))
    (_ 'error)))

(e.g. (value '(+ 2 3) '()) ===> 5)
(e.g. (value '(CONS 'hi (CONS x ())) '((x . there)))
      ===> (hi there))
(e.g. (value '(EQ? x (CONS (CAR x) (CDR x))) '((x . (q w e))))
      ===> T)
(e.g. (value '(EQ? (NIL? a) T) '((a . ()))) ===> T)
(e.g. (value '(EQ? (NIL? a) T) '((a . (1 2)))) ===> ())

(e.g. (value '(ATM? 'x) '()) ===> T)
(e.g. (value '(ATM? '(nope)) '()) ===> ())
(e.g. (value '(NUM? 23) '()) ===> T)
(e.g. (value '(NUM? x) '((x . 23))) ===> T)
(e.g. (value '(NUM? x) '((x . nope!))) ===> ())
(e.g. (value '(SYM? x) '((x . yup))) ===> T)
(e.g. (value '(SYM? x) '((x . (n o p e)))) ===> ())

(e.g. (value '(+ (V 0) (V 1)) '(((V 0) . 2) ((V 1) . 3))) ===> 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (run program input)
  (let loop ((binding `((*VIEW* . ,input)))
             (cur-block (lookup '(0) program)))
    ;[pretty-print binding]
    (match cur-block
      ((('LET v ('CALL pp v*)) . cur-block*)
       (let ((val (loop `((*VIEW* . ,(lookup v* binding)))
                        (lookup pp program))))
         (loop (updated binding v val) cur-block*)))

      ((('LET v e) . cur-block*)
       (let ((val (value e binding)))
         (loop (updated binding v val) cur-block*)))

      ((('GOTO pp)) (loop binding (lookup pp program)))

      ((('RETURN e)) (value e binding))       

      ((('IF e pp-t 'ELSE pp-f))
       (if (equal? (value e binding) '())
           (loop binding (lookup pp-f program))
           (loop binding (lookup pp-t program)))))))

(e.g.
 (run '(((0) (GOTO (0 0)))
        ((0 0) (IF (CONS? *VIEW*) (0 1) ELSE (1 0)))
        ((0 1) (IF (EQ? (CAR *VIEW*) 'APD) (0 2) ELSE (1 0)))
        ((0 2) (IF (CONS? (CDR *VIEW*)) (0 3) ELSE (1 0)))
        ((0 3) (IF (NIL? (CAR (CDR *VIEW*))) (0 4) ELSE (1 0)))
        ((0 4) (IF (CONS? (CDR (CDR *VIEW*))) (0 5) ELSE (1 0)))
        ((0 5) (IF (NIL? (CDR (CDR (CDR *VIEW*)))) (0 6) ELSE (1 0)))
        ((0 6) (LET ys (CAR (CDR (CDR *VIEW*)))) (RETURN ys))
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
               (RETURN (CONS x (V 0)))))
      '(APD (q w e) (1 2 3)))
 ===> (q w e 1 2 3))
;;; incredible!

(e.g. (run '(((0) (GOTO (0 0)))
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
            ((1 7) (LET x (CAR (CDR (CDR *VIEW*))))
                   (RETURN x))
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
                   (RETURN (+ y (V 0)))))
           '(MUL 3 5)) ===> 15)
