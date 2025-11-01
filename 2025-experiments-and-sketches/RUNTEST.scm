(use-modules (grand scheme))

(define (atom? x) (not (pair? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; now the fast and dirty (re-)implementation of FLC* variant
;;; (flowcharts with fun.calls similar to Glueck'2012 or sth)

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
    (('* e e*) (* (value e binding) (value e* binding)))
    (('CONS e e*) (cons (value e binding) (value e* binding)))
    #;(('CONS e e*) (let ((res (cons (value e binding) (value e* binding))))
                    (write 'new-cons:)
                    (write res) (newline) res)) ;;; TMP
    ;;;;;;;;;;
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
(e.g. (value '(EQ? (CONS (CAR x) (CDR x)) x) '((x . (q w e))))
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

[define op-cnter 0]

(define (run program input)
  [set! op-cnter 0]
  (write 'new-run----------------) (newline) ;; TMP
  (write input) (newline) ;; TMP
  (let loop ((binding `((*VIEW* . ,input)))
             (cur-block (cdr (car program)) ;; :D
                        #;(lookup '0 program)))
    [set! op-cnter [+ op-cnter 1]]
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
           (loop binding (lookup pp-t program))))
      (_ (begin
           (pretty-print `(EVIL BLOCK FOUND!!!))
           (pretty-print cur-block)
           (pretty-print '----)
           997)))))

(let* ((compilatte (read))
       (cmd (read)))
  (pretty-print (run compilatte cmd))
  (pretty-print `(took ,op-cnter ops))
  42)
