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
             (cur-block (cdr (car program))
                        #;(lookup '(0) program)))
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
           (loop binding (lookup pp-t program)))))))

;;;; pasted from output of troll2:
(define kompilat
'((0 (IF (CONS? *VIEW*) 1 ELSE 90))
 (1 (IF (EQ? (CAR *VIEW*) 'fold-r) 38 ELSE 2))
 (2 (IF (EQ? (CAR *VIEW*) 'cons) 36 ELSE 3))
 (3 (IF (EQ? (CAR *VIEW*) 'apd) 34 ELSE 4))
 (4 (IF (CONS? (CAR *VIEW*)) 5 ELSE 8))
 (5 (IF (EQ? (CAR (CAR *VIEW*)) 'cons*f.hd) 6 ELSE 8))
 (6 (IF (CONS? (CDR (CAR *VIEW*))) 7 ELSE 8))
 (7 (IF (NIL? (CDR (CDR (CAR *VIEW*)))) 32 ELSE 8))
 (8 (IF (EQ? (CAR *VIEW*) 'map) 30 ELSE 9))
 (9 (IF (EQ? (CAR *VIEW*) 'dup) 22 ELSE 10))
 (10 (IF (EQ? (CAR *VIEW*) 'dbl) 17 ELSE 11))
 (11 (IF (EQ? (CAR *VIEW*) 'rev) 12 ELSE 90))
 (12 (IF (CONS? (CDR *VIEW*)) 13 ELSE 90))
 (13 (IF (NIL? (CDR (CDR *VIEW*))) 86 ELSE 14))
 (14 (IF (NIL? (CAR (CDR *VIEW*))) 15 ELSE 26))
 (15 (IF (CONS? (CDR (CDR *VIEW*))) 16 ELSE 90))
 (16 (IF (NIL? (CDR (CDR (CDR *VIEW*)))) 73 ELSE 90))
 (17 (IF (CONS? (CDR *VIEW*)) 18 ELSE 90))
 (18 (IF (NUM? (CAR (CDR *VIEW*))) 21 ELSE 19))
 (19 (IF (EQ? (CAR *VIEW*) 'rev) 20 ELSE 90))
 (20 (IF (NIL? (CDR (CDR *VIEW*))) 86 ELSE 25))
 (21 (IF (NIL? (CDR (CDR *VIEW*))) 88 ELSE 24))
 (22 (IF (CONS? (CDR *VIEW*)) 23 ELSE 90))
 (23 (IF (NIL? (CDR (CDR *VIEW*))) 91 ELSE 24))
 (24 (IF (EQ? (CAR *VIEW*) 'rev) 25 ELSE 90))
 (25 (IF (NIL? (CAR (CDR *VIEW*))) 28 ELSE 26))
 (26 (IF (CONS? (CAR (CDR *VIEW*))) 27 ELSE 90))
 (27 (IF (CONS? (CDR (CDR *VIEW*))) 70 ELSE 90))
 (28 (IF (CONS? (CDR (CDR *VIEW*))) 29 ELSE 90))
 (29 (IF (NIL? (CDR (CDR (CDR *VIEW*)))) 73 ELSE 90))
 (30 (IF (CONS? (CDR *VIEW*)) 31 ELSE 90))
 (31 (IF (CONS? (CDR (CDR *VIEW*))) 74 ELSE 40))
 (32 (IF (CONS? (CDR *VIEW*)) 33 ELSE 90))
 (33 (IF (CONS? (CDR (CDR *VIEW*))) 76 ELSE 40))
 (34 (IF (CONS? (CDR *VIEW*)) 35 ELSE 90))
 (35 (IF (CONS? (CDR (CDR *VIEW*))) 78 ELSE 40))
 (36 (IF (CONS? (CDR *VIEW*)) 37 ELSE 90))
 (37 (IF (CONS? (CDR (CDR *VIEW*))) 80 ELSE 40))
 (38 (IF (CONS? (CDR *VIEW*)) 39 ELSE 90))
 (39 (IF (CONS? (CDR (CDR *VIEW*))) 47 ELSE 40))
 (40 (IF (EQ? (CAR *VIEW*) 'dup) 46 ELSE 41))
 (41 (IF (EQ? (CAR *VIEW*) 'dbl) 42 ELSE 43))
 (42 (IF (NUM? (CAR (CDR *VIEW*))) 45 ELSE 43))
 (43 (IF (EQ? (CAR *VIEW*) 'rev) 44 ELSE 90))
 (44 (IF (NIL? (CDR (CDR *VIEW*))) 86 ELSE 90))
 (45 (IF (NIL? (CDR (CDR *VIEW*))) 88 ELSE 90))
 (46 (IF (NIL? (CDR (CDR *VIEW*))) 91 ELSE 90))
 (47 (IF (CONS? (CDR (CDR (CDR *VIEW*)))) 48 ELSE 53))
 (48 (IF (NIL? (CAR (CDR (CDR (CDR *VIEW*))))) 52 ELSE 49))
 (49 (IF (CONS? (CAR (CDR (CDR (CDR *VIEW*))))) 50 ELSE 53))
 (50 (IF (NIL? (CDR (CDR (CDR (CDR *VIEW*))))) 51 ELSE 53))
 (51
  (LET xs (CDR (CAR (CDR (CDR (CDR *VIEW*))))))
  (LET x (CAR (CAR (CDR (CDR (CDR *VIEW*))))))
  (LET e (CAR (CDR (CDR *VIEW*))))
  (LET op (CAR (CDR *VIEW*)))
  (LET *VIEW* (CONS 'fold-r (CONS op (CONS e (CONS xs ())))))
  (LET (V 0) (CALL 0 *VIEW*))
  (LET *VIEW* (CONS op (CONS x (CONS (V 0) ()))))
  (GOTO 0))
 (52 (IF (NIL? (CDR (CDR (CDR (CDR *VIEW*))))) 93 ELSE 53))
 (53 (IF (EQ? (CAR *VIEW*) 'cons) 80 ELSE 54))
 (54 (IF (EQ? (CAR *VIEW*) 'apd) 78 ELSE 55))
 (55 (IF (CONS? (CAR *VIEW*)) 56 ELSE 59))
 (56 (IF (EQ? (CAR (CAR *VIEW*)) 'cons*f.hd) 57 ELSE 59))
 (57 (IF (CONS? (CDR (CAR *VIEW*))) 58 ELSE 59))
 (58 (IF (NIL? (CDR (CDR (CAR *VIEW*)))) 76 ELSE 59))
 (59 (IF (EQ? (CAR *VIEW*) 'map) 74 ELSE 60))
 (60 (IF (EQ? (CAR *VIEW*) 'dup) 66 ELSE 61))
 (61 (IF (EQ? (CAR *VIEW*) 'dbl) 62 ELSE 63))
 (62 (IF (NUM? (CAR (CDR *VIEW*))) 65 ELSE 63))
 (63 (IF (EQ? (CAR *VIEW*) 'rev) 64 ELSE 90))
 (64 (IF (NIL? (CDR (CDR *VIEW*))) 86 ELSE 68))
 (65 (IF (NIL? (CDR (CDR *VIEW*))) 88 ELSE 67))
 (66 (IF (NIL? (CDR (CDR *VIEW*))) 91 ELSE 67))
 (67 (IF (EQ? (CAR *VIEW*) 'rev) 68 ELSE 90))
 (68 (IF (NIL? (CAR (CDR *VIEW*))) 72 ELSE 69))
 (69 (IF (CONS? (CAR (CDR *VIEW*))) 70 ELSE 90))
 (70 (IF (NIL? (CDR (CDR (CDR *VIEW*)))) 71 ELSE 90))
 (71
  (LET rs (CAR (CDR (CDR *VIEW*))))
  (LET xs (CDR (CAR (CDR *VIEW*))))
  (LET x (CAR (CAR (CDR *VIEW*))))
  (LET *VIEW* (CONS 'rev (CONS xs (CONS (CONS x rs) ()))))
  (GOTO 0))
 (72 (IF (NIL? (CDR (CDR (CDR *VIEW*)))) 73 ELSE 90))
 (73 (LET rs (CAR (CDR (CDR *VIEW*)))) (RETURN rs))
 (74 (IF (NIL? (CDR (CDR (CDR *VIEW*)))) 75 ELSE 81))
 (75
  (LET xs (CAR (CDR (CDR *VIEW*))))
  (LET f (CAR (CDR *VIEW*)))
  (LET *VIEW*
       (CONS 'fold-r
             (CONS (CONS 'cons*f.hd (CONS f ())) (CONS () (CONS xs ())))))
  (GOTO 0))
 (76 (IF (NIL? (CDR (CDR (CDR *VIEW*)))) 77 ELSE 81))
 (77
  (LET t (CAR (CDR (CDR *VIEW*))))
  (LET h (CAR (CDR *VIEW*)))
  (LET f (CAR (CDR (CAR *VIEW*))))
  (LET *VIEW* (CONS f (CONS h ())))
  (LET (V 0) (CALL 0 *VIEW*))
  (LET *VIEW* (CONS 'cons (CONS (V 0) (CONS t ()))))
  (GOTO 0))
 (78 (IF (NIL? (CDR (CDR (CDR *VIEW*)))) 79 ELSE 81))
 (79
  (LET ys (CAR (CDR (CDR *VIEW*))))
  (LET xs (CAR (CDR *VIEW*)))
  (LET *VIEW* (CONS 'fold-r (CONS 'cons (CONS ys (CONS xs ())))))
  (GOTO 0))
 (80 (IF (NIL? (CDR (CDR (CDR *VIEW*)))) 92 ELSE 81))
 (81 (IF (EQ? (CAR *VIEW*) 'dup) 89 ELSE 82))
 (82 (IF (EQ? (CAR *VIEW*) 'dbl) 83 ELSE 84))
 (83 (IF (NUM? (CAR (CDR *VIEW*))) 87 ELSE 84))
 (84 (IF (EQ? (CAR *VIEW*) 'rev) 85 ELSE 90))
 (85 (IF (NIL? (CDR (CDR *VIEW*))) 86 ELSE 90))
 (86
  (LET xs (CAR (CDR *VIEW*)))
  (LET *VIEW* (CONS 'rev (CONS xs (CONS () ()))))
  (GOTO 0))
 (87 (IF (NIL? (CDR (CDR *VIEW*))) 88 ELSE 90))
 (88 (LET n (CAR (CDR *VIEW*))) (RETURN (+ n n)))
 (89 (IF (NIL? (CDR (CDR *VIEW*))) 91 ELSE 90))
 (90 (RETURN 'no-match))
 (91 (LET x (CAR (CDR *VIEW*))) (RETURN (CONS x x)))
 (92
  (LET t (CAR (CDR (CDR *VIEW*))))
  (LET h (CAR (CDR *VIEW*)))
  (RETURN (CONS h t)))
 (93 (LET e (CAR (CDR (CDR *VIEW*)))) (LET op (CAR (CDR *VIEW*))) (RETURN e)))
)

(e.g. (run kompilat '(apd (q w e) (a s d))) ===> (q w e a s d))
[pretty-print `(took ,op-cnter ops)]
(e.g. (run kompilat '(map dbl (1 2 3))) ===> (2 4 6))
[pretty-print `(took ,op-cnter ops)]
(e.g. (run kompilat '(map dup (- 0 ^)))
      ===> ((- . -) (0 . 0) (^ . ^)))
[pretty-print `(took ,op-cnter ops)]
(e.g. (run kompilat '(rev (dercz likes pindolf)))
      ===> (pindolf likes dercz))
[pretty-print `(took ,op-cnter ops)]
(e.g. (run kompilat '(map rev ((1 2 3 4 5 6) (1 2 3 4 5) (1 2 3) (1 2) (1))))
      ===> ((6 5 4 3 2 1) (5 4 3 2 1) (3 2 1) (2 1) (1)))
[pretty-print `(took ,op-cnter ops)]


;;;  express. naive  simplif.   s/n
;;;  apd       132     96      0.727
;;;  map.dbl   274    191      0.697
;;;  map.dup   265    185      0.698
;;;  rev       161     90      0.559
;;;  map.rev  1203    728      0.605

;;; nice. stage0 seems to work!
