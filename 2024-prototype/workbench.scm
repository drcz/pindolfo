(use-modules (grand scheme))
(add-to-load-path "./")
(define pindolf (@ (pindolf interpreter) pindolf))
(define parsed (@ (pindolf parser) parsed))
(define cpsized (@ (pindolf transformations) cpsized))
(define compiled (@ (pindolf compiler) compiled))
(define run (@ (pindolf vm) run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (lookup key #;in binding)
  (match binding
    (() #f)
    (((k . expr) . binding*) (if (equal? k key) expr
                              #;otherwise (lookup key binding*)))))

(define (update key #;with val #;in binding)
  (match binding
    (() `((,key . ,val)))
    (((k . v) . binding*)
     (if (equal? k key) 
         `((,k . ,val) . ,binding*)
         `((,k . ,v) . ,(update key val binding*))))))

(e.g. (update 'x 3 '((a . 23) (x . 6) (y . 10)))
      ===> ((a . 23) (x . 3) (y . 10)))

(e.g. (update 'z 3 '((a . 23) (x . 6) (y . 10)))
      ===> ((a . 23) (x . 6) (y . 10) (z . 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define test123 
  (compiled '( (('APD    ()      ?ys) ys)
               (('APD (?x . ?xs) ?ys) `(,x . ,(& (APD ,xs ,ys))))
               )))

(define p3
  '(
     (('fold-r (exp op) (exp e) ()) e)
     (('fold-r (exp op) (exp e) ((exp x) . (exp xs)))
      (& (,op ,x ,(& (fold-r ,op ,e ,xs)))))

     (('cons (exp h) (exp t)) `(,h . ,t))
     (('apd (exp xs) (exp ys)) (& (fold-r cons ,ys ,xs)))

     ((('cons*f.hd (exp f)) (exp h) (exp t)) (& (cons ,(& (,f ,h)) ,t)))
     (('map (exp f) (exp xs)) (& (fold-r (cons*f.hd ,f) () ,xs)))
     
     (('dup (exp x)) `(,x . ,x))
     (('dbl (num n)) (+ n n))
     
     (('rev (exp xs)) (& (rev ,xs ())))
     (('rev () (exp rs)) rs)
     (('rev ((exp x) . (exp xs)) (exp rs)) (& (rev ,xs (,x . ,rs))))
     ))

(define test-p3 (compiled p3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; first steps towards some kind of driving, for redundant tests
;;; removal to save some calls to <value> etc.

;;; 0. some helpful stuff
(define (all-tests code)
  (apply union
         (map (lambda (block)
                (match block 
                  ((lbl . (('IF t _ 'ELSE _))) `(,t))
                  (_ '())))
              code)))

(e.g. (all-tests test123)
      ===> ((CONS? (CAR (CDR *VIEW*)))
            (NIL? (CDR (CDR (CDR *VIEW*))))
            (CONS? (CDR (CDR *VIEW*)))
            (NIL? (CAR (CDR *VIEW*)))
            (CONS? (CDR *VIEW*))
            (EQ? (CAR *VIEW*) 'APD)
            (CONS? *VIEW*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. analysis of decission tree:

(define (successors label code) ;;; ONLY for IF nodes!
  (match (lookup label code)
    ((('IF _ l 'ELSE l*)) `(,l ,l*))
    ;((('GOTO l)) `(,l)) ;; again, just for bootstrap block?
    ((('GOTO _)) '()) ;; waat?
    (_ '())))

(e.g. (successors '(0 5) test123) ===> ((0 6) (1 0)))

;;; so we want to know for each block which tests are still
;;; alive i.e. worth remembering (is there any chance the
;;; result of this test will be needed on any path from given
;;; block). it's a bit like LVA, only simpler.
;;; nb the ordering is crucial to normalize the form of ppoints
;;; since we don't want different ordering of the same tests
;;; to result in a different ppoint -- cf (ppoint _ _ _ _) below.

(define (live-tests code)
  (let* ((init-tests (map (lambda (block)
                            (match block
                              ((lbl . (('IF t _ 'ELSE _))) `(,lbl . (,t)))
                              ((lbl . _) `(,lbl . ()))))
                          code))
         (all-tests (all-tests code)) ;; for ordering!
         (all-labels (map car code)))
    (let loop ((pend all-labels)
               (changes? #f)
               (tests init-tests))
      (if (null? pend)
          (if changes?
              (loop all-labels #f tests)
              tests)
          (let* (((lbl . pend*) pend)
                 (my-tests (lookup lbl tests))
                 (inherited-tests (apply append
                                         (map (lambda (l) (lookup l tests))
                                              (successors lbl code))))
                 (my-tests* (intersection all-tests ;;; ordering!
                                          (union my-tests inherited-tests)))
                 (changes?* (or changes?
                                (> (length my-tests*) (length my-tests))))
                 (tests* (if changes?*
                             (update lbl my-tests* tests)
                             tests)))
            (loop pend* changes?* tests*))))))

;;; we'll now use program points to describe label+info on tests
;;; failed and passed, so that they become new blocks of the residual
;;; program.

(define (ppoint lbl passed failed LVA)
  (let* ((live (lookup lbl LVA))
         (passed* (intersection live passed)) ;; see? ordering, since intersection
         (failed* (intersection live failed))) ;; preserves order from left operand
    `(,lbl ,passed* ,failed*)))

(e.g. (ppoint '(2 3)
              '(test1 test3)
              '(test4 test2) ;; again, nb corrected ordering in the result
              '(((2 3) . (test2 test3 test4))))
      ===> ((2 3) (test3) (test2 test4)))

;;; sorry ;)
(define (pp-lbl pp) (car pp))
(define (pp-passed pp) (cadr pp))
(define (pp-failed pp) (caddr pp))

;;; for now we won't allow any information to follow via CALLs, nor from
;;; GOTOs at the end of leaf block, since the *VIEW* is rebuit and we'd have
;;; to perform some calculations on which test info could be preserved; this
;;; could be achieved with some funny test algebra but that'll be the next
;;; experiment (if this one works at all), maybe in 2 weeks from now...

(define (massaged-lets ls LVA)
  (match ls
    (() '())
    ((('LET v ('CALL lbl-call '*VIEW*)) . ls*)
     `((LET ,v (CALL ,(ppoint lbl-call '() '() LVA) *VIEW*))
       . ,(massaged-lets ls* LVA)))
    ((l . ls*) `(,l . ,(massaged-lets ls* LVA)))))
;; instead of passing LVA we could have just cooked something in here
;; like `((,lbl-call . ())) BUT (a) caller keeps LVA map anyway, (b) later
;; this might be nevessary over here, if we want to perform the aforementioned
;; test calculations...
(e.g. (massaged-lets '((LET x (CAR *VIEW*))
                       (LET y (CALL (0) *VIEW*))
                       (LET z (CALL (0 1) *VIEW*))) '(((0) . ())
                                                      ((0 1) . ())))
      ===> ((LET x (CAR *VIEW*))
            (LET y (CALL ((0) () ()) *VIEW*))
            (LET z (CALL ((0 1) () ()) *VIEW*))))
;; perhaps this should be massaged-expression and happen on RETURNs too, though
;; as of now (CALL (0) *VIEW*) can only occur inside LET anyway...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. we're almost ready, just A LITTLE BIT OF LOGIC y'know...
;;; you've already seen what a test is, the expression after 'IF.
;;; an address is the part of test naming some part of *VIEW*, like
;;; *VIEW* or (CAR (CDR *VIEW*)).

(define (address? x)
  (match x
    ('*VIEW* #t)
    (('CAR e) (address? e))
    (('CDR e) (address? e))
    (_ #f)))

(e.g. (address? '*VIEW*))
(e.g. (address? '(CAR *VIEW*)))
(e.g. (address? '(CAR (CDR (CAR *VIEW*)))))
(e.g. (not (address? ''apd)))

;;; first of all, sometimes we expect two addresses a0 and a1 to contain
;;; the same value (expression) -- with the test (EQ? <a0> <a1>). so all
;;; the tests concerning a0 should also be true about a1!
;;; any such pair of addresses we'll call aliases of each other.

(define (address-aliases addr tests)
  (define (all-aliases-for a #;in ts)
    (match ts
      (() '())
      ((('EQ? (? (is _ equal? a)) (? address? a*)) . ts*)
       `(,a* . ,(all-aliases-for a ts*)))
     ((('EQ? (? address? a*) (? (is _ equal? a))) . ts*)
      `(,a* . ,(all-aliases-for a ts*)))
     ((_ .  ts*)
      (all-aliases-for a ts*))))  
  (let greedy ((aliases `(,addr))
               (new-aliases (all-aliases-for addr #;in tests)))
    (let* ((newer-aliases (apply union (map (lambda (a) (all-aliases-for a tests))
                                            new-aliases)))
           (aliases* (union aliases new-aliases))
           (truly-newer-aliases (difference newer-aliases aliases*)))
      (if (> (length truly-newer-aliases) 0)
          (greedy aliases* truly-newer-aliases)
          aliases*))))

(e.g. (address-aliases '(CAR *VIEW*) '((EQ? (CAR *VIEW*) (CDR *VIEW*))))
      ===> ((CDR *VIEW*) (CAR *VIEW*)))
(e.g. (address-aliases '(CAR *VIEW*)
                       '((EQ? (CAR *VIEW*) (CAR (CDR *VIEW*)))
                         (EQ? (CAR (CDR (CDR *VIEW*))) (CAR (CDR *VIEW*)))))
      ===> ((CAR (CDR (CDR *VIEW*)))
            (CAR (CDR *VIEW*))
            (CAR *VIEW*)))

;;; ok, now we need all the tests concerning whichever of these addresses:
(define (all-tests-concerning-one address tests)
  (filter (lambda (t) (match t
                   (('NIL? (? (is _ equal? address))) #t)
                   (('ATM? (? (is _ equal? address))) #t)
                   (('SYM? (? (is _ equal? address))) #t)
                   (('NUM? (? (is _ equal? address))) #t)
                   (('CONS? (? (is _ equal? address))) #t)
                   (('EQ? (? (is _ equal? address)) _) #t)
                   (('EQ? _ (? (is _ equal? address))) #t)
                   (_ #f)))
          tests))

(define (all-tests-concerning addresses tests)
  (apply union (map (lambda (a) (all-tests-concerning-one a tests)) addresses)))

(e.g. (all-tests-concerning-one '(CAR (CDR *VIEW*)) (all-tests test123))
      ===> ((CONS? (CAR (CDR *VIEW*)))
            (NIL? (CAR (CDR *VIEW*)))))
(e.g. (all-tests-concerning-one '(CAR *VIEW*) (all-tests test123))
      ===> ((EQ? (CAR *VIEW*) 'APD)))

(e.g. (all-tests-concerning '((CAR *VIEW*)
                              (CAR (CDR *VIEW*))) (all-tests test123))
      ===> ((NIL? (CAR (CDR *VIEW*)))
            (CONS? (CAR (CDR *VIEW*)))
            (EQ? (CAR *VIEW*) 'APD)))

;;; once this is done we no longer care about address, just the tests;
;;; we turn these tests into abstract descriptions of s-expressions,
;;; which can take one the following forms:
(define (valid-description? d)
  (match d
    (('CONS?) #t)
    (('NIL?) #t)
    (('ATM?) #t)
    (('SYM?) #t)
    (('NUM?) #t)
    (('EQ? (? number?)) #t)
    (('EQ? (? symbol?)) #t) ;; nb no quote.
    (('not (? valid-description?)) #t)
    (_ #f)))

;;; always useful (or not?):
(define (normalized d)
  (match d
    (('not ('not d*)) (normalized d*))
    (_ d)))

;;; so let's construct these descriptions:
(define (description<-test t)
  (match t
    (('CONS? _) '(CONS?))
    (('NIL? _) '(NIL?))
    (('ATM? _) '(ATM?))
    (('NUM? _) '(NUM?))
    (('SYM? _) '(SYM?))
    (('EQ? (? address?) (? number? n)) `(EQ? ,n))
    (('EQ? (? number? n) (? address?)) `(EQ? ,n))
    ;;; quoted numbers would be too much, no?
    (('EQ? (? address?) ('quote (? symbol? s))) `(EQ? ,s))
    (('EQ? ('quote (? symbol? s)) (? address?)) `(EQ? ,s))
    (_ #f)))

(define (descriptions<-tests ts)
  (filter-map description<-test ts))

(e.g. (descriptions<-tests '((EQ? (CAR *VIEW*) 23)
                             (EQ? (CDR *VIEW*) 'something)
                             (CONS? *VIEW*)
                             (EQ? (CAR *VIEW*) (CDR *VIEW*))))
      ===> ((EQ? 23) (EQ? something) (CONS?)))

;;; the point is to use them for only the tests concerning selected
;;; address and its aliases (if any).
(e.g. (descriptions<-tests
       (all-tests-concerning '((CAR *VIEW*)
                               (CAR (CDR *VIEW*))) (all-tests test123)))
      ===> ((NIL?) (CONS?) (EQ? APD)))

;;; for managing failed tests too:
(define (negated-descriptions ds)
  (map (lambda (d) (normalized `(not ,d))) ds))

(e.g. (negated-descriptions '((NIL?) (ATM?) (not (CONS?))))
      ===> ((not (NIL?)) (not (ATM?)) (CONS?)))


;;; it's not hard (only maybe a bit tedious) to figure out when two descriptions
;;; can't be simultainously satisfied:
(define ((contradicts? d0) d1)
  (match `(,d0 ,d1)
    ((d* ('not d*)) #t)
    ((('not d*) d*) #t)
    ((('CONS?) ('NIL?)) #t)
    ((('CONS?) ('ATM?)) #t)
    ((('CONS?) ('SYM?)) #t)
    ((('CONS?) ('NUM?)) #t)
    ((('CONS?) ('EQ? _)) #t)
    ((('NIL?) ('CONS?)) #t)
    ((('NIL?) ('SYM?)) #t)
    ((('NIL?) ('NUM?)) #t)
    ((('NIL?) ('EQ? ())) #f) ;; !
    ((('NIL?) ('EQ? _)) #t)
    ((('ATM?) ('CONS?)) #t)
    ((('SYM?) ('CONS?)) #t)
    ((('SYM?) ('NIL?)) #t)
    ((('SYM?) ('NUM?)) #t)
    ((('SYM?) ('EQ? (? number?))) #t)
    ((('SYM?) ('EQ? ())) #t)
    ((('NUM?) ('CONS?)) #t)
    ((('NUM?) ('NIL?)) #t)
    ((('NUM?) ('SYM?)) #t)
    ((('NUM?) ('EQ? (? symbol?))) #t)
    ((('NUM?) ('EQ? ())) #t)
    ((('EQ? x) ('EQ? x)) #f)
    ((('EQ? x) ('EQ? y)) #t)
    ((('EQ? _) ('CONS?)) #t)
    ((_ _) #f)))

;;; then there's some stuff following from particular descriptions too:
(define (implies d)
  (match d
    (('CONS?) '((not (ATM?))
                (not (NIL?))
                (not (SYM?))
                (not (NUM?))))
    (('not ('CONS?)) '((ATM?)))
    (('NIL?) '((ATM?)
               (not (SYM?))
               (not (NUM?))
               (not (CONS?))))
    (('ATM?) '((not (CONS?))))
    (('not ('ATM?)) '((CONS?)))
    (('NUM?) '((ATM?)
               (not (SYM?))
               (not (NIL?))
               (not (CONS?))))
    (('SYM?) '((ATM?)
               (not (NUM?))
               (not (NIL?))
               (not (CONS?))))
    (('EQ? ()) '((ATM?)
                 (NIL?)
                 (not (SYM?))
                 (not (NUM?))
                 (not (CONS?)))) ;; shouldn't happen...?
    (('EQ? (? number?)) '((ATM?)
                          (NUM?)
                          (not (NIL?))
                          (not (SYM?))
                          (not (CONS?))))
    (('EQ? (? symbol?)) '((ATM?)
                          (SYM?)
                          (not (NIL?))
                          (not (NUM?))
                          (not (CONS?))))
    (_ '())))

;;; and the "heavieast" stuff; mind they're mutually exclusive:
(define (infered ds)
  (cond ((and (member? '(not (NUM?)) ds)
              (member? '(not (NIL?)) ds)
              (member? '(ATM?) ds))
         '((SYM?)))
        ((and (member? '(not (SYM?)) ds)
              (member? '(not (NIL?)) ds)
              (member? '(ATM?) ds))
         '((NUM?)))
        ((and (member? '(not (SYM?)) ds)
              (member? '(not (NUM?)) ds)
              (member? '(ATM?) ds))
         '((NIL?)))
        (else '())))

;;; so NOW we can compute entailments (sort of):
(define (inf-closure ds)
  (let* ((ds* (apply union ds (map implies ds)))
         (ds** (union ds* (infered ds*))))
    ds**))

(e.g. (inf-closure '((not (CONS?)) (EQ? qwe)))
      ===> ((not (NUM?)) (not (NIL?)) (SYM?) (ATM?) (not (CONS?)) (EQ? qwe)))

(e.g. (inf-closure '((not (CONS?)) (not (NUM?)) (not (NIL?))))
      ===> ((SYM?) (ATM?) (not (CONS?)) (not (NUM?)) (not (NIL?))))

;;; and *NOW* ladies and gentlement, the big thing:
(define (inconsistent? ds)
  (let ((ds* (inf-closure ds)))
    (any (lambda (d) (any (contradicts? d) ds*)) ds*)))

(e.g. (inconsistent? '((CONS?) (ATM?))))
(e.g. (inconsistent? '((not (CONS?)) (not (ATM?)))))
(e.g. (inconsistent? '((CONS?) (SYM?))))
(e.g. (inconsistent? '((EQ? x) (NUM?))))
(e.g. (inconsistent? '((EQ? x) (not (ATM?)))))
(e.g. (inconsistent? '((EQ? map) (EQ? apd))))
(e.g. (not (inconsistent? '((EQ? map) (EQ? map)))))
(e.g. (not (inconsistent? '((EQ? map) (SYM?)))))
(e.g. (not (inconsistent? '((EQ? map) (not (NUM?))))))

;;; dadaam:
(define (consistent? ds) (not (inconsistent? ds)))


;;; easy, right? we have all the tools to decide which of the already
;;; tested/computed tests can be skipped by careful magical analysis,
;;; aka "the actual magic happens here":

(define (outcome-analysis #;of test #;given passed failed all-tests)
  (cond ((member? test passed) 'PASSED)
        ((member? test failed) 'FAILED)
        (else ;; madness starts here:
         (let* ((address (cadr test)) ;;; hahaha
                (desc-t (description<-test test))
                (desc-~t (normalized `(not ,desc-t)))
                (aliases (address-aliases address passed))
                (passed* (all-tests-concerning aliases passed))
                (failed* (all-tests-concerning aliases failed))
                (descs (append (descriptions<-tests passed*)
                               (negated-descriptions
                                (descriptions<-tests failed*))))
                (consistent-with-t? (consistent? `(,desc-t . ,descs)))
                (consistent-with-~t? (consistent? `(,desc-~t . ,descs))))
           (cond ((and consistent-with-t?
                       consistent-with-~t?) 'BOTH)
                 (consistent-with-t? 'PASSED)
                 (consistent-with-~t? 'FAILED)
                 (else 'IMPOSSIBLE))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. now THIS is the core of our poor man's driving:

(define (new-block pp code LVA all-tests)
  ;; assert (equal? LVA (live-tests code)) and also
  ;; assert (equal? all-tests (all-tests code)) y'know
  (let* ((lbl (pp-lbl pp))
         (passed (pp-passed pp))
         (failed (pp-failed pp))
         (old-code (lookup (pp-lbl pp) code)))
  (match old-code 
    ((('IF test lbl-then 'ELSE lbl-else))
     (match (outcome-analysis #;of test #;given passed failed all-tests)
       ('PASSED (let* ((pp-then (ppoint lbl-then passed failed LVA))
                       (new-code `((GOTO ,pp-then))))
                  `(,pp . ,new-code)))
       ('FAILED (let* ((pp-else (ppoint lbl-else passed failed LVA))
                       (new-code `((GOTO ,pp-else))))
                  `(,pp . ,new-code)))
       ('BOTH (let* ((pp-then
                      (ppoint lbl-then `(,test . ,passed) failed LVA))
                     (pp-else
                      (ppoint lbl-else passed `(,test . ,failed) LVA))
                     (new-code
                      `((IF ,test ,pp-then ELSE ,pp-else))))
                `(,pp . ,new-code)))
       ('IMPOSSILBE `(,pp (RETURN 'look-this-point-is-unreachable)))))

    ((('GOTO lbl-jmp)) ;; sure??
     (let* ((pp-jmp (ppoint lbl-jmp passed failed LVA))
            (new-code `((GOTO ,pp-jmp))))
       `(,pp . ,new-code)))

    ((lets ... ('RETURN e))
      (let* ((new-code `(,@(massaged-lets lets LVA) (RETURN ,e))))
        `(,pp . ,new-code)))

    ((lets ... ('GOTO lbl-jmp)) ;;; ignore passed and failed there:
      (let* ((pp-jmp (ppoint lbl-jmp '() #;passed '() #;failed LVA))
             (new-code `(,@(massaged-lets lets LVA) (GOTO ,pp-jmp))))
        `(,pp . ,new-code)))
    ;;; todo more stuff with CALL or tail-GOTOs?
    )))

;;; surprisingly this works, e.g. look at that block labelled (0 2):
(e.g. (lookup '(0 2) test123)
      ===> ((IF (CONS? (CDR *VIEW*)) (0 3) ELSE (1 0))))
;; we can propagate (EQ? (CAR *VIEW*) 'APD) { which we'll from now on write
;; shoter as eq?[car,'APD] since we know it's all about *VIEW* }:
(e.g.
 (new-block (ppoint '(0 2) '((EQ? (CAR *VIEW*) 'APD)) '() (live-tests test123))
            test123
            (live-tests test123)
            (all-tests test123))
 ===> ([(0 2) ((EQ? (CAR *VIEW*) 'APD)) ()]
       (IF (CONS? (CDR *VIEW*))
           [(0 3) ((CONS? (CDR *VIEW*)) (EQ? (CAR *VIEW*) 'APD)) ()]
           ELSE [(1 0) ((EQ? (CAR *VIEW*) 'APD)) ((CONS? (CDR *VIEW*)))])))
;;; and it gets to passed tests on both branches since the same eq?[car,'APD]
;;; can be asked again if we reach (1 1), which is not impossible.

;;; now presume we already knew cons?[cdr]:
(e.g.
 (new-block (ppoint '(0 2) '((CONS? (CDR *VIEW*))) '() (live-tests test123))
            test123
            (live-tests test123)
            (all-tests test123))
 ===> ([(0 2) ((CONS? (CDR *VIEW*))) ()]
       (GOTO [(0 3) ((CONS? (CDR *VIEW*))) ()])))
;;; so there's no need to build IF statement, only jump to "true branch",
;;; one specialized wrt cons?[cdr]

;;; on the contrary, if we knew cons?[cdr] wasn't satisfied:
(e.g.
 (new-block (ppoint '(0 2) '() '((CONS? (CDR *VIEW*))) (live-tests test123))
            test123
            (live-tests test123)
            (all-tests test123))
 ===> ([(0 2) () ((CONS? (CDR *VIEW*)))]
       (GOTO [(1 0) () ((CONS? (CDR *VIEW*)))])))
;;; similarly only "else branch" jump and preserving knowledge about test failed

;;; now it'll be handy to extract ppoints from such newly built block:
(define (successor-pps pp.newcode)
  (let* (((pp . newcode) pp.newcode)
         (control (last newcode)))
    (match control
      (('IF _ pp-t 'ELSE pp-e) `(,pp-t ,pp-e))
      (('GOTO pp-j) `(,pp-j))
      (_ '()))))

;;; we'll now generate (0 2)'s version where nothing is known about tests:
(e.g. (new-block (ppoint '(0 2) '() '() (live-tests test123))
                                test123 (live-tests test123) (all-tests test123))
      ===> ([(0 2) () ()]
            (IF (CONS? (CDR *VIEW*)) [(0 3) ((CONS? (CDR *VIEW*))) ()]
                ELSE [(1 0) () ((CONS? (CDR *VIEW*)))])))
;;; and we can easily get both possible futures (ppoints) of it:
(e.g. (successor-pps (new-block (ppoint '(0 2) '() '() (live-tests test123))
                                test123 (live-tests test123) (all-tests test123)))
      ===> ([(0 3) ((CONS? (CDR *VIEW*))) ()]
            [(1 0) () ((CONS? (CDR *VIEW*)))]))

;;; sweet.

;;; 2024/03/13 wait don't do this LTA! the whole problem was super silly
;;; once the (EQ? <address> <const0>) test was performed we forgot it, so
;;; it couldn't be used to resolve (EQ? <address> <const1>).
;;; it really should be live variable analysis, not live test analysis;
;;; and not even that because of equalities (EQ? <address0> <address1>).
;;; there's a neat way to resolve all these problems, for now just pretend
;;; all the tests are relevant everywhere and nevermind duplicated leaves:
(define (fake-lta code)
  (let* ((all (all-tests code)))
    (map (lambda ((lbl . code)) `(,lbl . ,all)) code)))


(define (residual passed failed code)
  (let ((first-lbl (caar code)) ;; XD
        (LTA (live-tests code))
        (all-tests (fake-lta #;all-tests code)))
    (let ride ((pending `(,(ppoint first-lbl passed failed LTA)))
               (new-code '()))
      (match pending
        (() new-code)
        ((pp . pending*) (match (lookup pp new-code)
                           (#f (let* ((block (new-block pp code LTA all-tests))
                                      (futures (successor-pps block)))
                                 (ride (append futures pending*)
                                       `(,@new-code ,block))))
                           (_ (ride pending* new-code))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. prettify&simplify...
;;; we'll see the outputs in a minute since now they're barely readable
;;; because (a) ppoints-as-labels was handy but not friendly, (b) there
;;; are now tons of GOTO-threads which require compressing.

;;; like this:
(define (final-lbl #;starting-at lbl #;in code)
  (match (lookup lbl code)
    ((('GOTO lbl*)) (final-lbl lbl* code))
    (_ lbl)))

(e.g. (final-lbl '0 '((0 (GOTO 1)) (1 (GOTO 3)) (3 (GOTO 2)) (2 (RETURN x))))
      ===> 2)

(define (compressed-trasitions-in-block block code)
  (match block
    ((('GOTO lbl-jmp))
     `((GOTO ,(final-lbl lbl-jmp code))))
    ((('IF t lbl-then 'ELSE lbl-else))
     `((IF ,t ,(final-lbl lbl-then code) ELSE ,(final-lbl lbl-else code))))
    ((('LET v ('CALL lbl-call '*VIEW*)) . block*)
     `((LET ,v (CALL ,(final-lbl lbl-call code) *VIEW*))
       . ,(compressed-trasitions-in-block block* code)))
    ((('LET v e) . block*)
     `((LET ,v ,e)
       . ,(compressed-trasitions-in-block block* code)))
    ((('RETURN e))
     block)))

(define (compressed-trasitions code)
  (map (lambda ((lbl . block))
         `(,lbl . ,(compressed-trasitions-in-block block code)))
       code))

;;; that also leaves some dead code, so we'll get rid of unreachable blocks
;;; (the GOTO-alone thingies). first let's find labels of blocks _directly_
;;; reachable from one particular block. mind this thing is different from
;;; (successors <lbl> <code>) used to follow IF-only blocks, because it also
;;; counts GOTOs at the end of block and CALLs.

(define (reachable-from-block block)
  (match block
    ((('GOTO lbl-jmp)) `(,lbl-jmp))
    ((('IF t lbl-then 'ELSE lbl-else)) `(,lbl-then ,lbl-else))
    ((('LET v ('CALL lbl-call '*VIEW*))
      . block*) (union `(,lbl-call) (reachable-from-block block*)))
    ((('LET v e)
      . block*) (reachable-from-block block*))
    ((('RETURN e)) '())))

(e.g. (reachable-from-block (lookup '(0) test123)) ===> ((0 0)))
(e.g. (reachable-from-block (lookup '(0 0) test123)) ===> ((0 1) (1 0)))
(e.g. (reachable-from-block (lookup '(0 6) test123)) ===> ())
(e.g. (reachable-from-block (lookup '(1 6) test123)) ===> ((0))) ;; nb CALL!

;;; so now we can establish which labels are reachable from selected root
(define (live-labels root code)
  (let walk ((pending `(,root))
             (live '()))
    (match pending
      (() live)
      ((lbl . pending) (let* ((block (lookup lbl code))
                              (next (reachable-from-block block))
                              (next* (difference next live))
                              (pending* (union next* pending))
                              (live* (union live `(,lbl))))
                         (walk pending* live*))))))

(e.g. (live-labels '(0) test123)
      ===> ((2 0) (1 6) (1 5) (1 4) (1 3) (1 2) (1 1) (1 0)
            (0 6) (0 5) (0 4) (0 3) (0 2) (0 1) (0 0)
            (0)))

;;; ...and remove the dead code easily
(define (with-dead-code-removed code)
  (let* ((root (caar code))
         (live (live-labels root code)))
    (filter (lambda ((lbl . _)) (member? lbl live)) code)))

;;; now the ppoints-as-labels thing, but that's easy and we already had
;;; this procedure on the workbench:
(define (simplified-labels code)
  (let ((lbl-map (map (lambda (c i) (cons (car c) i))
                      code (iota (length code)))))
    (define (clause* c)
      (match c
        ((idx ('IF tst t-idx 'ELSE f-idx))
         `(,(lookup idx lbl-map) (IF ,tst ,(lookup t-idx lbl-map)
                                     ELSE ,(lookup f-idx lbl-map))))
        ((idx ('GOTO pp))
         `(,(lookup idx lbl-map) (GOTO ,(lookup pp lbl-map))))
      ((idx . cmds)
       `(,(lookup idx lbl-map) .
         ,(map (lambda (c)
                 (match c
                   (('LET v ('CALL pp '*VIEW*))
                    `(LET ,v (CALL ,(or (lookup `(,pp () ()) lbl-map)
                                        (lookup pp lbl-map))
                                   *VIEW*)))
                   (('GOTO pp) ;;; just-in-case...
                    `(GOTO ,(or (lookup `(,pp () ()) lbl-map)
                                (lookup pp lbl-map))))
                   (_ c))) cmds)))))
    (map clause* code)))

(e.g. (simplified-labels '((0weird (IF <test> (weird 1 2 3) ELSE (99 7)))
                           ((weird 1 2 3) (GOTO (99 7)))
                           ((99 7) (LET x (CALL (weird 1 2 3) *VIEW*))
                                   (RETURN 'whoa))))
     ===> ((0 (IF <test> 1 ELSE 2))
           (1 (GOTO 2))
           (2 (LET x (CALL 1 *VIEW*))
              (RETURN 'whoa))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5. this all sums up to very crude and super slow optimizer:
(define (optimized code)
  (simplified-labels
   (with-dead-code-removed
    (compressed-trasitions
     (residual '() '() code)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; without:
(e.g. (run test-p3 '(apd (q w e) (a s d))) ===> (q w e a s d))
;; with:
(e.g. (run (optimized test-p3) '(apd (q w e) (a s d)))
      ===> (q w e a s d))
;; without:
(e.g. (run  test-p3 '(map dbl (1 2 3))) ===> (2 4 6))
;; with:
(e.g. (run (optimized test-p3) '(map dbl (1 2 3))) ===> (2 4 6))
;; without:
(e.g. (run test-p3 '(map dup (- 0 ^)))
      ===> ((- . -) (0 . 0) (^ . ^)))
;; with:
(e.g. (run (optimized test-p3) '(map dup (- 0 ^)))
      ===> ((- . -) (0 . 0) (^ . ^)))
;; without:
(e.g. (run test-p3 '(rev (dercz likes pindolf)))
      ===> (pindolf likes dercz))
;; with:
(e.g. (run (optimized test-p3) '(rev (dercz likes pindolf)))
      ===> (pindolf likes dercz))

;;; these not only work but perform approx 80% of the original
;;; calls to (value expr env):

;;; p3 test   | w/out | with | ratio
;;; ----------+-------+------+---------
;;; - apd     |   477 |  389 | 0.81
;;; - map dbl |   918 |  736 | 0.80
;;; - map dup |   888 |  712 | 0.80
;;; - rev     |   505 |  342 | 0.67 (!)

;;; upd, PinP compiled with new version, 3-fold speedup
;;; when interpreting P3 tests:
;;; apd     | 122905 | 44395 | 0.36
;;; map dbl | 235431 | 85046 | 0.36
;;; map dup | 233925 | 84299 | 0.36
;;; rev     | 121685 | 44442 | 0.36

;;; it still seems to produce some garbage though. tbc

;;; upd 2024/03/13 i found it! for now it's just patched but already
;;; few hundred cals to (value _ _) less per run, and 10x less produced nodes
;;; (and produced pipn in 7min, not 40).
;;; test    |  bez   |  zzz  | r
;;; apd     | 122905 | 44299 | 0.360
;;; map dbl | 235431 | 84890 | 0.360
;;; map dup | 233925 | 84143 | 0.359
;;; rev     | 121685 | 44398 | 0.364

;;; with the new driver it'll be even smaller and faster since we're getting
;;; rid of post-processing phase, tbc!
