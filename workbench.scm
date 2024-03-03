(use-modules (grand scheme))
(add-to-load-path "./")
(define pindolf (@ (pindolf interpreter) pindolf))
(define parsed (@ (pindolf parser) parsed))
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

(define test-p3
  (compiled
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
     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; first steps towards some kind of driving, for redundant tests
;;; removal to save some calls to <value> etc.

;;; 1. analysis of decission tree:

(define (successors label code) ;;; ONLY for IF nodes!
  (match (lookup label code)
    ((('IF _ l 'ELSE l*)) `(,l ,l*))
    ((('GOTO l)) `(,l)) ;; again, just for bootstrap block?
    (_ '())))

(e.g. (successors '(0 5) test123) ===> ((0 6) (1 0)))

;;; so we want to know for each block which tests are still
;;; alive i.e. worth remembering (is there any chance the
;;; result of this test will be needed on any path from given
;;; block). it's a bit like LVA, only simpler.

(define (live-tests code)
  (let* ((init-tests (map (lambda (block)
                            (match block
                              ((lbl . (('IF t _ 'ELSE _))) `(,lbl . (,t)))
                              ((lbl . _) `(,lbl . ()))))
                          code))
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
                 (my-tests* (union my-tests inherited-tests))
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
         (passed* (intersection passed live))
         (failed* (intersection failed live)))
    `(,lbl ,passed* ,failed*)))

(e.g. (ppoint '(2 3)
              '(test1 test3)
              '(test2 test4)
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

;;; now THIS is the core of our poor man's driving:

(define (new-block pp code LVA) ;; assert (equal? LVA (live-tests code)) y'know
  (let* ((lbl (pp-lbl pp))
         (passed (pp-passed pp))
         (failed (pp-failed pp))
         (old-code (lookup (pp-lbl pp) code)))
  (match old-code 
    ((('IF test lbl-then 'ELSE lbl-else))
     (cond ((member? test passed)
            (let* ((pp-then (ppoint lbl-then passed failed LVA))
                   (new-code `((GOTO ,pp-then))))
              `(,pp . ,new-code)))
           ((member? test failed)
            (let* ((pp-else (ppoint lbl-else passed failed LVA))
                   (new-code `((GOTO ,pp-else))))
              `(,pp . ,new-code)))
           (else
            (let* ((pp-then
                    (ppoint lbl-then `(,test . ,passed) failed LVA))
                   (pp-else
                    (ppoint lbl-else passed `(,test . ,failed) LVA))
                   (new-code
                    `((IF ,test ,pp-then ELSE ,pp-else))))
              `(,pp . ,new-code)))))
    ((('GOTO lbl-jmp))
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
            (live-tests test123))
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
            (live-tests test123))
 ===> ([(0 2) ((CONS? (CDR *VIEW*))) ()]
       (GOTO [(0 3) ((CONS? (CDR *VIEW*))) ()])))
;;; so there's no need to build IF statement, only jump to "true branch",
;;; one specialized wrt cons?[cdr]

;;; on the contrary, if we knew cons?[cdr] wasn't satisfied:
(e.g.
 (new-block (ppoint '(0 2) '() '((CONS? (CDR *VIEW*))) (live-tests test123))
            test123
            (live-tests test123))
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
                                test123 (live-tests test123))
      ===> ([(0 2) () ()]
            (IF (CONS? (CDR *VIEW*)) [(0 3) ((CONS? (CDR *VIEW*))) ()]
                ELSE [(1 0) () ((CONS? (CDR *VIEW*)))])))
;;; and we can easily get both possible futures (ppoints) of it:
(e.g. (successor-pps (new-block (ppoint '(0 2) '() '() (live-tests test123))
                                test123 (live-tests test123)))
      ===> ([(0 3) ((CONS? (CDR *VIEW*))) ()]
            [(1 0) () ((CONS? (CDR *VIEW*)))]))
;;; sweet.

;;; surprisingly that was all needed to process entire compiled program
(define (residual passed failed code)
  (let ((first-lbl (caar code)) ;; XD
        (LTA (live-tests code)))
    (let ride ((pending `(,(ppoint first-lbl passed failed LTA)))
               (new-code '()))
      (match pending
        (() new-code)
        ((pp . pending*) (match (lookup pp new-code)
                           (#f (let* ((block (new-block pp code LTA))
                                      (futures (successor-pps block)))
                                 (ride (append futures pending*)
                                       `(,@new-code ,block))))
                           (_ (ride pending* new-code))))))))

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
;;; this all sums up to very crude and super slow optimizer:
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

;;; we expect the ratio to go lower as the programs get bigger and
;;; more interpretive; however as of now it seems to get stuck when
;;; trying to compile lisp0, which quite well might be combinatorial
;;; explossion. we have another idea but, again, that'll have to wait
;;; for approx. 2 weeks... tbc!


