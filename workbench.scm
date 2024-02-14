(use-modules (grand scheme))
(add-to-load-path "./")
(define pindolf (@ (pindolf interpreter) pindolf))
(define parsed (@ (pindolf parser) parsed))
(define compiled (@ (pindolf compiler) compiled))
(define run (@ (pindolf vm) run))

(define pinp
  (with-input-from-file
      "examples/pindolf-self-interpreter.pndlf" read))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; first steps towards some kind of driving...

(define (lookup key #;in binding)
  (match binding
    (() #f)
    (((k . expr) . binding*) (if (equal? k key) expr
                              #;otherwise (lookup key binding*)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. getting rid of the obvious

(define (obvious code)
  (let ride ((pending `((,(car code)#;(assoc '(0) code) () ())))
             (result '())
             (visited '()))
    (match pending
      (()
       result)
      (((#f _ _) . pending*)
       (ride pending* result visited))
      ((((idx ('IF test t-idx 'ELSE f-idx)) passed failed) . pending*)
       (let ((idx* `(,idx ,passed ,failed)))
         (cond ((member? idx* visited)
                (ride pending* result visited))
               ((member? test passed)
                (let ((t-idx* `(,t-idx ,passed ,failed)))
                  (ride `((,(assoc t-idx code) ,passed ,failed)
                          . ,pending*)
                        `(,@result
                          (,idx* (GOTO ,t-idx*)))
                        `(,idx* . ,visited))))
               ((member? test failed)
                (let ((f-idx* `(,f-idx ,passed ,failed)))
                  (ride `((,(assoc f-idx code) ,passed ,failed)
                          . ,pending*)
                        `(,@result
                          (,idx* (GOTO ,f-idx*)))
                        `(,idx* . ,visited))))
               (else
                (let* ((passed* `(,test . ,passed))
                       (failed* `(,test . ,failed))
                       (t-idx* `(,t-idx ,passed* ,failed))
                       (f-idx* `(,f-idx ,passed ,failed*)))
                  (ride `((,(assoc t-idx code) ,passed* ,failed)
                          (,(assoc f-idx code) ,passed ,failed*)
                          . ,pending*)
                        `(,@result
                          (,idx* (IF ,test ,t-idx* ELSE ,f-idx*)))
                    `(,idx* . ,visited)))))))
      ((((idx ('GOTO pp)) passed failed) . pending*)
       (let ((idx* `(,idx ,passed ,failed))
             (pp* `(,pp ,passed ,failed)))
         (if (member? idx* visited)
             (ride pending* result visited)
             (ride `((,(assoc pp code) ,passed ,failed)
                     . ,pending*)
                   `(,@result (,idx* (GOTO ,pp*)))
                   `(,idx* . ,visited)))))
      ((((idx . lets-n-stuff) passed failed) . pending*)
       ;; calls don't get the ride yet (termination is tricky!)
       (let ((idx* `(,idx ,passed ,failed)))
         (if (member? idx* visited)
             (ride pending* result visited)
             (ride pending*
                   `(,@result (,idx* . ,lets-n-stuff))
                   `(,idx* . ,visited))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. getting rid of goto chains

(define (with-compressed-transitions code)
  (let walk ((pending `(,(caar code)))
             (code* '()))
    (match pending
      (()
       code*)
      ((idx . pending*)
       (if (lookup idx code*) ;;; already visited?
           (walk pending* code*)
           (let walk-block ((block (lookup idx code))
                            (block* '()))
             (match block
               ((('LET v e) . block**)
                (walk-block block** `(,@block* (LET ,v ,e))))
               ((('GOTO pp))
                (walk-block (lookup pp code) block*))
               ((('RETURN e))
                (walk pending*
                      `(,@code*
                        (,idx ,@block*
                              (RETURN ,e)))))
               ((('IF tst t-idx 'ELSE f-idx))
                (walk `(,t-idx ,f-idx . ,pending*)
                      `(,@code*
                        (,idx ,@block*
                              (IF ,tst ,t-idx ELSE ,f-idx))))))))))))


(e.g. (with-compressed-transitions
       '((0 (IF <test1> 1 ELSE 2))
         (1 (GOTO 2))
         (2 (RETURN 23))))
      ===> ((0 (IF <test1> 1 ELSE 2))
            (1 (RETURN 23))
            (2 (RETURN 23))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. duplicated (behaviorally identical) blocks should go, no?

;;; (a) we might not want to do this (at most after driving),
;;; (b) but if we did i only had two ideas, the greedy one (prolly
;;; not suitable) of grouping all the identically-shaped blocks and
;;; picking lowest label L for them, then replacing all references
;;; (GOTOs and CALLs) to any from the group with L, and then
;;; repeating the process until there are no more replacements
;;; (ie each grouping has only one member)...
;;; ...and the weird one with identifying each leaf with the
;;; conditions of reaching it but that'll go into separate file
;;; for now since it's a pretty bloated idea.

;;; there's no need to worry since it seems we won't need that
;;; (obvious _) thing soon.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. replace weird labels with natural numbers
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
;;; nice.
(e.g. (simplified-labels
       (with-compressed-transitions
        (obvious
         (compiled '( (('APD    ()      ?ys) ys)
                      (('APD (?x . ?xs) ?ys) `(,x . ,(& (APD ,xs ,ys))))
                      )))))
      ===>
      ((0 (IF (CONS? *VIEW*) 1 ELSE 22))
       (1 (IF (EQ? (CAR *VIEW*) 'APD) 2 ELSE 21))
       (2 (IF (CONS? (CDR *VIEW*)) 3 ELSE 20))
       (3 (IF (NIL? (CAR (CDR *VIEW*))) 4 ELSE 13))
       (4 (IF (CONS? (CDR (CDR *VIEW*))) 5 ELSE 10))
       (5 (IF (NIL? (CDR (CDR (CDR *VIEW*)))) 6 ELSE 7))
       (6 (LET ys (CAR (CDR (CDR *VIEW*))))
          (RETURN ys))
       (7 (IF (CONS? (CAR (CDR *VIEW*))) 8 ELSE 9))
       (8 (RETURN 'no-match))
       (9 (RETURN 'no-match))
       (10 (IF (CONS? (CAR (CDR *VIEW*))) 11 ELSE 12))
       (11 (RETURN 'no-match))
       (12 (RETURN 'no-match))
       (13 (IF (CONS? (CAR (CDR *VIEW*))) 14 ELSE 19))
       (14 (IF (CONS? (CDR (CDR *VIEW*))) 15 ELSE 18))
       (15 (IF (NIL? (CDR (CDR (CDR *VIEW*)))) 16 ELSE 17))
       (16 (LET ys (CAR (CDR (CDR *VIEW*))))
           (LET xs (CDR (CAR (CDR *VIEW*))))
           (LET x (CAR (CAR (CDR *VIEW*))))
           (LET *VIEW* (CONS 'APD (CONS xs (CONS ys ()))))
           (LET (V 0) (CALL 0 *VIEW*))
           (RETURN (CONS x (V 0))))
       (17 (RETURN 'no-match))
       (18 (RETURN 'no-match))
       (19 (RETURN 'no-match))
       (20 (RETURN 'no-match))
       (21 (RETURN 'no-match))
       (22 (RETURN 'no-match))))

;;; ok, almost there...



            
(e.g. 
 (run (simplified-labels
       (with-compressed-transitions
        (obvious
         (compiled '( (('APD    ()      ?ys) ys)
                      (('APD (?x . ?xs) ?ys) `(,x . ,(& (APD ,xs ,ys))))
                 )))))
      '(APD (q w e) (1 2 3)))
 ===> (q w e 1 2 3))
