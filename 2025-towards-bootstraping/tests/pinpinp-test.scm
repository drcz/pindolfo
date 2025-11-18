;;; this is just an experiment since (read) struggled with too long expressions
;;; a compiled PinP running PinP interpreting DRC running the double append!
;;; takes 0.4s on my machine! \o/
;;; ---------------------------------------------------------------------------

;; pindolfo2scm v1.2
(define (LEAF0 v-expression v-pattern)
  (let* ((a-0 (DISPATCH24 v-expression v-pattern))) a-0))
(define (LEAF1 v-bnd) v-bnd)
(define (LEAF2 v-bnd) v-bnd)
(define (LEAF3 v-bnd v-n) v-bnd)
(define (LEAF4 v-bnd v-e) v-bnd)
(define (LEAF5 v-bnd v-e v-nv) (let* ((a-0 (DISPATCH23 v-e v-bnd v-nv))) a-0))
(define (LEAF6) 'NO-MATCH)
(define (LEAF7 v-bnd v-e v-sv) (let* ((a-0 (DISPATCH22 v-e v-bnd v-sv))) a-0))
(define (LEAF8) 'NO-MATCH)
(define (LEAF9 v-bnd v-e v-av) (let* ((a-0 (DISPATCH21 v-e v-bnd v-av))) a-0))
(define (LEAF10) 'NO-MATCH)
(define (LEAF11 v-bnd v-e v-ev) (let* ((a-0 (DISPATCH20 v-e v-bnd v-ev))) a-0))
(define (LEAF12 v-bnd v-es v-e v-ps v-p)
  (let* ((a-0 (DISPATCH19 v-e v-bnd v-p)) (a-1 (DISPATCH18 v-ps v-es a-0)))
    a-1))
(define (LEAF13) 'NO-MATCH)
(define (LEAF14) 'NO-MATCH)
(define (LEAF15 v-e v-p v-bnd) (let* ((a-0 (DISPATCH19 v-e v-bnd v-p))) a-0))
(define (LEAF16 v-bnd v-e v-v)
  (let* ((a-0 (DISPATCH17 v-bnd v-v)) (a-1 (DISPATCH16 v-v v-bnd v-e a-0)))
    a-1))
(define (LEAF17 v-bnd v-e v-v) (cons (cons v-v v-e) v-bnd))
(define (LEAF18 v-bnd v-v v-e) v-bnd)
(define (LEAF19) 'NO-MATCH)
(define (LEAF20 v-v v-k) (cons 'JUST (cons v-v '())))
(define (LEAF21 v-bnd v-k) (let* ((a-0 (DISPATCH15 v-bnd v-k))) a-0))
(define (LEAF22) '(NONE))
(define (LEAF23) '())
(define (LEAF24 v-n) v-n)
(define (LEAF25 v-e) v-e)
(define (LEAF26 v-app v-bnd v-qq)
  (let* ((a-0 (DISPATCH14 v-bnd v-app v-qq))) a-0))
(define (LEAF27 v-app v-bnd v-e* v-e)
  (let* ((a-0 (DISPATCH13 v-bnd v-app v-e))
         (a-1 (DISPATCH12 v-bnd v-app v-e*)))
    (+ a-0 a-1)))
(define (LEAF28 v-app v-bnd v-e* v-e)
  (let* ((a-0 (DISPATCH13 v-bnd v-app v-e))
         (a-1 (DISPATCH12 v-bnd v-app v-e*)))
    (- a-0 a-1)))
(define (LEAF29 v-app v-bnd v-e* v-e)
  (let* ((a-0 (DISPATCH13 v-bnd v-app v-e))
         (a-1 (DISPATCH12 v-bnd v-app v-e*)))
    (* a-0 a-1)))
(define (LEAF30 v-app v-bnd v-a)
  (let* ((a-0 (DISPATCH11 v-bnd v-app v-a)) (a-1 (DISPATCH10 a-0 v-app))) a-1))
(define (LEAF31 v-bnd v-k)
  (let* ((a-0 (DISPATCH15 v-bnd v-k)) (a-1 (DISPATCH9 a-0 v-k))) a-1))
(define (LEAF32 v-k) (cons 'unbound (cons v-k '())))
(define (LEAF33 v-v) v-v)
(define (LEAF34 v-app v-bnd v-e)
  (let* ((a-0 (DISPATCH13 v-bnd v-app v-e))) a-0))
(define (LEAF35 v-app v-bnd v-t v-h)
  (let* ((a-0 (DISPATCH8 v-bnd v-app v-h)) (a-1 (DISPATCH7 v-bnd v-app v-t)))
    (cons a-0 a-1)))
(define (LEAF36 v-e) v-e)
(define (LEAF37 v-prg v-e) (let* ((a-0 (DISPATCH6 v-prg v-e))) a-0))
(define (LEAF38 v-e) (cons 'NO-MATCH (cons v-e '())))
(define (LEAF39 v-prg v-prg* v-exp v-pat v-e)
  (let* ((a-0 (DISPATCH5 v-e v-pat))
         (a-1 (DISPATCH4 a-0 v-prg* v-prg v-exp v-e)))
    a-1))
(define (LEAF40 v-prg v-prg* v-e)
  (let* ((a-0 (DISPATCH3 v-prg* v-prg v-e))) a-0))
(define (LEAF41 v-prg v-e* v-bnd)
  (let* ((a-0 (DISPATCH2 v-bnd v-prg v-e*))) a-0))
(define (LEAF42 v-e v-prg) (let* ((a-0 (DISPATCH1 v-prg v-e))) a-0))
(define (DISPATCH0 v0)
  (if (pair? v0)
      (if (equal? 'match? (car v0))
          (if (pair? (cdr v0))
              (if (pair? (cdr (cdr v0)))
                  (if (null? (cdr (cdr (cdr v0))))
                      (LEAF0 (car (cdr (cdr v0))) (car (cdr v0)))
                      (if (null? (car (cdr v0)))
                          (if (null? (car (cdr (cdr v0))))
                              (if (pair? (cdr (cdr (cdr v0))))
                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                      (LEAF1 (car (cdr (cdr (cdr v0)))))
                                      (FAIL v0))
                                  (FAIL v0))
                              (if (pair? (cdr (cdr (cdr v0))))
                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                      (LEAF13)
                                      (FAIL v0))
                                  (FAIL v0)))
                          (if (equal? '_ (car (cdr v0)))
                              (if (pair? (cdr (cdr (cdr v0))))
                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                      (LEAF2 (car (cdr (cdr (cdr v0)))))
                                      (FAIL v0))
                                  (FAIL v0))
                              (if (number? (car (cdr v0)))
                                  (if (equal?
                                       (car (cdr v0))
                                       (car (cdr (cdr v0))))
                                      (if (pair? (cdr (cdr (cdr v0))))
                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                              (LEAF3 (car (cdr (cdr (cdr v0))))
                                                     (car (cdr v0)))
                                              (FAIL v0))
                                          (FAIL v0))
                                      (if (pair? (cdr (cdr (cdr v0))))
                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                              (LEAF13)
                                              (FAIL v0))
                                          (FAIL v0)))
                                  (if (pair? (car (cdr v0)))
                                      (if (equal? 'quote (car (car (cdr v0))))
                                          (if (pair? (cdr (car (cdr v0))))
                                              (if (null? (cdr (cdr (car (cdr v0)))))
                                                  (if (equal?
                                                       (car (cdr (car (cdr v0))))
                                                       (car (cdr (cdr v0))))
                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                              (LEAF4 (car (cdr (cdr (cdr v0))))
                                                                     (car (cdr (car (cdr v0)))))
                                                              (FAIL v0))
                                                          (FAIL v0))
                                                      (if (pair? (car (cdr (cdr v0))))
                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                  (LEAF12
                                                                   (car (cdr (cdr (cdr v0))))
                                                                   (cdr (car (cdr (cdr v0))))
                                                                   (car (car (cdr (cdr v0))))
                                                                   (cdr (car (cdr v0)))
                                                                   (car (car (cdr v0))))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                  (LEAF13)
                                                                  (FAIL v0))
                                                              (FAIL v0))))
                                                  (if (pair? (car (cdr (cdr v0))))
                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                              (LEAF12
                                                               (car (cdr (cdr (cdr v0))))
                                                               (cdr (car (cdr (cdr v0))))
                                                               (car (car (cdr (cdr v0))))
                                                               (cdr (car (cdr v0)))
                                                               (car (car (cdr v0))))
                                                              (FAIL v0))
                                                          (FAIL v0))
                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                              (LEAF13)
                                                              (FAIL v0))
                                                          (FAIL v0))))
                                              (if (pair? (car (cdr (cdr v0))))
                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                          (LEAF12
                                                           (car (cdr (cdr (cdr v0))))
                                                           (cdr (car (cdr (cdr v0))))
                                                           (car (car (cdr (cdr v0))))
                                                           (cdr (car (cdr v0)))
                                                           (car (car (cdr v0))))
                                                          (FAIL v0))
                                                      (FAIL v0))
                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                          (LEAF13)
                                                          (FAIL v0))
                                                      (FAIL v0))))
                                          (if (equal?
                                               'num
                                               (car (car (cdr v0))))
                                              (if (pair? (cdr (car (cdr v0))))
                                                  (if (symbol?
                                                       (car (cdr (car (cdr v0)))))
                                                      (if (null? (cdr (cdr (car (cdr v0)))))
                                                          (if (number?
                                                               (car (cdr (cdr v0))))
                                                              (if (pair? (cdr (cdr (cdr v0))))
                                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                      (LEAF5 (car (cdr (cdr (cdr v0))))
                                                                             (car (cdr (cdr v0)))
                                                                             (car (cdr (car (cdr v0)))))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (if (pair? (cdr (cdr (cdr v0))))
                                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                      (LEAF6)
                                                                      (FAIL v0))
                                                                  (FAIL v0)))
                                                          (if (pair? (car (cdr (cdr v0))))
                                                              (if (pair? (cdr (cdr (cdr v0))))
                                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                      (LEAF12
                                                                       (car (cdr (cdr (cdr v0))))
                                                                       (cdr (car (cdr (cdr v0))))
                                                                       (car (car (cdr (cdr v0))))
                                                                       (cdr (car (cdr v0)))
                                                                       (car (car (cdr v0))))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (if (pair? (cdr (cdr (cdr v0))))
                                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                      (LEAF13)
                                                                      (FAIL v0))
                                                                  (FAIL v0))))
                                                      (if (null? (cdr (cdr (car (cdr v0)))))
                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                  (LEAF6)
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (if (pair? (car (cdr (cdr v0))))
                                                              (if (pair? (cdr (cdr (cdr v0))))
                                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                      (LEAF12
                                                                       (car (cdr (cdr (cdr v0))))
                                                                       (cdr (car (cdr (cdr v0))))
                                                                       (car (car (cdr (cdr v0))))
                                                                       (cdr (car (cdr v0)))
                                                                       (car (car (cdr v0))))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (if (pair? (cdr (cdr (cdr v0))))
                                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                      (LEAF13)
                                                                      (FAIL v0))
                                                                  (FAIL v0)))))
                                                  (if (pair? (car (cdr (cdr v0))))
                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                              (LEAF12
                                                               (car (cdr (cdr (cdr v0))))
                                                               (cdr (car (cdr (cdr v0))))
                                                               (car (car (cdr (cdr v0))))
                                                               (cdr (car (cdr v0)))
                                                               (car (car (cdr v0))))
                                                              (FAIL v0))
                                                          (FAIL v0))
                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                              (LEAF13)
                                                              (FAIL v0))
                                                          (FAIL v0))))
                                              (if (equal?
                                                   'sym
                                                   (car (car (cdr v0))))
                                                  (if (pair? (cdr (car (cdr v0))))
                                                      (if (symbol?
                                                           (car (cdr (car (cdr v0)))))
                                                          (if (null? (cdr (cdr (car (cdr v0)))))
                                                              (if (symbol?
                                                                   (car (cdr (cdr v0))))
                                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                          (LEAF7 (car (cdr (cdr (cdr v0))))
                                                                                 (car (cdr (cdr v0)))
                                                                                 (car (cdr (car (cdr v0)))))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                          (LEAF8)
                                                                          (FAIL v0))
                                                                      (FAIL v0)))
                                                              (if (pair? (car (cdr (cdr v0))))
                                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                          (LEAF12
                                                                           (car (cdr (cdr (cdr v0))))
                                                                           (cdr (car (cdr (cdr v0))))
                                                                           (car (car (cdr (cdr v0))))
                                                                           (cdr (car (cdr v0)))
                                                                           (car (car (cdr v0))))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                          (LEAF13)
                                                                          (FAIL v0))
                                                                      (FAIL v0))))
                                                          (if (null? (cdr (cdr (car (cdr v0)))))
                                                              (if (pair? (cdr (cdr (cdr v0))))
                                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                      (LEAF8)
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (if (pair? (car (cdr (cdr v0))))
                                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                          (LEAF12
                                                                           (car (cdr (cdr (cdr v0))))
                                                                           (cdr (car (cdr (cdr v0))))
                                                                           (car (car (cdr (cdr v0))))
                                                                           (cdr (car (cdr v0)))
                                                                           (car (car (cdr v0))))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                          (LEAF13)
                                                                          (FAIL v0))
                                                                      (FAIL v0)))))
                                                      (if (pair? (car (cdr (cdr v0))))
                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                  (LEAF12
                                                                   (car (cdr (cdr (cdr v0))))
                                                                   (cdr (car (cdr (cdr v0))))
                                                                   (car (car (cdr (cdr v0))))
                                                                   (cdr (car (cdr v0)))
                                                                   (car (car (cdr v0))))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                  (LEAF13)
                                                                  (FAIL v0))
                                                              (FAIL v0))))
                                                  (if (equal?
                                                       'atm
                                                       (car (car (cdr v0))))
                                                      (if (pair? (cdr (car (cdr v0))))
                                                          (if (symbol?
                                                               (car (cdr (car (cdr v0)))))
                                                              (if (null? (cdr (cdr (car (cdr v0)))))
                                                                  (if (not (pair? (car (cdr (cdr v0)))))
                                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                              (LEAF9 (car (cdr (cdr (cdr v0))))
                                                                                     (car (cdr (cdr v0)))
                                                                                     (car (cdr (car (cdr v0)))))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                              (LEAF10)
                                                                              (FAIL v0))
                                                                          (FAIL v0)))
                                                                  (if (pair? (car (cdr (cdr v0))))
                                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                              (LEAF12
                                                                               (car (cdr (cdr (cdr v0))))
                                                                               (cdr (car (cdr (cdr v0))))
                                                                               (car (car (cdr (cdr v0))))
                                                                               (cdr (car (cdr v0)))
                                                                               (car (car (cdr v0))))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                              (LEAF13)
                                                                              (FAIL v0))
                                                                          (FAIL v0))))
                                                              (if (null? (cdr (cdr (car (cdr v0)))))
                                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                          (LEAF10)
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (if (pair? (car (cdr (cdr v0))))
                                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                              (LEAF12
                                                                               (car (cdr (cdr (cdr v0))))
                                                                               (cdr (car (cdr (cdr v0))))
                                                                               (car (car (cdr (cdr v0))))
                                                                               (cdr (car (cdr v0)))
                                                                               (car (car (cdr v0))))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                              (LEAF13)
                                                                              (FAIL v0))
                                                                          (FAIL v0)))))
                                                          (if (pair? (car (cdr (cdr v0))))
                                                              (if (pair? (cdr (cdr (cdr v0))))
                                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                      (LEAF12
                                                                       (car (cdr (cdr (cdr v0))))
                                                                       (cdr (car (cdr (cdr v0))))
                                                                       (car (car (cdr (cdr v0))))
                                                                       (cdr (car (cdr v0)))
                                                                       (car (car (cdr v0))))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (if (pair? (cdr (cdr (cdr v0))))
                                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                      (LEAF13)
                                                                      (FAIL v0))
                                                                  (FAIL v0))))
                                                      (if (equal?
                                                           'exp
                                                           (car (car (cdr v0))))
                                                          (if (pair? (cdr (car (cdr v0))))
                                                              (if (symbol?
                                                                   (car (cdr (car (cdr v0)))))
                                                                  (if (null? (cdr (cdr (car (cdr v0)))))
                                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                              (LEAF11
                                                                               (car (cdr (cdr (cdr v0))))
                                                                               (car (cdr (cdr v0)))
                                                                               (car (cdr (car (cdr v0)))))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (if (pair? (car (cdr (cdr v0))))
                                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                                  (LEAF12
                                                                                   (car (cdr (cdr (cdr v0))))
                                                                                   (cdr (car (cdr (cdr v0))))
                                                                                   (car (car (cdr (cdr v0))))
                                                                                   (cdr (car (cdr v0)))
                                                                                   (car (car (cdr v0))))
                                                                                  (FAIL v0))
                                                                              (FAIL v0))
                                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                                  (LEAF13)
                                                                                  (FAIL v0))
                                                                              (FAIL v0))))
                                                                  (if (pair? (car (cdr (cdr v0))))
                                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                              (LEAF12
                                                                               (car (cdr (cdr (cdr v0))))
                                                                               (cdr (car (cdr (cdr v0))))
                                                                               (car (car (cdr (cdr v0))))
                                                                               (cdr (car (cdr v0)))
                                                                               (car (car (cdr v0))))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                              (LEAF13)
                                                                              (FAIL v0))
                                                                          (FAIL v0))))
                                                              (if (pair? (car (cdr (cdr v0))))
                                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                          (LEAF12
                                                                           (car (cdr (cdr (cdr v0))))
                                                                           (cdr (car (cdr (cdr v0))))
                                                                           (car (car (cdr (cdr v0))))
                                                                           (cdr (car (cdr v0)))
                                                                           (car (car (cdr v0))))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                          (LEAF13)
                                                                          (FAIL v0))
                                                                      (FAIL v0))))
                                                          (if (pair? (car (cdr (cdr v0))))
                                                              (if (pair? (cdr (cdr (cdr v0))))
                                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                      (LEAF12
                                                                       (car (cdr (cdr (cdr v0))))
                                                                       (cdr (car (cdr (cdr v0))))
                                                                       (car (car (cdr (cdr v0))))
                                                                       (cdr (car (cdr v0)))
                                                                       (car (car (cdr v0))))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (if (pair? (cdr (cdr (cdr v0))))
                                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                      (LEAF13)
                                                                      (FAIL v0))
                                                                  (FAIL v0))))))))
                                      (if (pair? (cdr (cdr (cdr v0))))
                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                              (LEAF13)
                                              (FAIL v0))
                                          (FAIL v0)))))))
                  (FAIL v0))
              (FAIL v0))
          (if (equal? 'match?* (car v0))
              (if (pair? (cdr v0))
                  (if (equal? 'NO-MATCH (car (cdr v0)))
                      (if (pair? (cdr (cdr v0)))
                          (if (pair? (cdr (cdr (cdr v0))))
                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                  (LEAF14)
                                  (FAIL v0))
                              (FAIL v0))
                          (FAIL v0))
                      (if (pair? (cdr (cdr v0)))
                          (if (pair? (cdr (cdr (cdr v0))))
                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                  (LEAF15
                                   (car (cdr (cdr (cdr v0))))
                                   (car (cdr (cdr v0)))
                                   (car (cdr v0)))
                                  (FAIL v0))
                              (FAIL v0))
                          (FAIL v0)))
                  (FAIL v0))
              (if (equal? 'match-var (car v0))
                  (if (pair? (cdr v0))
                      (if (symbol? (car (cdr v0)))
                          (if (pair? (cdr (cdr v0)))
                              (if (pair? (cdr (cdr (cdr v0))))
                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                      (LEAF16
                                       (car (cdr (cdr (cdr v0))))
                                       (car (cdr (cdr v0)))
                                       (car (cdr v0)))
                                      (FAIL v0))
                                  (FAIL v0))
                              (FAIL v0))
                          (FAIL v0))
                      (FAIL v0))
                  (if (equal? 'match-var* (car v0))
                      (if (pair? (cdr v0))
                          (if (pair? (car (cdr v0)))
                              (if (equal? 'NONE (car (car (cdr v0))))
                                  (if (null? (cdr (car (cdr v0))))
                                      (if (pair? (cdr (cdr v0)))
                                          (if (symbol? (car (cdr (cdr v0))))
                                              (if (pair? (cdr (cdr (cdr v0))))
                                                  (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                      (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                          (LEAF17
                                                           (car (cdr (cdr (cdr (cdr v0)))))
                                                           (car (cdr (cdr (cdr v0))))
                                                           (car (cdr (cdr v0))))
                                                          (FAIL v0))
                                                      (FAIL v0))
                                                  (FAIL v0))
                                              (FAIL v0))
                                          (FAIL v0))
                                      (FAIL v0))
                                  (if (equal? 'JUST (car (car (cdr v0))))
                                      (if (pair? (cdr (car (cdr v0))))
                                          (if (null? (cdr (cdr (car (cdr v0)))))
                                              (if (pair? (cdr (cdr v0)))
                                                  (if (symbol?
                                                       (car (cdr (cdr v0))))
                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                          (if (equal?
                                                               (car (cdr (car (cdr v0))))
                                                               (car (cdr (cdr (cdr v0)))))
                                                              (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                  (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                      (LEAF18
                                                                       (car (cdr (cdr (cdr (cdr v0)))))
                                                                       (car (cdr (cdr v0)))
                                                                       (car (cdr (car (cdr v0)))))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                  (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                      (LEAF19)
                                                                      (FAIL v0))
                                                                  (FAIL v0)))
                                                          (FAIL v0))
                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                          (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                              (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                  (LEAF19)
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (FAIL v0)))
                                                  (FAIL v0))
                                              (FAIL v0))
                                          (FAIL v0))
                                      (FAIL v0)))
                              (FAIL v0))
                          (FAIL v0))
                      (if (equal? 'lookup (car v0))
                          (if (pair? (cdr v0))
                              (if (symbol? (car (cdr v0)))
                                  (if (pair? (cdr (cdr v0)))
                                      (if (pair? (car (cdr (cdr v0))))
                                          (if (pair? (car (car (cdr (cdr v0)))))
                                              (if (equal?
                                                   (car (cdr v0))
                                                   (car (car (car (cdr (cdr v0))))))
                                                  (if (null? (cdr (cdr (cdr v0))))
                                                      (LEAF20
                                                       (cdr (car (car (cdr (cdr v0)))))
                                                       (car (cdr v0)))
                                                      (FAIL v0))
                                                  (if (null? (cdr (cdr (cdr v0))))
                                                      (LEAF21
                                                       (cdr (car (cdr (cdr v0))))
                                                       (car (cdr v0)))
                                                      (FAIL v0)))
                                              (FAIL v0))
                                          (if (null? (car (cdr (cdr v0))))
                                              (if (null? (cdr (cdr (cdr v0))))
                                                  (LEAF22)
                                                  (FAIL v0))
                                              (FAIL v0)))
                                      (FAIL v0))
                                  (if (pair? (cdr (cdr v0)))
                                      (if (null? (car (cdr (cdr v0))))
                                          (if (null? (cdr (cdr (cdr v0))))
                                              (LEAF22)
                                              (FAIL v0))
                                          (FAIL v0))
                                      (FAIL v0)))
                              (FAIL v0))
                          (if (equal? 'value (car v0))
                              (if (pair? (cdr v0))
                                  (if (null? (car (cdr v0)))
                                      (if (pair? (cdr (cdr v0)))
                                          (if (pair? (cdr (cdr (cdr v0))))
                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                  (LEAF23)
                                                  (FAIL v0))
                                              (FAIL v0))
                                          (FAIL v0))
                                      (if (number? (car (cdr v0)))
                                          (if (pair? (cdr (cdr v0)))
                                              (if (pair? (cdr (cdr (cdr v0))))
                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                      (LEAF24 (car (cdr v0)))
                                                      (FAIL v0))
                                                  (FAIL v0))
                                              (FAIL v0))
                                          (if (pair? (car (cdr v0)))
                                              (if (equal?
                                                   'quote
                                                   (car (car (cdr v0))))
                                                  (if (pair? (cdr (car (cdr v0))))
                                                      (if (null? (cdr (cdr (car (cdr v0)))))
                                                          (if (pair? (cdr (cdr v0)))
                                                              (if (pair? (cdr (cdr (cdr v0))))
                                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                      (LEAF25
                                                                       (car (cdr (car (cdr v0)))))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (FAIL v0))
                                                      (FAIL v0))
                                                  (if (equal?
                                                       'quasiquote
                                                       (car (car (cdr v0))))
                                                      (if (pair? (cdr (car (cdr v0))))
                                                          (if (null? (cdr (cdr (car (cdr v0)))))
                                                              (if (pair? (cdr (cdr v0)))
                                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                          (LEAF26
                                                                           (car (cdr (cdr (cdr v0))))
                                                                           (car (cdr (cdr v0)))
                                                                           (car (cdr (car (cdr v0)))))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (FAIL v0))
                                                      (if (equal?
                                                           '+
                                                           (car (car (cdr v0))))
                                                          (if (pair? (cdr (car (cdr v0))))
                                                              (if (pair? (cdr (cdr (car (cdr v0)))))
                                                                  (if (null? (cdr (cdr (cdr (car (cdr v0))))))
                                                                      (if (pair? (cdr (cdr v0)))
                                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                                  (LEAF27
                                                                                   (car (cdr (cdr (cdr v0))))
                                                                                   (car (cdr (cdr v0)))
                                                                                   (car (cdr (cdr (car (cdr v0)))))
                                                                                   (car (cdr (car (cdr v0)))))
                                                                                  (FAIL v0))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (if (equal?
                                                               '-
                                                               (car (car (cdr v0))))
                                                              (if (pair? (cdr (car (cdr v0))))
                                                                  (if (pair? (cdr (cdr (car (cdr v0)))))
                                                                      (if (null? (cdr (cdr (cdr (car (cdr v0))))))
                                                                          (if (pair? (cdr (cdr v0)))
                                                                              (if (pair? (cdr (cdr (cdr v0))))
                                                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                                      (LEAF28
                                                                                       (car (cdr (cdr (cdr v0))))
                                                                                       (car (cdr (cdr v0)))
                                                                                       (car (cdr (cdr (car (cdr v0)))))
                                                                                       (car (cdr (car (cdr v0)))))
                                                                                      (FAIL v0))
                                                                                  (FAIL v0))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (if (equal?
                                                                   '*
                                                                   (car (car (cdr v0))))
                                                                  (if (pair? (cdr (car (cdr v0))))
                                                                      (if (pair? (cdr (cdr (car (cdr v0)))))
                                                                          (if (null? (cdr (cdr (cdr (car (cdr v0))))))
                                                                              (if (pair? (cdr (cdr v0)))
                                                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                                          (LEAF29
                                                                                           (car (cdr (cdr (cdr v0))))
                                                                                           (car (cdr (cdr v0)))
                                                                                           (car (cdr (cdr (car (cdr v0)))))
                                                                                           (car (cdr (car (cdr v0)))))
                                                                                          (FAIL v0))
                                                                                      (FAIL v0))
                                                                                  (FAIL v0))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (if (equal?
                                                                       'rec
                                                                       (car (car (cdr v0))))
                                                                      (if (pair? (cdr (car (cdr v0))))
                                                                          (if (null? (cdr (cdr (car (cdr v0)))))
                                                                              (if (pair? (cdr (cdr v0)))
                                                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                                          (LEAF30
                                                                                           (car (cdr (cdr (cdr v0))))
                                                                                           (car (cdr (cdr v0)))
                                                                                           (car (cdr (car (cdr v0)))))
                                                                                          (FAIL v0))
                                                                                      (FAIL v0))
                                                                                  (FAIL v0))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (FAIL v0)))))))
                                              (if (symbol? (car (cdr v0)))
                                                  (if (pair? (cdr (cdr v0)))
                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                              (LEAF31
                                                               (car (cdr (cdr v0)))
                                                               (car (cdr v0)))
                                                              (FAIL v0))
                                                          (FAIL v0))
                                                      (FAIL v0))
                                                  (FAIL v0)))))
                                  (FAIL v0))
                              (if (equal? 'val-lu (car v0))
                                  (if (pair? (cdr v0))
                                      (if (symbol? (car (cdr v0)))
                                          (if (pair? (cdr (cdr v0)))
                                              (if (pair? (car (cdr (cdr v0))))
                                                  (if (equal?
                                                       'NONE
                                                       (car (car (cdr (cdr v0)))))
                                                      (if (null? (cdr (car (cdr (cdr v0)))))
                                                          (if (null? (cdr (cdr (cdr v0))))
                                                              (LEAF32
                                                               (car (cdr v0)))
                                                              (FAIL v0))
                                                          (FAIL v0))
                                                      (if (equal?
                                                           'JUST
                                                           (car (car (cdr (cdr v0)))))
                                                          (if (pair? (cdr (car (cdr (cdr v0)))))
                                                              (if (null? (cdr (cdr (car (cdr (cdr v0))))))
                                                                  (if (null? (cdr (cdr (cdr v0))))
                                                                      (LEAF33
                                                                       (car (cdr (car (cdr (cdr v0))))))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (FAIL v0)))
                                                  (FAIL v0))
                                              (FAIL v0))
                                          (if (pair? (cdr (cdr v0)))
                                              (if (pair? (car (cdr (cdr v0))))
                                                  (if (equal?
                                                       'JUST
                                                       (car (car (cdr (cdr v0)))))
                                                      (if (pair? (cdr (car (cdr (cdr v0)))))
                                                          (if (null? (cdr (cdr (car (cdr (cdr v0))))))
                                                              (if (null? (cdr (cdr (cdr v0))))
                                                                  (LEAF33
                                                                   (car (cdr (car (cdr (cdr v0))))))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (FAIL v0))
                                                      (FAIL v0))
                                                  (FAIL v0))
                                              (FAIL v0)))
                                      (FAIL v0))
                                  (if (equal? 'val-qq (car v0))
                                      (if (pair? (cdr v0))
                                          (if (pair? (car (cdr v0)))
                                              (if (equal?
                                                   'unquote
                                                   (car (car (cdr v0))))
                                                  (if (pair? (cdr (car (cdr v0))))
                                                      (if (null? (cdr (cdr (car (cdr v0)))))
                                                          (if (pair? (cdr (cdr v0)))
                                                              (if (pair? (cdr (cdr (cdr v0))))
                                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                      (LEAF34
                                                                       (car (cdr (cdr (cdr v0))))
                                                                       (car (cdr (cdr v0)))
                                                                       (car (cdr (car (cdr v0)))))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (if (pair? (cdr (cdr v0)))
                                                              (if (pair? (cdr (cdr (cdr v0))))
                                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                      (LEAF35
                                                                       (car (cdr (cdr (cdr v0))))
                                                                       (car (cdr (cdr v0)))
                                                                       (cdr (car (cdr v0)))
                                                                       (car (car (cdr v0))))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (FAIL v0)))
                                                      (if (pair? (cdr (cdr v0)))
                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                  (LEAF35
                                                                   (car (cdr (cdr (cdr v0))))
                                                                   (car (cdr (cdr v0)))
                                                                   (cdr (car (cdr v0)))
                                                                   (car (car (cdr v0))))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (FAIL v0)))
                                                  (if (pair? (cdr (cdr v0)))
                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                              (LEAF35
                                                               (car (cdr (cdr (cdr v0))))
                                                               (car (cdr (cdr v0)))
                                                               (cdr (car (cdr v0)))
                                                               (car (car (cdr v0))))
                                                              (FAIL v0))
                                                          (FAIL v0))
                                                      (FAIL v0)))
                                              (if (pair? (cdr (cdr v0)))
                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                          (LEAF36
                                                           (car (cdr v0)))
                                                          (FAIL v0))
                                                      (FAIL v0))
                                                  (FAIL v0)))
                                          (FAIL v0))
                                      (if (equal? 'pindolf (car v0))
                                          (if (pair? (cdr v0))
                                              (if (pair? (cdr (cdr v0)))
                                                  (if (null? (cdr (cdr (cdr v0))))
                                                      (LEAF37
                                                       (car (cdr (cdr v0)))
                                                       (car (cdr v0)))
                                                      (if (null? (car (cdr (cdr v0))))
                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                  (LEAF38
                                                                   (car (cdr v0)))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (if (pair? (car (cdr (cdr v0))))
                                                              (if (pair? (car (car (cdr (cdr v0)))))
                                                                  (if (pair? (cdr (car (car (cdr (cdr v0))))))
                                                                      (if (null? (cdr (cdr (car (car (cdr (cdr v0)))))))
                                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                                  (LEAF39
                                                                                   (car (cdr (cdr (cdr v0))))
                                                                                   (cdr (car (cdr (cdr v0))))
                                                                                   (car (cdr (car (car (cdr (cdr v0))))))
                                                                                   (car (car (car (cdr (cdr v0)))))
                                                                                   (car (cdr v0)))
                                                                                  (FAIL v0))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (FAIL v0))))
                                                  (FAIL v0))
                                              (FAIL v0))
                                          (if (equal? 'pindolf* (car v0))
                                              (if (pair? (cdr v0))
                                                  (if (pair? (cdr (cdr v0)))
                                                      (if (equal?
                                                           'NO-MATCH
                                                           (car (cdr (cdr v0))))
                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                              (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                  (if (pair? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                      (if (null? (cdr (cdr (cdr (cdr (cdr (cdr v0)))))))
                                                                          (LEAF40
                                                                           (car (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                           (car (cdr (cdr (cdr (cdr v0)))))
                                                                           (car (cdr v0)))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                              (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                  (if (pair? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                      (if (null? (cdr (cdr (cdr (cdr (cdr (cdr v0)))))))
                                                                          (LEAF41
                                                                           (car (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                           (car (cdr (cdr (cdr v0))))
                                                                           (car (cdr (cdr v0))))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (FAIL v0)))
                                                      (FAIL v0))
                                                  (FAIL v0))
                                              (if (pair? (car v0))
                                                  (if (equal?
                                                       'app
                                                       (car (car v0)))
                                                      (if (pair? (cdr (car v0)))
                                                          (if (null? (cdr (cdr (car v0))))
                                                              (if (pair? (cdr v0))
                                                                  (if (null? (cdr (cdr v0)))
                                                                      (LEAF42
                                                                       (car (cdr v0))
                                                                       (car (cdr (car v0))))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (FAIL v0))
                                                      (FAIL v0))
                                                  (FAIL v0))))))))))))
      (FAIL v0)))
(define (DISPATCH1 v-prg v-e) (LEAF37 v-prg v-e))
(define (DISPATCH2 v-bnd v-prg v-e*)
  (if (null? v-e*)
      (LEAF23)
      (if (number? v-e*)
          (LEAF24 v-e*)
          (if (pair? v-e*)
              (if (equal? 'quote (car v-e*))
                  (if (pair? (cdr v-e*))
                      (if (null? (cdr (cdr v-e*)))
                          (LEAF25 (car (cdr v-e*)))
                          (FAIL (cons 'value
                                      (cons v-e*
                                            (cons v-bnd
                                                  (cons (cons 'app
                                                              (cons v-prg '()))
                                                        '()))))))
                      (FAIL (cons 'value
                                  (cons v-e*
                                        (cons v-bnd
                                              (cons (cons 'app
                                                          (cons v-prg '()))
                                                    '()))))))
                  (if (equal? 'quasiquote (car v-e*))
                      (if (pair? (cdr v-e*))
                          (if (null? (cdr (cdr v-e*)))
                              (LEAF26
                               (cons 'app (cons v-prg '()))
                               v-bnd
                               (car (cdr v-e*)))
                              (FAIL (cons 'value
                                          (cons v-e*
                                                (cons v-bnd
                                                      (cons (cons 'app
                                                                  (cons v-prg
                                                                        '()))
                                                            '()))))))
                          (FAIL (cons 'value
                                      (cons v-e*
                                            (cons v-bnd
                                                  (cons (cons 'app
                                                              (cons v-prg '()))
                                                        '()))))))
                      (if (equal? '+ (car v-e*))
                          (if (pair? (cdr v-e*))
                              (if (pair? (cdr (cdr v-e*)))
                                  (if (null? (cdr (cdr (cdr v-e*))))
                                      (LEAF27
                                       (cons 'app (cons v-prg '()))
                                       v-bnd
                                       (car (cdr (cdr v-e*)))
                                       (car (cdr v-e*)))
                                      (FAIL (cons 'value
                                                  (cons v-e*
                                                        (cons v-bnd
                                                              (cons (cons 'app
                                                                          (cons v-prg
                                                                                '()))
                                                                    '()))))))
                                  (FAIL (cons 'value
                                              (cons v-e*
                                                    (cons v-bnd
                                                          (cons (cons 'app
                                                                      (cons v-prg
                                                                            '()))
                                                                '()))))))
                              (FAIL (cons 'value
                                          (cons v-e*
                                                (cons v-bnd
                                                      (cons (cons 'app
                                                                  (cons v-prg
                                                                        '()))
                                                            '()))))))
                          (if (equal? '- (car v-e*))
                              (if (pair? (cdr v-e*))
                                  (if (pair? (cdr (cdr v-e*)))
                                      (if (null? (cdr (cdr (cdr v-e*))))
                                          (LEAF28
                                           (cons 'app (cons v-prg '()))
                                           v-bnd
                                           (car (cdr (cdr v-e*)))
                                           (car (cdr v-e*)))
                                          (FAIL (cons 'value
                                                      (cons v-e*
                                                            (cons v-bnd
                                                                  (cons (cons 'app
                                                                              (cons v-prg
                                                                                    '()))
                                                                        '()))))))
                                      (FAIL (cons 'value
                                                  (cons v-e*
                                                        (cons v-bnd
                                                              (cons (cons 'app
                                                                          (cons v-prg
                                                                                '()))
                                                                    '()))))))
                                  (FAIL (cons 'value
                                              (cons v-e*
                                                    (cons v-bnd
                                                          (cons (cons 'app
                                                                      (cons v-prg
                                                                            '()))
                                                                '()))))))
                              (if (equal? '* (car v-e*))
                                  (if (pair? (cdr v-e*))
                                      (if (pair? (cdr (cdr v-e*)))
                                          (if (null? (cdr (cdr (cdr v-e*))))
                                              (LEAF29
                                               (cons 'app (cons v-prg '()))
                                               v-bnd
                                               (car (cdr (cdr v-e*)))
                                               (car (cdr v-e*)))
                                              (FAIL (cons 'value
                                                          (cons v-e*
                                                                (cons v-bnd
                                                                      (cons (cons 'app
                                                                                  (cons v-prg
                                                                                        '()))
                                                                            '()))))))
                                          (FAIL (cons 'value
                                                      (cons v-e*
                                                            (cons v-bnd
                                                                  (cons (cons 'app
                                                                              (cons v-prg
                                                                                    '()))
                                                                        '()))))))
                                      (FAIL (cons 'value
                                                  (cons v-e*
                                                        (cons v-bnd
                                                              (cons (cons 'app
                                                                          (cons v-prg
                                                                                '()))
                                                                    '()))))))
                                  (if (equal? 'rec (car v-e*))
                                      (if (pair? (cdr v-e*))
                                          (if (null? (cdr (cdr v-e*)))
                                              (LEAF30
                                               (cons 'app (cons v-prg '()))
                                               v-bnd
                                               (car (cdr v-e*)))
                                              (FAIL (cons 'value
                                                          (cons v-e*
                                                                (cons v-bnd
                                                                      (cons (cons 'app
                                                                                  (cons v-prg
                                                                                        '()))
                                                                            '()))))))
                                          (FAIL (cons 'value
                                                      (cons v-e*
                                                            (cons v-bnd
                                                                  (cons (cons 'app
                                                                              (cons v-prg
                                                                                    '()))
                                                                        '()))))))
                                      (FAIL (cons 'value
                                                  (cons v-e*
                                                        (cons v-bnd
                                                              (cons (cons 'app
                                                                          (cons v-prg
                                                                                '()))
                                                                    '())))))))))))
              (if (symbol? v-e*)
                  (LEAF31 v-bnd v-e*)
                  (FAIL (cons 'value
                              (cons v-e*
                                    (cons v-bnd
                                          (cons (cons 'app (cons v-prg '()))
                                                '()))))))))))
(define (DISPATCH3 v-prg* v-prg v-e)
  (if (null? (cons v-prg '()))
      (LEAF37 v-prg* v-e)
      (if (null? v-prg*)
          (LEAF38 v-e)
          (if (pair? v-prg*)
              (if (pair? (car v-prg*))
                  (if (pair? (cdr (car v-prg*)))
                      (if (null? (cdr (cdr (car v-prg*))))
                          (LEAF39
                           v-prg
                           (cdr v-prg*)
                           (car (cdr (car v-prg*)))
                           (car (car v-prg*))
                           v-e)
                          (FAIL (cons 'pindolf
                                      (cons v-e (cons v-prg* (cons v-prg '()))))))
                      (FAIL (cons 'pindolf
                                  (cons v-e (cons v-prg* (cons v-prg '()))))))
                  (FAIL (cons 'pindolf
                              (cons v-e (cons v-prg* (cons v-prg '()))))))
              (FAIL (cons 'pindolf (cons v-e (cons v-prg* (cons v-prg '())))))))))
(define (DISPATCH4 a-0 v-prg* v-prg v-exp v-e)
  (if (equal? 'NO-MATCH a-0)
      (LEAF40 v-prg v-prg* v-e)
      (LEAF41 v-prg v-exp a-0)))
(define (DISPATCH5 v-e v-pat) (LEAF0 v-e v-pat))
(define (DISPATCH6 v-prg v-e)
  (if (null? (cons v-prg '()))
      (LEAF37 v-prg v-e)
      (if (null? v-prg)
          (LEAF38 v-e)
          (if (pair? v-prg)
              (if (pair? (car v-prg))
                  (if (pair? (cdr (car v-prg)))
                      (if (null? (cdr (cdr (car v-prg))))
                          (LEAF39
                           v-prg
                           (cdr v-prg)
                           (car (cdr (car v-prg)))
                           (car (car v-prg))
                           v-e)
                          (FAIL (cons 'pindolf
                                      (cons v-e (cons v-prg (cons v-prg '()))))))
                      (FAIL (cons 'pindolf
                                  (cons v-e (cons v-prg (cons v-prg '()))))))
                  (FAIL (cons 'pindolf
                              (cons v-e (cons v-prg (cons v-prg '()))))))
              (FAIL (cons 'pindolf (cons v-e (cons v-prg (cons v-prg '())))))))))
(define (DISPATCH7 v-bnd v-app v-t)
  (if (pair? v-t)
      (if (equal? 'unquote (car v-t))
          (if (pair? (cdr v-t))
              (if (null? (cdr (cdr v-t)))
                  (LEAF34 v-app v-bnd (car (cdr v-t)))
                  (LEAF35 v-app v-bnd (cdr v-t) (car v-t)))
              (LEAF35 v-app v-bnd (cdr v-t) (car v-t)))
          (LEAF35 v-app v-bnd (cdr v-t) (car v-t)))
      (LEAF36 v-t)))
(define (DISPATCH8 v-bnd v-app v-h)
  (if (pair? v-h)
      (if (equal? 'unquote (car v-h))
          (if (pair? (cdr v-h))
              (if (null? (cdr (cdr v-h)))
                  (LEAF34 v-app v-bnd (car (cdr v-h)))
                  (LEAF35 v-app v-bnd (cdr v-h) (car v-h)))
              (LEAF35 v-app v-bnd (cdr v-h) (car v-h)))
          (LEAF35 v-app v-bnd (cdr v-h) (car v-h)))
      (LEAF36 v-h)))
(define (DISPATCH9 a-0 v-k)
  (if (symbol? v-k)
      (if (pair? a-0)
          (if (equal? 'NONE (car a-0))
              (if (null? (cdr a-0))
                  (LEAF32 v-k)
                  (FAIL (cons 'val-lu (cons v-k (cons a-0 '())))))
              (if (equal? 'JUST (car a-0))
                  (if (pair? (cdr a-0))
                      (if (null? (cdr (cdr a-0)))
                          (LEAF33 (car (cdr a-0)))
                          (FAIL (cons 'val-lu (cons v-k (cons a-0 '())))))
                      (FAIL (cons 'val-lu (cons v-k (cons a-0 '())))))
                  (FAIL (cons 'val-lu (cons v-k (cons a-0 '()))))))
          (FAIL (cons 'val-lu (cons v-k (cons a-0 '())))))
      (if (pair? a-0)
          (if (equal? 'JUST (car a-0))
              (if (pair? (cdr a-0))
                  (if (null? (cdr (cdr a-0)))
                      (LEAF33 (car (cdr a-0)))
                      (FAIL (cons 'val-lu (cons v-k (cons a-0 '())))))
                  (FAIL (cons 'val-lu (cons v-k (cons a-0 '())))))
              (FAIL (cons 'val-lu (cons v-k (cons a-0 '())))))
          (FAIL (cons 'val-lu (cons v-k (cons a-0 '())))))))
(define (DISPATCH10 a-0 v-app)
  (if (equal? 'match? v-app)
      (FAIL (cons v-app (cons a-0 '())))
      (if (equal? 'match?* v-app)
          (if (equal? 'NO-MATCH a-0)
              (FAIL (cons v-app (cons a-0 '())))
              (FAIL (cons v-app (cons a-0 '()))))
          (if (equal? 'match-var v-app)
              (if (symbol? a-0)
                  (FAIL (cons v-app (cons a-0 '())))
                  (FAIL (cons v-app (cons a-0 '()))))
              (if (equal? 'match-var* v-app)
                  (if (pair? a-0)
                      (if (equal? 'NONE (car a-0))
                          (if (null? (cdr a-0))
                              (FAIL (cons v-app (cons a-0 '())))
                              (FAIL (cons v-app (cons a-0 '()))))
                          (if (equal? 'JUST (car a-0))
                              (if (pair? (cdr a-0))
                                  (if (null? (cdr (cdr a-0)))
                                      (FAIL (cons v-app (cons a-0 '())))
                                      (FAIL (cons v-app (cons a-0 '()))))
                                  (FAIL (cons v-app (cons a-0 '()))))
                              (FAIL (cons v-app (cons a-0 '())))))
                      (FAIL (cons v-app (cons a-0 '()))))
                  (if (equal? 'lookup v-app)
                      (if (symbol? a-0)
                          (FAIL (cons v-app (cons a-0 '())))
                          (FAIL (cons v-app (cons a-0 '()))))
                      (if (equal? 'value v-app)
                          (if (null? a-0)
                              (FAIL (cons v-app (cons a-0 '())))
                              (if (number? a-0)
                                  (FAIL (cons v-app (cons a-0 '())))
                                  (if (pair? a-0)
                                      (if (equal? 'quote (car a-0))
                                          (if (pair? (cdr a-0))
                                              (if (null? (cdr (cdr a-0)))
                                                  (FAIL (cons v-app
                                                              (cons a-0 '())))
                                                  (FAIL (cons v-app
                                                              (cons a-0 '()))))
                                              (FAIL (cons v-app (cons a-0 '()))))
                                          (if (equal? 'quasiquote (car a-0))
                                              (if (pair? (cdr a-0))
                                                  (if (null? (cdr (cdr a-0)))
                                                      (FAIL (cons v-app
                                                                  (cons a-0
                                                                        '())))
                                                      (FAIL (cons v-app
                                                                  (cons a-0
                                                                        '()))))
                                                  (FAIL (cons v-app
                                                              (cons a-0 '()))))
                                              (if (equal? '+ (car a-0))
                                                  (if (pair? (cdr a-0))
                                                      (if (pair? (cdr (cdr a-0)))
                                                          (if (null? (cdr (cdr (cdr a-0))))
                                                              (FAIL (cons v-app
                                                                          (cons a-0
                                                                                '())))
                                                              (FAIL (cons v-app
                                                                          (cons a-0
                                                                                '()))))
                                                          (FAIL (cons v-app
                                                                      (cons a-0
                                                                            '()))))
                                                      (FAIL (cons v-app
                                                                  (cons a-0
                                                                        '()))))
                                                  (if (equal? '- (car a-0))
                                                      (if (pair? (cdr a-0))
                                                          (if (pair? (cdr (cdr a-0)))
                                                              (if (null? (cdr (cdr (cdr a-0))))
                                                                  (FAIL (cons v-app
                                                                              (cons a-0
                                                                                    '())))
                                                                  (FAIL (cons v-app
                                                                              (cons a-0
                                                                                    '()))))
                                                              (FAIL (cons v-app
                                                                          (cons a-0
                                                                                '()))))
                                                          (FAIL (cons v-app
                                                                      (cons a-0
                                                                            '()))))
                                                      (if (equal? '* (car a-0))
                                                          (if (pair? (cdr a-0))
                                                              (if (pair? (cdr (cdr a-0)))
                                                                  (if (null? (cdr (cdr (cdr a-0))))
                                                                      (FAIL (cons v-app
                                                                                  (cons a-0
                                                                                        '())))
                                                                      (FAIL (cons v-app
                                                                                  (cons a-0
                                                                                        '()))))
                                                                  (FAIL (cons v-app
                                                                              (cons a-0
                                                                                    '()))))
                                                              (FAIL (cons v-app
                                                                          (cons a-0
                                                                                '()))))
                                                          (if (equal?
                                                               'rec
                                                               (car a-0))
                                                              (if (pair? (cdr a-0))
                                                                  (if (null? (cdr (cdr a-0)))
                                                                      (FAIL (cons v-app
                                                                                  (cons a-0
                                                                                        '())))
                                                                      (FAIL (cons v-app
                                                                                  (cons a-0
                                                                                        '()))))
                                                                  (FAIL (cons v-app
                                                                              (cons a-0
                                                                                    '()))))
                                                              (FAIL (cons v-app
                                                                          (cons a-0
                                                                                '())))))))))
                                      (if (symbol? a-0)
                                          (FAIL (cons v-app (cons a-0 '())))
                                          (FAIL (cons v-app (cons a-0 '())))))))
                          (if (equal? 'val-lu v-app)
                              (if (symbol? a-0)
                                  (FAIL (cons v-app (cons a-0 '())))
                                  (FAIL (cons v-app (cons a-0 '()))))
                              (if (equal? 'val-qq v-app)
                                  (if (pair? a-0)
                                      (if (equal? 'unquote (car a-0))
                                          (if (pair? (cdr a-0))
                                              (if (null? (cdr (cdr a-0)))
                                                  (FAIL (cons v-app
                                                              (cons a-0 '())))
                                                  (FAIL (cons v-app
                                                              (cons a-0 '()))))
                                              (FAIL (cons v-app (cons a-0 '()))))
                                          (FAIL (cons v-app (cons a-0 '()))))
                                      (FAIL (cons v-app (cons a-0 '()))))
                                  (if (equal? 'pindolf v-app)
                                      (FAIL (cons v-app (cons a-0 '())))
                                      (if (equal? 'pindolf* v-app)
                                          (FAIL (cons v-app (cons a-0 '())))
                                          (if (pair? v-app)
                                              (if (equal? 'app (car v-app))
                                                  (if (pair? (cdr v-app))
                                                      (if (null? (cdr (cdr v-app)))
                                                          (LEAF42
                                                           a-0
                                                           (car (cdr v-app)))
                                                          (FAIL (cons v-app
                                                                      (cons a-0
                                                                            '()))))
                                                      (FAIL (cons v-app
                                                                  (cons a-0
                                                                        '()))))
                                                  (FAIL (cons v-app
                                                              (cons a-0 '()))))
                                              (FAIL (cons v-app (cons a-0 '())))))))))))))))
(define (DISPATCH11 v-bnd v-app v-a)
  (if (pair? v-a)
      (if (equal? 'unquote (car v-a))
          (if (pair? (cdr v-a))
              (if (null? (cdr (cdr v-a)))
                  (LEAF34 v-app v-bnd (car (cdr v-a)))
                  (LEAF35 v-app v-bnd (cdr v-a) (car v-a)))
              (LEAF35 v-app v-bnd (cdr v-a) (car v-a)))
          (LEAF35 v-app v-bnd (cdr v-a) (car v-a)))
      (LEAF36 v-a)))
(define (DISPATCH12 v-bnd v-app v-e*)
  (if (null? v-e*)
      (LEAF23)
      (if (number? v-e*)
          (LEAF24 v-e*)
          (if (pair? v-e*)
              (if (equal? 'quote (car v-e*))
                  (if (pair? (cdr v-e*))
                      (if (null? (cdr (cdr v-e*)))
                          (LEAF25 (car (cdr v-e*)))
                          (FAIL (cons 'value
                                      (cons v-e* (cons v-bnd (cons v-app '()))))))
                      (FAIL (cons 'value
                                  (cons v-e* (cons v-bnd (cons v-app '()))))))
                  (if (equal? 'quasiquote (car v-e*))
                      (if (pair? (cdr v-e*))
                          (if (null? (cdr (cdr v-e*)))
                              (LEAF26 v-app v-bnd (car (cdr v-e*)))
                              (FAIL (cons 'value
                                          (cons v-e*
                                                (cons v-bnd (cons v-app '()))))))
                          (FAIL (cons 'value
                                      (cons v-e* (cons v-bnd (cons v-app '()))))))
                      (if (equal? '+ (car v-e*))
                          (if (pair? (cdr v-e*))
                              (if (pair? (cdr (cdr v-e*)))
                                  (if (null? (cdr (cdr (cdr v-e*))))
                                      (LEAF27
                                       v-app
                                       v-bnd
                                       (car (cdr (cdr v-e*)))
                                       (car (cdr v-e*)))
                                      (FAIL (cons 'value
                                                  (cons v-e*
                                                        (cons v-bnd
                                                              (cons v-app '()))))))
                                  (FAIL (cons 'value
                                              (cons v-e*
                                                    (cons v-bnd
                                                          (cons v-app '()))))))
                              (FAIL (cons 'value
                                          (cons v-e*
                                                (cons v-bnd (cons v-app '()))))))
                          (if (equal? '- (car v-e*))
                              (if (pair? (cdr v-e*))
                                  (if (pair? (cdr (cdr v-e*)))
                                      (if (null? (cdr (cdr (cdr v-e*))))
                                          (LEAF28
                                           v-app
                                           v-bnd
                                           (car (cdr (cdr v-e*)))
                                           (car (cdr v-e*)))
                                          (FAIL (cons 'value
                                                      (cons v-e*
                                                            (cons v-bnd
                                                                  (cons v-app
                                                                        '()))))))
                                      (FAIL (cons 'value
                                                  (cons v-e*
                                                        (cons v-bnd
                                                              (cons v-app '()))))))
                                  (FAIL (cons 'value
                                              (cons v-e*
                                                    (cons v-bnd
                                                          (cons v-app '()))))))
                              (if (equal? '* (car v-e*))
                                  (if (pair? (cdr v-e*))
                                      (if (pair? (cdr (cdr v-e*)))
                                          (if (null? (cdr (cdr (cdr v-e*))))
                                              (LEAF29
                                               v-app
                                               v-bnd
                                               (car (cdr (cdr v-e*)))
                                               (car (cdr v-e*)))
                                              (FAIL (cons 'value
                                                          (cons v-e*
                                                                (cons v-bnd
                                                                      (cons v-app
                                                                            '()))))))
                                          (FAIL (cons 'value
                                                      (cons v-e*
                                                            (cons v-bnd
                                                                  (cons v-app
                                                                        '()))))))
                                      (FAIL (cons 'value
                                                  (cons v-e*
                                                        (cons v-bnd
                                                              (cons v-app '()))))))
                                  (if (equal? 'rec (car v-e*))
                                      (if (pair? (cdr v-e*))
                                          (if (null? (cdr (cdr v-e*)))
                                              (LEAF30
                                               v-app
                                               v-bnd
                                               (car (cdr v-e*)))
                                              (FAIL (cons 'value
                                                          (cons v-e*
                                                                (cons v-bnd
                                                                      (cons v-app
                                                                            '()))))))
                                          (FAIL (cons 'value
                                                      (cons v-e*
                                                            (cons v-bnd
                                                                  (cons v-app
                                                                        '()))))))
                                      (FAIL (cons 'value
                                                  (cons v-e*
                                                        (cons v-bnd
                                                              (cons v-app '())))))))))))
              (if (symbol? v-e*)
                  (LEAF31 v-bnd v-e*)
                  (FAIL (cons 'value (cons v-e* (cons v-bnd (cons v-app '()))))))))))
(define (DISPATCH13 v-bnd v-app v-e)
  (if (null? v-e)
      (LEAF23)
      (if (number? v-e)
          (LEAF24 v-e)
          (if (pair? v-e)
              (if (equal? 'quote (car v-e))
                  (if (pair? (cdr v-e))
                      (if (null? (cdr (cdr v-e)))
                          (LEAF25 (car (cdr v-e)))
                          (FAIL (cons 'value
                                      (cons v-e (cons v-bnd (cons v-app '()))))))
                      (FAIL (cons 'value
                                  (cons v-e (cons v-bnd (cons v-app '()))))))
                  (if (equal? 'quasiquote (car v-e))
                      (if (pair? (cdr v-e))
                          (if (null? (cdr (cdr v-e)))
                              (LEAF26 v-app v-bnd (car (cdr v-e)))
                              (FAIL (cons 'value
                                          (cons v-e
                                                (cons v-bnd (cons v-app '()))))))
                          (FAIL (cons 'value
                                      (cons v-e (cons v-bnd (cons v-app '()))))))
                      (if (equal? '+ (car v-e))
                          (if (pair? (cdr v-e))
                              (if (pair? (cdr (cdr v-e)))
                                  (if (null? (cdr (cdr (cdr v-e))))
                                      (LEAF27
                                       v-app
                                       v-bnd
                                       (car (cdr (cdr v-e)))
                                       (car (cdr v-e)))
                                      (FAIL (cons 'value
                                                  (cons v-e
                                                        (cons v-bnd
                                                              (cons v-app '()))))))
                                  (FAIL (cons 'value
                                              (cons v-e
                                                    (cons v-bnd
                                                          (cons v-app '()))))))
                              (FAIL (cons 'value
                                          (cons v-e
                                                (cons v-bnd (cons v-app '()))))))
                          (if (equal? '- (car v-e))
                              (if (pair? (cdr v-e))
                                  (if (pair? (cdr (cdr v-e)))
                                      (if (null? (cdr (cdr (cdr v-e))))
                                          (LEAF28
                                           v-app
                                           v-bnd
                                           (car (cdr (cdr v-e)))
                                           (car (cdr v-e)))
                                          (FAIL (cons 'value
                                                      (cons v-e
                                                            (cons v-bnd
                                                                  (cons v-app
                                                                        '()))))))
                                      (FAIL (cons 'value
                                                  (cons v-e
                                                        (cons v-bnd
                                                              (cons v-app '()))))))
                                  (FAIL (cons 'value
                                              (cons v-e
                                                    (cons v-bnd
                                                          (cons v-app '()))))))
                              (if (equal? '* (car v-e))
                                  (if (pair? (cdr v-e))
                                      (if (pair? (cdr (cdr v-e)))
                                          (if (null? (cdr (cdr (cdr v-e))))
                                              (LEAF29
                                               v-app
                                               v-bnd
                                               (car (cdr (cdr v-e)))
                                               (car (cdr v-e)))
                                              (FAIL (cons 'value
                                                          (cons v-e
                                                                (cons v-bnd
                                                                      (cons v-app
                                                                            '()))))))
                                          (FAIL (cons 'value
                                                      (cons v-e
                                                            (cons v-bnd
                                                                  (cons v-app
                                                                        '()))))))
                                      (FAIL (cons 'value
                                                  (cons v-e
                                                        (cons v-bnd
                                                              (cons v-app '()))))))
                                  (if (equal? 'rec (car v-e))
                                      (if (pair? (cdr v-e))
                                          (if (null? (cdr (cdr v-e)))
                                              (LEAF30
                                               v-app
                                               v-bnd
                                               (car (cdr v-e)))
                                              (FAIL (cons 'value
                                                          (cons v-e
                                                                (cons v-bnd
                                                                      (cons v-app
                                                                            '()))))))
                                          (FAIL (cons 'value
                                                      (cons v-e
                                                            (cons v-bnd
                                                                  (cons v-app
                                                                        '()))))))
                                      (FAIL (cons 'value
                                                  (cons v-e
                                                        (cons v-bnd
                                                              (cons v-app '())))))))))))
              (if (symbol? v-e)
                  (LEAF31 v-bnd v-e)
                  (FAIL (cons 'value (cons v-e (cons v-bnd (cons v-app '()))))))))))
(define (DISPATCH14 v-bnd v-app v-qq)
  (if (pair? v-qq)
      (if (equal? 'unquote (car v-qq))
          (if (pair? (cdr v-qq))
              (if (null? (cdr (cdr v-qq)))
                  (LEAF34 v-app v-bnd (car (cdr v-qq)))
                  (LEAF35 v-app v-bnd (cdr v-qq) (car v-qq)))
              (LEAF35 v-app v-bnd (cdr v-qq) (car v-qq)))
          (LEAF35 v-app v-bnd (cdr v-qq) (car v-qq)))
      (LEAF36 v-qq)))
(define (DISPATCH15 v-bnd v-k)
  (if (symbol? v-k)
      (if (pair? v-bnd)
          (if (pair? (car v-bnd))
              (if (equal? v-k (car (car v-bnd)))
                  (LEAF20 (cdr (car v-bnd)) v-k)
                  (LEAF21 (cdr v-bnd) v-k))
              (FAIL (cons 'lookup (cons v-k (cons v-bnd '())))))
          (if (null? v-bnd)
              (LEAF22)
              (FAIL (cons 'lookup (cons v-k (cons v-bnd '()))))))
      (if (null? v-bnd)
          (LEAF22)
          (FAIL (cons 'lookup (cons v-k (cons v-bnd '())))))))
(define (DISPATCH16 v-v v-bnd v-e a-0)
  (if (pair? a-0)
      (if (equal? 'NONE (car a-0))
          (if (null? (cdr a-0))
              (if (symbol? v-v)
                  (LEAF17 v-bnd v-e v-v)
                  (FAIL (cons 'match-var*
                              (cons a-0 (cons v-v (cons v-e (cons v-bnd '())))))))
              (FAIL (cons 'match-var*
                          (cons a-0 (cons v-v (cons v-e (cons v-bnd '())))))))
          (if (equal? 'JUST (car a-0))
              (if (pair? (cdr a-0))
                  (if (null? (cdr (cdr a-0)))
                      (if (symbol? v-v)
                          (if (equal? (car (cdr a-0)) v-e)
                              (LEAF18 v-bnd v-v (car (cdr a-0)))
                              (LEAF19))
                          (LEAF19))
                      (FAIL (cons 'match-var*
                                  (cons a-0
                                        (cons v-v (cons v-e (cons v-bnd '())))))))
                  (FAIL (cons 'match-var*
                              (cons a-0 (cons v-v (cons v-e (cons v-bnd '())))))))
              (FAIL (cons 'match-var*
                          (cons a-0 (cons v-v (cons v-e (cons v-bnd '()))))))))
      (FAIL (cons 'match-var*
                  (cons a-0 (cons v-v (cons v-e (cons v-bnd '()))))))))
(define (DISPATCH17 v-bnd v-v)
  (if (symbol? v-v)
      (if (pair? v-bnd)
          (if (pair? (car v-bnd))
              (if (equal? v-v (car (car v-bnd)))
                  (LEAF20 (cdr (car v-bnd)) v-v)
                  (LEAF21 (cdr v-bnd) v-v))
              (FAIL (cons 'lookup (cons v-v (cons v-bnd '())))))
          (if (null? v-bnd)
              (LEAF22)
              (FAIL (cons 'lookup (cons v-v (cons v-bnd '()))))))
      (if (null? v-bnd)
          (LEAF22)
          (FAIL (cons 'lookup (cons v-v (cons v-bnd '())))))))
(define (DISPATCH18 v-ps v-es a-0)
  (if (equal? 'NO-MATCH a-0) (LEAF14) (LEAF15 v-es v-ps a-0)))
(define (DISPATCH19 v-e v-bnd v-p)
  (if (null? (cons v-bnd '()))
      (LEAF0 v-e v-p)
      (if (null? v-p)
          (if (null? v-e) (LEAF1 v-bnd) (LEAF13))
          (if (equal? '_ v-p)
              (LEAF2 v-bnd)
              (if (number? v-p)
                  (if (equal? v-p v-e) (LEAF3 v-bnd v-p) (LEAF13))
                  (if (pair? v-p)
                      (if (equal? 'quote (car v-p))
                          (if (pair? (cdr v-p))
                              (if (null? (cdr (cdr v-p)))
                                  (if (equal? (car (cdr v-p)) v-e)
                                      (LEAF4 v-bnd (car (cdr v-p)))
                                      (if (pair? v-e)
                                          (LEAF12
                                           v-bnd
                                           (cdr v-e)
                                           (car v-e)
                                           (cdr v-p)
                                           (car v-p))
                                          (LEAF13)))
                                  (if (pair? v-e)
                                      (LEAF12
                                       v-bnd
                                       (cdr v-e)
                                       (car v-e)
                                       (cdr v-p)
                                       (car v-p))
                                      (LEAF13)))
                              (if (pair? v-e)
                                  (LEAF12
                                   v-bnd
                                   (cdr v-e)
                                   (car v-e)
                                   (cdr v-p)
                                   (car v-p))
                                  (LEAF13)))
                          (if (equal? 'num (car v-p))
                              (if (pair? (cdr v-p))
                                  (if (symbol? (car (cdr v-p)))
                                      (if (null? (cdr (cdr v-p)))
                                          (if (number? v-e)
                                              (LEAF5 v-bnd v-e (car (cdr v-p)))
                                              (LEAF6))
                                          (if (pair? v-e)
                                              (LEAF12
                                               v-bnd
                                               (cdr v-e)
                                               (car v-e)
                                               (cdr v-p)
                                               (car v-p))
                                              (LEAF13)))
                                      (if (null? (cdr (cdr v-p)))
                                          (LEAF6)
                                          (if (pair? v-e)
                                              (LEAF12
                                               v-bnd
                                               (cdr v-e)
                                               (car v-e)
                                               (cdr v-p)
                                               (car v-p))
                                              (LEAF13))))
                                  (if (pair? v-e)
                                      (LEAF12
                                       v-bnd
                                       (cdr v-e)
                                       (car v-e)
                                       (cdr v-p)
                                       (car v-p))
                                      (LEAF13)))
                              (if (equal? 'sym (car v-p))
                                  (if (pair? (cdr v-p))
                                      (if (symbol? (car (cdr v-p)))
                                          (if (null? (cdr (cdr v-p)))
                                              (if (symbol? v-e)
                                                  (LEAF7 v-bnd
                                                         v-e
                                                         (car (cdr v-p)))
                                                  (LEAF8))
                                              (if (pair? v-e)
                                                  (LEAF12
                                                   v-bnd
                                                   (cdr v-e)
                                                   (car v-e)
                                                   (cdr v-p)
                                                   (car v-p))
                                                  (LEAF13)))
                                          (if (null? (cdr (cdr v-p)))
                                              (LEAF8)
                                              (if (pair? v-e)
                                                  (LEAF12
                                                   v-bnd
                                                   (cdr v-e)
                                                   (car v-e)
                                                   (cdr v-p)
                                                   (car v-p))
                                                  (LEAF13))))
                                      (if (pair? v-e)
                                          (LEAF12
                                           v-bnd
                                           (cdr v-e)
                                           (car v-e)
                                           (cdr v-p)
                                           (car v-p))
                                          (LEAF13)))
                                  (if (equal? 'atm (car v-p))
                                      (if (pair? (cdr v-p))
                                          (if (symbol? (car (cdr v-p)))
                                              (if (null? (cdr (cdr v-p)))
                                                  (if (not (pair? v-e))
                                                      (LEAF9 v-bnd
                                                             v-e
                                                             (car (cdr v-p)))
                                                      (LEAF10))
                                                  (if (pair? v-e)
                                                      (LEAF12
                                                       v-bnd
                                                       (cdr v-e)
                                                       (car v-e)
                                                       (cdr v-p)
                                                       (car v-p))
                                                      (LEAF13)))
                                              (if (null? (cdr (cdr v-p)))
                                                  (LEAF10)
                                                  (if (pair? v-e)
                                                      (LEAF12
                                                       v-bnd
                                                       (cdr v-e)
                                                       (car v-e)
                                                       (cdr v-p)
                                                       (car v-p))
                                                      (LEAF13))))
                                          (if (pair? v-e)
                                              (LEAF12
                                               v-bnd
                                               (cdr v-e)
                                               (car v-e)
                                               (cdr v-p)
                                               (car v-p))
                                              (LEAF13)))
                                      (if (equal? 'exp (car v-p))
                                          (if (pair? (cdr v-p))
                                              (if (symbol? (car (cdr v-p)))
                                                  (if (null? (cdr (cdr v-p)))
                                                      (LEAF11
                                                       v-bnd
                                                       v-e
                                                       (car (cdr v-p)))
                                                      (if (pair? v-e)
                                                          (LEAF12
                                                           v-bnd
                                                           (cdr v-e)
                                                           (car v-e)
                                                           (cdr v-p)
                                                           (car v-p))
                                                          (LEAF13)))
                                                  (if (pair? v-e)
                                                      (LEAF12
                                                       v-bnd
                                                       (cdr v-e)
                                                       (car v-e)
                                                       (cdr v-p)
                                                       (car v-p))
                                                      (LEAF13)))
                                              (if (pair? v-e)
                                                  (LEAF12
                                                   v-bnd
                                                   (cdr v-e)
                                                   (car v-e)
                                                   (cdr v-p)
                                                   (car v-p))
                                                  (LEAF13)))
                                          (if (pair? v-e)
                                              (LEAF12
                                               v-bnd
                                               (cdr v-e)
                                               (car v-e)
                                               (cdr v-p)
                                               (car v-p))
                                              (LEAF13)))))))
                      (LEAF13)))))))
(define (DISPATCH20 v-e v-bnd v-ev)
  (if (symbol? v-ev)
      (LEAF16 v-bnd v-e v-ev)
      (FAIL (cons 'match-var (cons v-ev (cons v-e (cons v-bnd '())))))))
(define (DISPATCH21 v-e v-bnd v-av)
  (if (symbol? v-av)
      (LEAF16 v-bnd v-e v-av)
      (FAIL (cons 'match-var (cons v-av (cons v-e (cons v-bnd '())))))))
(define (DISPATCH22 v-e v-bnd v-sv)
  (if (symbol? v-sv)
      (LEAF16 v-bnd v-e v-sv)
      (FAIL (cons 'match-var (cons v-sv (cons v-e (cons v-bnd '())))))))
(define (DISPATCH23 v-e v-bnd v-nv)
  (if (symbol? v-nv)
      (LEAF16 v-bnd v-e v-nv)
      (FAIL (cons 'match-var (cons v-nv (cons v-e (cons v-bnd '())))))))
(define (DISPATCH24 v-expression v-pattern)
  (if (null? v-pattern)
      (if (null? v-expression) (LEAF1 '()) (LEAF13))
      (if (equal? '_ v-pattern)
          (LEAF2 '())
          (if (number? v-pattern)
              (if (equal? v-pattern v-expression)
                  (LEAF3 '() v-pattern)
                  (LEAF13))
              (if (pair? v-pattern)
                  (if (equal? 'quote (car v-pattern))
                      (if (pair? (cdr v-pattern))
                          (if (null? (cdr (cdr v-pattern)))
                              (if (equal? (car (cdr v-pattern)) v-expression)
                                  (LEAF4 '() (car (cdr v-pattern)))
                                  (if (pair? v-expression)
                                      (LEAF12
                                       '()
                                       (cdr v-expression)
                                       (car v-expression)
                                       (cdr v-pattern)
                                       (car v-pattern))
                                      (LEAF13)))
                              (if (pair? v-expression)
                                  (LEAF12
                                   '()
                                   (cdr v-expression)
                                   (car v-expression)
                                   (cdr v-pattern)
                                   (car v-pattern))
                                  (LEAF13)))
                          (if (pair? v-expression)
                              (LEAF12
                               '()
                               (cdr v-expression)
                               (car v-expression)
                               (cdr v-pattern)
                               (car v-pattern))
                              (LEAF13)))
                      (if (equal? 'num (car v-pattern))
                          (if (pair? (cdr v-pattern))
                              (if (symbol? (car (cdr v-pattern)))
                                  (if (null? (cdr (cdr v-pattern)))
                                      (if (number? v-expression)
                                          (LEAF5 '()
                                                 v-expression
                                                 (car (cdr v-pattern)))
                                          (LEAF6))
                                      (if (pair? v-expression)
                                          (LEAF12
                                           '()
                                           (cdr v-expression)
                                           (car v-expression)
                                           (cdr v-pattern)
                                           (car v-pattern))
                                          (LEAF13)))
                                  (if (null? (cdr (cdr v-pattern)))
                                      (LEAF6)
                                      (if (pair? v-expression)
                                          (LEAF12
                                           '()
                                           (cdr v-expression)
                                           (car v-expression)
                                           (cdr v-pattern)
                                           (car v-pattern))
                                          (LEAF13))))
                              (if (pair? v-expression)
                                  (LEAF12
                                   '()
                                   (cdr v-expression)
                                   (car v-expression)
                                   (cdr v-pattern)
                                   (car v-pattern))
                                  (LEAF13)))
                          (if (equal? 'sym (car v-pattern))
                              (if (pair? (cdr v-pattern))
                                  (if (symbol? (car (cdr v-pattern)))
                                      (if (null? (cdr (cdr v-pattern)))
                                          (if (symbol? v-expression)
                                              (LEAF7 '()
                                                     v-expression
                                                     (car (cdr v-pattern)))
                                              (LEAF8))
                                          (if (pair? v-expression)
                                              (LEAF12
                                               '()
                                               (cdr v-expression)
                                               (car v-expression)
                                               (cdr v-pattern)
                                               (car v-pattern))
                                              (LEAF13)))
                                      (if (null? (cdr (cdr v-pattern)))
                                          (LEAF8)
                                          (if (pair? v-expression)
                                              (LEAF12
                                               '()
                                               (cdr v-expression)
                                               (car v-expression)
                                               (cdr v-pattern)
                                               (car v-pattern))
                                              (LEAF13))))
                                  (if (pair? v-expression)
                                      (LEAF12
                                       '()
                                       (cdr v-expression)
                                       (car v-expression)
                                       (cdr v-pattern)
                                       (car v-pattern))
                                      (LEAF13)))
                              (if (equal? 'atm (car v-pattern))
                                  (if (pair? (cdr v-pattern))
                                      (if (symbol? (car (cdr v-pattern)))
                                          (if (null? (cdr (cdr v-pattern)))
                                              (if (not (pair? v-expression))
                                                  (LEAF9 '()
                                                         v-expression
                                                         (car (cdr v-pattern)))
                                                  (LEAF10))
                                              (if (pair? v-expression)
                                                  (LEAF12
                                                   '()
                                                   (cdr v-expression)
                                                   (car v-expression)
                                                   (cdr v-pattern)
                                                   (car v-pattern))
                                                  (LEAF13)))
                                          (if (null? (cdr (cdr v-pattern)))
                                              (LEAF10)
                                              (if (pair? v-expression)
                                                  (LEAF12
                                                   '()
                                                   (cdr v-expression)
                                                   (car v-expression)
                                                   (cdr v-pattern)
                                                   (car v-pattern))
                                                  (LEAF13))))
                                      (if (pair? v-expression)
                                          (LEAF12
                                           '()
                                           (cdr v-expression)
                                           (car v-expression)
                                           (cdr v-pattern)
                                           (car v-pattern))
                                          (LEAF13)))
                                  (if (equal? 'exp (car v-pattern))
                                      (if (pair? (cdr v-pattern))
                                          (if (symbol? (car (cdr v-pattern)))
                                              (if (null? (cdr (cdr v-pattern)))
                                                  (LEAF11
                                                   '()
                                                   v-expression
                                                   (car (cdr v-pattern)))
                                                  (if (pair? v-expression)
                                                      (LEAF12
                                                       '()
                                                       (cdr v-expression)
                                                       (car v-expression)
                                                       (cdr v-pattern)
                                                       (car v-pattern))
                                                      (LEAF13)))
                                              (if (pair? v-expression)
                                                  (LEAF12
                                                   '()
                                                   (cdr v-expression)
                                                   (car v-expression)
                                                   (cdr v-pattern)
                                                   (car v-pattern))
                                                  (LEAF13)))
                                          (if (pair? v-expression)
                                              (LEAF12
                                               '()
                                               (cdr v-expression)
                                               (car v-expression)
                                               (cdr v-pattern)
                                               (car v-pattern))
                                              (LEAF13)))
                                      (if (pair? v-expression)
                                          (LEAF12
                                           '()
                                           (cdr v-expression)
                                           (car v-expression)
                                           (cdr v-pattern)
                                           (car v-pattern))
                                          (LEAF13)))))))
                  (LEAF13))))))
(define (FAIL a) (display `(NO MATCH FOUND FOR ,a)) (newline) (error "halt"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; now some godelian stuff

(define magic-input
  '(pindolf (pindolf (test (M A) (K U L A) (T U R A))
                     ((((quote cat) () (exp ys)) ys)
                      (((quote cat) ((exp x) exp xs) (exp ys)) (quasiquote ((unquote x) unquote (rec (cat (unquote xs) (unquote ys))))))
                      (((quote name) (sym k) (exp v) (exp dict)) (quasiquote (((unquote k) unquote v) unquote dict)))
                      (((quote forget) (sym k) ()) ())
                      (((quote forget) (sym k) (((sym k) . _) exp dict)) dict)
                      (((quote forget) (sym k) (_ exp dict)) (rec (forget (unquote k) (unquote dict))))
                      (((quote lookup) (sym k) (((sym k) exp v) . _)) v)
                      (((quote lookup) (sym k) (_ exp dict)) (rec (lookup (unquote k) (unquote dict))))
                      (((quote run-drc) (exp program) (exp inputs)) (rec (step* () (unquote inputs) (unquote program))))
                      (((quote step*) (exp D) ((exp e) exp R) (((quote NAME) (sym n)) exp C)) (rec (step* (unquote (rec (name (unquote n) (unquote e) (unquote D)))) (unquote R) (unquote C))))
                      (((quote step*) (exp D) (exp R) (((quote FORGET) (sym n)) exp C)) (rec (step* (unquote (rec (forget (unquote n) (unquote D)))) (unquote R) (unquote C))))
                      (((quote step*) (exp D) (exp R) (((quote LOOKUP) (sym n)) exp C)) (rec (step* (unquote D) ((unquote (rec (lookup (unquote n) (unquote D)))) unquote R) (unquote C))))
                      (((quote step*) (exp D) (exp R) (((quote CONST) (exp e)) exp C)) (rec (step* (unquote D) ((unquote e) unquote R) (unquote C))))
                      (((quote step*) (exp D) (exp R) (((quote PROC) (exp p)) exp C)) (rec (step* (unquote D) ((unquote p) unquote R) (unquote C))))
                      (((quote step*) (exp D) ((exp h) (exp t) exp R) (((quote CONS)) exp C)) (rec (step* (unquote D) (((unquote h) unquote t) unquote R) (unquote C))))
                      (((quote step*) (exp D) (((exp h) exp t) exp R) (((quote CAR)) exp C)) (rec (step* (unquote D) ((unquote h) unquote R) (unquote C))))
                      (((quote step*) (exp D) (((exp h) exp t) exp R) (((quote CDR)) exp C)) (rec (step* (unquote D) ((unquote t) unquote R) (unquote C))))
                      (((quote step*) (exp D) ((exp e) (exp e) exp R) (((quote EQ?)) exp C)) (rec (step* (unquote D) (T unquote R) (unquote C))))
                      (((quote step*) (exp D) (_ _ exp R) (((quote EQ?)) exp C)) (rec (step* (unquote D) (() unquote R) (unquote C))))
                      (((quote step*) (exp D) (((exp h) exp t) exp R) (((quote ATOM?)) exp C)) (rec (step* (unquote D) (() unquote R) (unquote C))))
                      (((quote step*) (exp D) (_ exp R) (((quote ATOM?)) exp C)) (rec (step* (unquote D) (T unquote R) (unquote C))))
                      (((quote step*) (exp D) ((num n) exp R) (((quote NUM?)) exp C)) (rec (step* (unquote D) (T unquote R) (unquote C))))
                      (((quote step*) (exp D) (_ exp R) (((quote NUM?)) exp C)) (rec (step* (unquote D) (() unquote R) (unquote C))))
                      (((quote step*) (exp D) ((sym s) exp R) (((quote SYM?)) exp C)) (rec (step* (unquote D) (T unquote R) (unquote C))))
                      (((quote step*) (exp D) (_ exp R) (((quote SYM?)) exp C)) (rec (step* (unquote D) (() unquote R) (unquote C))))
                      (((quote step*) (exp D) ((exp n) (exp m) exp R) (((quote PLUS)) exp C)) (rec (step* (unquote D) ((unquote (+ n m)) unquote R) (unquote C))))
                      (((quote step*) (exp D) ((exp n) (exp m) exp R) (((quote MINUS)) exp C)) (rec (step* (unquote D) ((unquote (- n m)) unquote R) (unquote C))))
                      (((quote step*) (exp D) ((exp n) (exp m) exp R) (((quote TIMES)) exp C)) (rec (step* (unquote D) ((unquote (* n m)) unquote R) (unquote C))))
                      (((quote step*) (exp D) (() exp R) (((quote SELECT) (exp p) (exp p*)) exp C)) (rec (step* (unquote D) (unquote R) (unquote (rec (cat (unquote p*) (unquote C)))))))
                      (((quote step*) (exp D) (_ exp R) (((quote SELECT) (exp p) (exp p*)) exp C)) (rec (step* (unquote D) (unquote R) (unquote (rec (cat (unquote p) (unquote C)))))))
                      (((quote step*) (exp D) ((exp p) exp R) (((quote APPLY)) exp C)) (rec (step* (unquote D) (unquote R) (unquote (rec (cat (unquote p) (unquote C)))))))
                      (((quote step*) (exp D) ((exp e) exp R) ()) e)
                      (((quote mk-example)) (quote ((PROC ((NAME xs)
                                                           (NAME ys)
                                                           (LOOKUP xs)
                                                           (CONST ()) (EQ?)
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
                                                    (APPLY)
                                                    (LOOKUP apd)
                                                    (APPLY))))
                      (((quote test) (exp xs) (exp ys) (exp zs)) (rec (run-drc (unquote (rec (mk-example))) ((unquote xs) (unquote ys) (unquote zs)))))))
            ((((quote match?) (exp pattern) (exp expression)) (rec (match? (unquote pattern) (unquote expression) ())))
             (((quote match?) () () (exp bnd)) bnd)
             (((quote match?) (quote _) _ (exp bnd)) bnd)
             (((quote match?) (num n) (num n) (exp bnd)) bnd)
             (((quote match?) ((quote quote) (exp e)) (exp e) (exp bnd)) bnd)
             (((quote match?) ((quote num) (sym nv)) (num e) (exp bnd)) (rec (match-var (unquote nv) (unquote e) (unquote bnd))))
             (((quote match?) ((quote num) _) _ _) (quote NO-MATCH))
             (((quote match?) ((quote sym) (sym sv)) (sym e) (exp bnd)) (rec (match-var (unquote sv) (unquote e) (unquote bnd))))
             (((quote match?) ((quote sym) _) _ _) (quote NO-MATCH))
             (((quote match?) ((quote atm) (sym av)) (atm e) (exp bnd)) (rec (match-var (unquote av) (unquote e) (unquote bnd))))
             (((quote match?) ((quote atm) _) _ _) (quote NO-MATCH))
             (((quote match?) ((quote exp) (sym ev)) (exp e) (exp bnd)) (rec (match-var (unquote ev) (unquote e) (unquote bnd))))
             (((quote match?) ((exp p) exp ps) ((exp e) exp es) (exp bnd)) (rec (match?* (unquote (rec (match? (unquote p) (unquote e) (unquote bnd)))) (unquote ps) (unquote es))))
             (((quote match?) _ _ _) (quote NO-MATCH))
             (((quote match?*) (quote NO-MATCH) _ _) (quote NO-MATCH))
             (((quote match?*) (exp bnd) (exp p) (exp e)) (rec (match? (unquote p) (unquote e) (unquote bnd))))
             (((quote match-var) (sym v) (exp e) (exp bnd)) (rec (match-var* (unquote (rec (lookup (unquote v) (unquote bnd)))) (unquote v) (unquote e) (unquote bnd))))
             (((quote match-var*) ((quote NONE)) (sym v) (exp e) (exp bnd)) (quasiquote (((unquote v) unquote e) unquote bnd)))
             (((quote match-var*) ((quote JUST) (exp e)) (sym v) (exp e) (exp bnd)) bnd)
             (((quote match-var*) ((quote JUST) _) _ _ _) (quote NO-MATCH))
             (((quote lookup) (sym k) (((sym k) exp v) . _)) (quasiquote (JUST (unquote v))))
             (((quote lookup) (sym k) ((_ . _) exp bnd)) (rec (lookup (unquote k) (unquote bnd))))
             (((quote lookup) _ ()) (quote (NONE)))
             (((quote value) () _ _) ())
             (((quote value) (num n) _ _) n)
             (((quote value) ((quote quote) (exp e)) _ _) e)
             (((quote value) ((quote quasiquote) (exp qq)) (exp bnd) (exp app)) (rec (val-qq (unquote qq) (unquote bnd) (unquote app))))
             (((quote value) ((quote +) (exp e) (exp e*)) (exp bnd) (exp app)) (+ (rec (value (unquote e) (unquote bnd) (unquote app))) (rec (value (unquote e*) (unquote bnd) (unquote app)))))
             (((quote value) ((quote -) (exp e) (exp e*)) (exp bnd) (exp app)) (- (rec (value (unquote e) (unquote bnd) (unquote app))) (rec (value (unquote e*) (unquote bnd) (unquote app)))))
             (((quote value) ((quote *) (exp e) (exp e*)) (exp bnd) (exp app)) (* (rec (value (unquote e) (unquote bnd) (unquote app))) (rec (value (unquote e*) (unquote bnd) (unquote app)))))
             (((quote value) ((quote rec) (exp a)) (exp bnd) (exp app)) (rec ((unquote app) (unquote (rec (val-qq (unquote a) (unquote bnd) (unquote app)))))))
             (((quote value) (sym k) (exp bnd) _) (rec (val-lu (unquote k) (unquote (rec (lookup (unquote k) (unquote bnd)))))))
             (((quote val-lu) (sym k) ((quote NONE))) (quasiquote (unbound (unquote k))))
             (((quote val-lu) _ ((quote JUST) (exp v))) v)
             (((quote val-qq) ((quote unquote) (exp e)) (exp bnd) (exp app)) (rec (value (unquote e) (unquote bnd) (unquote app))))
             (((quote val-qq) ((exp h) exp t) (exp bnd) (exp app)) (quasiquote ((unquote (rec (val-qq (unquote h) (unquote bnd) (unquote app)))) .
                                                                                (unquote (rec (val-qq (unquote t) (unquote bnd) (unquote app)))))))
             (((quote val-qq) (exp e) _ _) e)
             (((quote pindolf) (exp e) (exp prg)) (rec (pindolf (unquote e) (unquote prg) (unquote prg))))
             (((quote pindolf) (exp e) () _) (quasiquote (NO-MATCH (unquote e))))
             (((quote pindolf) (exp e) (((exp pat) (exp exp)) exp prg*) (exp prg))
              (rec (pindolf* (unquote e) (unquote (rec (match? (unquote pat) (unquote e)))) (unquote exp) (unquote prg*) (unquote prg))))
             (((quote pindolf*) (exp e) (quote NO-MATCH) _ (exp prg*) (exp prg)) (rec (pindolf (unquote e) (unquote prg*) (unquote prg))))
             (((quote pindolf*) _ (exp bnd) (exp e*) _ (exp prg)) (rec (value (unquote e*) (unquote bnd) (app (unquote prg)))))
             ((((quote app) (exp prg)) (exp e)) (rec (pindolf (unquote e) (unquote prg))))))
  )

(begin (write (DISPATCH0 magic-input)) (newline))
