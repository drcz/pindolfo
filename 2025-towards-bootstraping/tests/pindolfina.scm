;; pindolfo2scm v1.2
(define (LEAF0) '())
(define (LEAF1 v-xs v-x v-n)
  (let* ((a-0 (DISPATCH22 v-xs v-n))) (cons v-x a-0)))
(define (LEAF2 v-xs) v-xs)
(define (LEAF3 v-xs v-x v-n) (let* ((a-0 (DISPATCH21 v-xs v-n))) a-0))
(define (LEAF4) 'NOT-FOUND)
(define (LEAF5 v-val v-key) v-val)
(define (LEAF6 v-alist v-key) (let* ((a-0 (DISPATCH20 v-alist v-key))) a-0))
(define (LEAF7 v-e) v-e)
(define (LEAF8 v-e v-addr) (let* ((a-0 (DISPATCH19 v-e v-addr))) a-0))
(define (LEAF9 v-e v-addr) (let* ((a-0 (DISPATCH19 v-e v-addr))) a-0))
(define (LEAF10 v-program v-expr)
  (let* ((a-0 (DISPATCH18 v-program v-expr))) a-0))
(define (LEAF11 v-r) v-r)
(define (LEAF12 v-prg v-C v-R v-D)
  (let* ((a-0 (DISPATCH17 v-R v-prg v-C v-D)) (a-1 (DISPATCH16 v-prg a-0)))
    a-1))
(define (LEAF13 v-prg v-C v-n v-id v-R v-D)
  (let* ((a-0 (DISPATCH15 v-R v-n))
         (a-1 (DISPATCH14 v-R v-n))
         (a-2 (DISPATCH13 v-prg v-id)))
    (cons (cons a-0 v-D) (cons a-1 (cons (cons a-2 v-C) '())))))
(define (LEAF14 v-C v-R v-D) (cons v-D (cons v-R (cons v-C '()))))
(define (LEAF15 v-C v-tree v-R v-D v-d)
  (let* ((a-0 (DISPATCH12 v-d v-tree)))
    (cons (cons v-d v-D) (cons v-R (cons (cons a-0 v-C) '())))))
(define (LEAF16 v-C v-addr v-R v-D v-d)
  (let* ((a-0 (DISPATCH11 v-d v-addr)))
    (cons (cons v-d v-D) (cons (cons a-0 v-R) (cons v-C '())))))
(define (LEAF17 v-C v-R v-r v-D v-d)
  (cons (cons (cons v-r v-d) v-D) (cons v-R (cons v-C '()))))
(define (LEAF18 v-C v-R v-t v-h v-D)
  (cons v-D (cons (cons (cons v-h v-t) v-R) (cons v-C '()))))
(define (LEAF19 v-C v-R v-a v-D)
  (cons v-D (cons (cons v-a v-R) (cons v-C '()))))
(define (LEAF20 v-C v-R v-d v-D)
  (cons v-D (cons (cons v-d v-R) (cons v-C '()))))
(define (LEAF21 v-C v-R v-m v-n v-D)
  (cons v-D (cons (cons (+ v-n v-m) v-R) (cons v-C '()))))
(define (LEAF22 v-C v-R v-m v-n v-D)
  (cons v-D (cons (cons (- v-n v-m) v-R) (cons v-C '()))))
(define (LEAF23 v-C v-R v-m v-n v-D)
  (cons v-D (cons (cons (* v-n v-m) v-R) (cons v-C '()))))
(define (LEAF24 v-C v-R v-m v-n v-D)
  (cons v-D (cons (cons (modulo v-n v-m) v-R) (cons v-C '()))))
(define (LEAF25 v-d v-fb v-tb v-req)
  (let* ((a-0 (DISPATCH10 v-d v-req)) (a-1 (DISPATCH9 v-tb v-d v-fb a-0))) a-1))
(define (LEAF26 v-leaf) v-leaf)
(define (LEAF27 v-d v-tb) (let* ((a-0 (DISPATCH8 v-d v-tb))) a-0))
(define (LEAF28 v-d v-fb) (let* ((a-0 (DISPATCH7 v-d v-fb))) a-0))
(define (LEAF29 v-d v-ts v-t)
  (let* ((a-0 (DISPATCH6 v-d v-t)) (a-1 (DISPATCH5 v-ts v-d a-0))) a-1))
(define (LEAF30 v-d) 'YES)
(define (LEAF31 v-d v-ts) (let* ((a-0 (DISPATCH4 v-d v-ts))) a-0))
(define (LEAF32) 'NO)
(define (LEAF33 v-d v-addr v-test)
  (let* ((a-0 (DISPATCH11 v-d v-addr)) (a-1 (DISPATCH3 a-0 v-test))) a-1))
(define (LEAF34 v-d v-addr* v-addr v-test)
  (let* ((a-0 (DISPATCH11 v-d v-addr))
         (a-1 (DISPATCH2 v-d v-addr*))
         (a-2 (DISPATCH1 a-0 a-1 v-test)))
    a-2))
(define (LEAF35) 'YES)
(define (LEAF36) 'NO)
(define (LEAF37) 'YES)
(define (LEAF38) 'NO)
(define (LEAF39 v-n) 'YES)
(define (LEAF40) 'NO)
(define (LEAF41 v-s) 'YES)
(define (LEAF42) 'NO)
(define (LEAF43) 'NO)
(define (LEAF44) 'YES)
(define (LEAF45 v-x) 'YES)
(define (LEAF46) 'NO)
(define (DISPATCH0 v0)
  (if (pair? v0)
      (if (equal? 'take (car v0))
          (if (pair? (cdr v0))
              (if (equal? 0 (car (cdr v0)))
                  (if (pair? (cdr (cdr v0)))
                      (if (equal? 'from (car (cdr (cdr v0))))
                          (if (pair? (cdr (cdr (cdr v0))))
                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                  (LEAF0)
                                  (FAIL v0))
                              (FAIL v0))
                          (FAIL v0))
                      (FAIL v0))
                  (if (number? (car (cdr v0)))
                      (if (pair? (cdr (cdr v0)))
                          (if (equal? 'from (car (cdr (cdr v0))))
                              (if (pair? (cdr (cdr (cdr v0))))
                                  (if (pair? (car (cdr (cdr (cdr v0)))))
                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                          (LEAF1 (cdr (car (cdr (cdr (cdr v0)))))
                                                 (car (car (cdr (cdr (cdr v0)))))
                                                 (car (cdr v0)))
                                          (FAIL v0))
                                      (FAIL v0))
                                  (FAIL v0))
                              (FAIL v0))
                          (FAIL v0))
                      (FAIL v0)))
              (FAIL v0))
          (if (equal? 'drop (car v0))
              (if (pair? (cdr v0))
                  (if (equal? 0 (car (cdr v0)))
                      (if (pair? (cdr (cdr v0)))
                          (if (equal? 'from (car (cdr (cdr v0))))
                              (if (pair? (cdr (cdr (cdr v0))))
                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                      (LEAF2 (car (cdr (cdr (cdr v0)))))
                                      (FAIL v0))
                                  (FAIL v0))
                              (FAIL v0))
                          (FAIL v0))
                      (if (number? (car (cdr v0)))
                          (if (pair? (cdr (cdr v0)))
                              (if (equal? 'from (car (cdr (cdr v0))))
                                  (if (pair? (cdr (cdr (cdr v0))))
                                      (if (pair? (car (cdr (cdr (cdr v0)))))
                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                              (LEAF3 (cdr (car (cdr (cdr (cdr v0)))))
                                                     (car (car (cdr (cdr (cdr v0)))))
                                                     (car (cdr v0)))
                                              (FAIL v0))
                                          (FAIL v0))
                                      (FAIL v0))
                                  (FAIL v0))
                              (FAIL v0))
                          (FAIL v0)))
                  (FAIL v0))
              (if (equal? 'lookup (car v0))
                  (if (pair? (cdr v0))
                      (if (pair? (cdr (cdr v0)))
                          (if (equal? 'in (car (cdr (cdr v0))))
                              (if (pair? (cdr (cdr (cdr v0))))
                                  (if (null? (car (cdr (cdr (cdr v0)))))
                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                          (LEAF4)
                                          (FAIL v0))
                                      (if (pair? (car (cdr (cdr (cdr v0)))))
                                          (if (pair? (car (car (cdr (cdr (cdr v0))))))
                                              (if (equal?
                                                   (car (cdr v0))
                                                   (car (car (car (cdr (cdr (cdr v0)))))))
                                                  (if (pair? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                      (if (null? (cdr (cdr (car (car (cdr (cdr (cdr v0))))))))
                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                              (LEAF5 (car (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                     (car (cdr v0)))
                                                              (FAIL v0))
                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                              (LEAF6 (cdr (car (cdr (cdr (cdr v0)))))
                                                                     (car (cdr v0)))
                                                              (FAIL v0)))
                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                          (LEAF6 (cdr (car (cdr (cdr (cdr v0)))))
                                                                 (car (cdr v0)))
                                                          (FAIL v0)))
                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                      (LEAF6 (cdr (car (cdr (cdr (cdr v0)))))
                                                             (car (cdr v0)))
                                                      (FAIL v0)))
                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                  (LEAF6 (cdr (car (cdr (cdr (cdr v0)))))
                                                         (car (cdr v0)))
                                                  (FAIL v0)))
                                          (FAIL v0)))
                                  (FAIL v0))
                              (FAIL v0))
                          (FAIL v0))
                      (FAIL v0))
                  (if (equal? 'extract (car v0))
                      (if (pair? (cdr v0))
                          (if (null? (car (cdr v0)))
                              (if (pair? (cdr (cdr v0)))
                                  (if (equal? 'from (car (cdr (cdr v0))))
                                      (if (pair? (cdr (cdr (cdr v0))))
                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                              (LEAF7 (car (cdr (cdr (cdr v0)))))
                                              (FAIL v0))
                                          (FAIL v0))
                                      (FAIL v0))
                                  (FAIL v0))
                              (if (pair? (car (cdr v0)))
                                  (if (equal? 'A (car (car (cdr v0))))
                                      (if (pair? (cdr (cdr v0)))
                                          (if (equal?
                                               'from
                                               (car (cdr (cdr v0))))
                                              (if (pair? (cdr (cdr (cdr v0))))
                                                  (if (pair? (car (cdr (cdr (cdr v0)))))
                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                          (LEAF8 (car (car (cdr (cdr (cdr v0)))))
                                                                 (cdr (car (cdr v0))))
                                                          (FAIL v0))
                                                      (FAIL v0))
                                                  (FAIL v0))
                                              (FAIL v0))
                                          (FAIL v0))
                                      (if (equal? 'D (car (car (cdr v0))))
                                          (if (pair? (cdr (cdr v0)))
                                              (if (equal?
                                                   'from
                                                   (car (cdr (cdr v0))))
                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                      (if (pair? (car (cdr (cdr (cdr v0)))))
                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                              (LEAF9 (cdr (car (cdr (cdr (cdr v0)))))
                                                                     (cdr (car (cdr v0))))
                                                              (FAIL v0))
                                                          (FAIL v0))
                                                      (FAIL v0))
                                                  (FAIL v0))
                                              (FAIL v0))
                                          (FAIL v0)))
                                  (FAIL v0)))
                          (FAIL v0))
                      (if (equal? 'run-program (car v0))
                          (if (pair? (cdr v0))
                              (if (pair? (cdr (cdr v0)))
                                  (if (null? (cdr (cdr (cdr v0))))
                                      (LEAF10
                                       (car (cdr (cdr v0)))
                                       (car (cdr v0)))
                                      (FAIL v0))
                                  (FAIL v0))
                              (FAIL v0))
                          (if (equal? 'step* (car v0))
                              (if (pair? (cdr v0))
                                  (if (pair? (car (cdr v0)))
                                      (if (pair? (cdr (car (cdr v0))))
                                          (if (pair? (car (cdr (car (cdr v0)))))
                                              (if (pair? (cdr (cdr (car (cdr v0)))))
                                                  (if (null? (car (cdr (cdr (car (cdr v0))))))
                                                      (if (null? (cdr (cdr (cdr (car (cdr v0))))))
                                                          (if (pair? (cdr (cdr v0)))
                                                              (if (null? (cdr (cdr (cdr v0))))
                                                                  (LEAF11
                                                                   (car (car (cdr (car (cdr v0))))))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (FAIL v0))
                                                      (if (null? (cdr (cdr (cdr (car (cdr v0))))))
                                                          (if (pair? (cdr (cdr v0)))
                                                              (if (null? (cdr (cdr (cdr v0))))
                                                                  (LEAF12
                                                                   (car (cdr (cdr v0)))
                                                                   (car (cdr (cdr (car (cdr v0)))))
                                                                   (car (cdr (car (cdr v0))))
                                                                   (car (car (cdr v0))))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (FAIL v0)))
                                                  (FAIL v0))
                                              (if (pair? (cdr (cdr (car (cdr v0)))))
                                                  (if (null? (cdr (cdr (cdr (car (cdr v0))))))
                                                      (if (pair? (cdr (cdr v0)))
                                                          (if (null? (cdr (cdr (cdr v0))))
                                                              (LEAF12
                                                               (car (cdr (cdr v0)))
                                                               (car (cdr (cdr (car (cdr v0)))))
                                                               (car (cdr (car (cdr v0))))
                                                               (car (car (cdr v0))))
                                                              (FAIL v0))
                                                          (FAIL v0))
                                                      (FAIL v0))
                                                  (FAIL v0)))
                                          (FAIL v0))
                                      (FAIL v0))
                                  (FAIL v0))
                              (if (equal? 'step (car v0))
                                  (if (pair? (cdr v0))
                                      (if (pair? (cdr (cdr v0)))
                                          (if (pair? (cdr (cdr (cdr v0))))
                                              (if (pair? (car (cdr (cdr (cdr v0)))))
                                                  (if (pair? (car (car (cdr (cdr (cdr v0))))))
                                                      (if (equal?
                                                           'CALL
                                                           (car (car (car (cdr (cdr (cdr v0)))))))
                                                          (if (pair? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                              (if (symbol?
                                                                   (car (cdr (car (car (cdr (cdr (cdr v0))))))))
                                                                  (if (pair? (cdr (cdr (car (car (cdr (cdr (cdr v0))))))))
                                                                      (if (number?
                                                                           (car (cdr (cdr (car (car (cdr (cdr (cdr v0)))))))))
                                                                          (if (null? (cdr (cdr (cdr (car (car (cdr (cdr (cdr v0)))))))))
                                                                              (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                  (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                      (LEAF13
                                                                                       (car (cdr (cdr (cdr (cdr v0)))))
                                                                                       (cdr (car (cdr (cdr (cdr v0)))))
                                                                                       (car (cdr (cdr (car (car (cdr (cdr (cdr v0))))))))
                                                                                       (car (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                       (car (cdr (cdr v0)))
                                                                                       (car (cdr v0)))
                                                                                      (FAIL v0))
                                                                                  (FAIL v0))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (if (pair? (car (cdr v0)))
                                                              (if (equal?
                                                                   'RET
                                                                   (car (car (car (cdr (cdr (cdr v0)))))))
                                                                  (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                      (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                          (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                              (LEAF14
                                                                               (cdr (car (cdr (cdr (cdr v0)))))
                                                                               (car (cdr (cdr v0)))
                                                                               (cdr (car (cdr v0))))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (if (equal?
                                                                       'DISP
                                                                       (car (car (car (cdr (cdr (cdr v0)))))))
                                                                      (if (pair? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                          (if (null? (cdr (cdr (car (car (cdr (cdr (cdr v0))))))))
                                                                              (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                  (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                      (LEAF15
                                                                                       (cdr (car (cdr (cdr (cdr v0)))))
                                                                                       (car (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                       (car (cdr (cdr v0)))
                                                                                       (cdr (car (cdr v0)))
                                                                                       (car (car (cdr v0))))
                                                                                      (FAIL v0))
                                                                                  (FAIL v0))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (if (equal?
                                                                           'LD
                                                                           (car (car (car (cdr (cdr (cdr v0)))))))
                                                                          (if (pair? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                              (if (null? (cdr (cdr (car (car (cdr (cdr (cdr v0))))))))
                                                                                  (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                      (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                          (LEAF16
                                                                                           (cdr (car (cdr (cdr (cdr v0)))))
                                                                                           (car (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                           (car (cdr (cdr v0)))
                                                                                           (cdr (car (cdr v0)))
                                                                                           (car (car (cdr v0))))
                                                                                          (FAIL v0))
                                                                                      (FAIL v0))
                                                                                  (FAIL v0))
                                                                              (FAIL v0))
                                                                          (if (pair? (car (cdr (cdr v0))))
                                                                              (if (equal?
                                                                                   'ST
                                                                                   (car (car (car (cdr (cdr (cdr v0)))))))
                                                                                  (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                      (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                          (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                              (LEAF17
                                                                                               (cdr (car (cdr (cdr (cdr v0)))))
                                                                                               (cdr (car (cdr (cdr v0))))
                                                                                               (car (car (cdr (cdr v0))))
                                                                                               (cdr (car (cdr v0)))
                                                                                               (car (car (cdr v0))))
                                                                                              (FAIL v0))
                                                                                          (FAIL v0))
                                                                                      (FAIL v0))
                                                                                  (if (pair? (cdr (car (cdr (cdr v0)))))
                                                                                      (if (equal?
                                                                                           'CONS
                                                                                           (car (car (car (cdr (cdr (cdr v0)))))))
                                                                                          (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                              (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                                  (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                                      (LEAF18
                                                                                                       (cdr (car (cdr (cdr (cdr v0)))))
                                                                                                       (cdr (cdr (car (cdr (cdr v0)))))
                                                                                                       (car (cdr (car (cdr (cdr v0)))))
                                                                                                       (car (car (cdr (cdr v0))))
                                                                                                       (car (cdr v0)))
                                                                                                      (FAIL v0))
                                                                                                  (FAIL v0))
                                                                                              (FAIL v0))
                                                                                          (if (pair? (car (car (cdr (cdr v0)))))
                                                                                              (if (equal?
                                                                                                   'CAR
                                                                                                   (car (car (car (cdr (cdr (cdr v0)))))))
                                                                                                  (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                                      (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                                          (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                                              (LEAF19
                                                                                                               (cdr (car (cdr (cdr (cdr v0)))))
                                                                                                               (cdr (car (cdr (cdr v0))))
                                                                                                               (car (car (car (cdr (cdr v0)))))
                                                                                                               (car (cdr v0)))
                                                                                                              (FAIL v0))
                                                                                                          (FAIL v0))
                                                                                                      (FAIL v0))
                                                                                                  (if (equal?
                                                                                                       'CDR
                                                                                                       (car (car (car (cdr (cdr (cdr v0)))))))
                                                                                                      (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                                          (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                                              (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                                                  (LEAF20
                                                                                                                   (cdr (car (cdr (cdr (cdr v0)))))
                                                                                                                   (cdr (car (cdr (cdr v0))))
                                                                                                                   (cdr (car (car (cdr (cdr v0)))))
                                                                                                                   (car (cdr v0)))
                                                                                                                  (FAIL v0))
                                                                                                              (FAIL v0))
                                                                                                          (FAIL v0))
                                                                                                      (FAIL v0)))
                                                                                              (if (number?
                                                                                                   (car (car (cdr (cdr v0)))))
                                                                                                  (if (number?
                                                                                                       (car (cdr (car (cdr (cdr v0))))))
                                                                                                      (if (equal?
                                                                                                           'ADD
                                                                                                           (car (car (car (cdr (cdr (cdr v0)))))))
                                                                                                          (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                                              (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                                                  (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                                                      (LEAF21
                                                                                                                       (cdr (car (cdr (cdr (cdr v0)))))
                                                                                                                       (cdr (cdr (car (cdr (cdr v0)))))
                                                                                                                       (car (cdr (car (cdr (cdr v0)))))
                                                                                                                       (car (car (cdr (cdr v0))))
                                                                                                                       (car (cdr v0)))
                                                                                                                      (FAIL v0))
                                                                                                                  (FAIL v0))
                                                                                                              (FAIL v0))
                                                                                                          (if (equal?
                                                                                                               'SUB
                                                                                                               (car (car (car (cdr (cdr (cdr v0)))))))
                                                                                                              (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                                                  (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                                                      (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                                                          (LEAF22
                                                                                                                           (cdr (car (cdr (cdr (cdr v0)))))
                                                                                                                           (cdr (cdr (car (cdr (cdr v0)))))
                                                                                                                           (car (cdr (car (cdr (cdr v0)))))
                                                                                                                           (car (car (cdr (cdr v0))))
                                                                                                                           (car (cdr v0)))
                                                                                                                          (FAIL v0))
                                                                                                                      (FAIL v0))
                                                                                                                  (FAIL v0))
                                                                                                              (if (equal?
                                                                                                                   'MUL
                                                                                                                   (car (car (car (cdr (cdr (cdr v0)))))))
                                                                                                                  (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                                                      (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                                                          (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                                                              (LEAF23
                                                                                                                               (cdr (car (cdr (cdr (cdr v0)))))
                                                                                                                               (cdr (cdr (car (cdr (cdr v0)))))
                                                                                                                               (car (cdr (car (cdr (cdr v0)))))
                                                                                                                               (car (car (cdr (cdr v0))))
                                                                                                                               (car (cdr v0)))
                                                                                                                              (FAIL v0))
                                                                                                                          (FAIL v0))
                                                                                                                      (FAIL v0))
                                                                                                                  (if (equal?
                                                                                                                       'MOD
                                                                                                                       (car (car (car (cdr (cdr (cdr v0)))))))
                                                                                                                      (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                                                          (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                                                              (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                                                                  (LEAF24
                                                                                                                                   (cdr (car (cdr (cdr (cdr v0)))))
                                                                                                                                   (cdr (cdr (car (cdr (cdr v0)))))
                                                                                                                                   (car (cdr (car (cdr (cdr v0)))))
                                                                                                                                   (car (car (cdr (cdr v0))))
                                                                                                                                   (car (cdr v0)))
                                                                                                                                  (FAIL v0))
                                                                                                                              (FAIL v0))
                                                                                                                          (FAIL v0))
                                                                                                                      (FAIL v0)))))
                                                                                                      (FAIL v0))
                                                                                                  (FAIL v0))))
                                                                                      (if (pair? (car (car (cdr (cdr v0)))))
                                                                                          (if (equal?
                                                                                               'CAR
                                                                                               (car (car (car (cdr (cdr (cdr v0)))))))
                                                                                              (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                                  (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                                      (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                                          (LEAF19
                                                                                                           (cdr (car (cdr (cdr (cdr v0)))))
                                                                                                           (cdr (car (cdr (cdr v0))))
                                                                                                           (car (car (car (cdr (cdr v0)))))
                                                                                                           (car (cdr v0)))
                                                                                                          (FAIL v0))
                                                                                                      (FAIL v0))
                                                                                                  (FAIL v0))
                                                                                              (if (equal?
                                                                                                   'CDR
                                                                                                   (car (car (car (cdr (cdr (cdr v0)))))))
                                                                                                  (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                                      (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                                          (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                                              (LEAF20
                                                                                                               (cdr (car (cdr (cdr (cdr v0)))))
                                                                                                               (cdr (car (cdr (cdr v0))))
                                                                                                               (cdr (car (car (cdr (cdr v0)))))
                                                                                                               (car (cdr v0)))
                                                                                                              (FAIL v0))
                                                                                                          (FAIL v0))
                                                                                                      (FAIL v0))
                                                                                                  (FAIL v0)))
                                                                                          (FAIL v0))))
                                                                              (FAIL v0)))))
                                                              (if (pair? (car (cdr (cdr v0))))
                                                                  (if (pair? (cdr (car (cdr (cdr v0)))))
                                                                      (if (equal?
                                                                           'CONS
                                                                           (car (car (car (cdr (cdr (cdr v0)))))))
                                                                          (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                              (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                  (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                      (LEAF18
                                                                                       (cdr (car (cdr (cdr (cdr v0)))))
                                                                                       (cdr (cdr (car (cdr (cdr v0)))))
                                                                                       (car (cdr (car (cdr (cdr v0)))))
                                                                                       (car (car (cdr (cdr v0))))
                                                                                       (car (cdr v0)))
                                                                                      (FAIL v0))
                                                                                  (FAIL v0))
                                                                              (FAIL v0))
                                                                          (if (pair? (car (car (cdr (cdr v0)))))
                                                                              (if (equal?
                                                                                   'CAR
                                                                                   (car (car (car (cdr (cdr (cdr v0)))))))
                                                                                  (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                      (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                          (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                              (LEAF19
                                                                                               (cdr (car (cdr (cdr (cdr v0)))))
                                                                                               (cdr (car (cdr (cdr v0))))
                                                                                               (car (car (car (cdr (cdr v0)))))
                                                                                               (car (cdr v0)))
                                                                                              (FAIL v0))
                                                                                          (FAIL v0))
                                                                                      (FAIL v0))
                                                                                  (if (equal?
                                                                                       'CDR
                                                                                       (car (car (car (cdr (cdr (cdr v0)))))))
                                                                                      (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                          (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                              (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                                  (LEAF20
                                                                                                   (cdr (car (cdr (cdr (cdr v0)))))
                                                                                                   (cdr (car (cdr (cdr v0))))
                                                                                                   (cdr (car (car (cdr (cdr v0)))))
                                                                                                   (car (cdr v0)))
                                                                                                  (FAIL v0))
                                                                                              (FAIL v0))
                                                                                          (FAIL v0))
                                                                                      (FAIL v0)))
                                                                              (if (number?
                                                                                   (car (car (cdr (cdr v0)))))
                                                                                  (if (number?
                                                                                       (car (cdr (car (cdr (cdr v0))))))
                                                                                      (if (equal?
                                                                                           'ADD
                                                                                           (car (car (car (cdr (cdr (cdr v0)))))))
                                                                                          (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                              (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                                  (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                                      (LEAF21
                                                                                                       (cdr (car (cdr (cdr (cdr v0)))))
                                                                                                       (cdr (cdr (car (cdr (cdr v0)))))
                                                                                                       (car (cdr (car (cdr (cdr v0)))))
                                                                                                       (car (car (cdr (cdr v0))))
                                                                                                       (car (cdr v0)))
                                                                                                      (FAIL v0))
                                                                                                  (FAIL v0))
                                                                                              (FAIL v0))
                                                                                          (if (equal?
                                                                                               'SUB
                                                                                               (car (car (car (cdr (cdr (cdr v0)))))))
                                                                                              (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                                  (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                                      (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                                          (LEAF22
                                                                                                           (cdr (car (cdr (cdr (cdr v0)))))
                                                                                                           (cdr (cdr (car (cdr (cdr v0)))))
                                                                                                           (car (cdr (car (cdr (cdr v0)))))
                                                                                                           (car (car (cdr (cdr v0))))
                                                                                                           (car (cdr v0)))
                                                                                                          (FAIL v0))
                                                                                                      (FAIL v0))
                                                                                                  (FAIL v0))
                                                                                              (if (equal?
                                                                                                   'MUL
                                                                                                   (car (car (car (cdr (cdr (cdr v0)))))))
                                                                                                  (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                                      (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                                          (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                                              (LEAF23
                                                                                                               (cdr (car (cdr (cdr (cdr v0)))))
                                                                                                               (cdr (cdr (car (cdr (cdr v0)))))
                                                                                                               (car (cdr (car (cdr (cdr v0)))))
                                                                                                               (car (car (cdr (cdr v0))))
                                                                                                               (car (cdr v0)))
                                                                                                              (FAIL v0))
                                                                                                          (FAIL v0))
                                                                                                      (FAIL v0))
                                                                                                  (if (equal?
                                                                                                       'MOD
                                                                                                       (car (car (car (cdr (cdr (cdr v0)))))))
                                                                                                      (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                                          (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                                              (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                                                  (LEAF24
                                                                                                                   (cdr (car (cdr (cdr (cdr v0)))))
                                                                                                                   (cdr (cdr (car (cdr (cdr v0)))))
                                                                                                                   (car (cdr (car (cdr (cdr v0)))))
                                                                                                                   (car (car (cdr (cdr v0))))
                                                                                                                   (car (cdr v0)))
                                                                                                                  (FAIL v0))
                                                                                                              (FAIL v0))
                                                                                                          (FAIL v0))
                                                                                                      (FAIL v0)))))
                                                                                      (FAIL v0))
                                                                                  (FAIL v0))))
                                                                      (if (pair? (car (car (cdr (cdr v0)))))
                                                                          (if (equal?
                                                                               'CAR
                                                                               (car (car (car (cdr (cdr (cdr v0)))))))
                                                                              (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                  (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                      (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                          (LEAF19
                                                                                           (cdr (car (cdr (cdr (cdr v0)))))
                                                                                           (cdr (car (cdr (cdr v0))))
                                                                                           (car (car (car (cdr (cdr v0)))))
                                                                                           (car (cdr v0)))
                                                                                          (FAIL v0))
                                                                                      (FAIL v0))
                                                                                  (FAIL v0))
                                                                              (if (equal?
                                                                                   'CDR
                                                                                   (car (car (car (cdr (cdr (cdr v0)))))))
                                                                                  (if (null? (cdr (car (car (cdr (cdr (cdr v0)))))))
                                                                                      (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                                          (if (null? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                                              (LEAF20
                                                                                               (cdr (car (cdr (cdr (cdr v0)))))
                                                                                               (cdr (car (cdr (cdr v0))))
                                                                                               (cdr (car (car (cdr (cdr v0)))))
                                                                                               (car (cdr v0)))
                                                                                              (FAIL v0))
                                                                                          (FAIL v0))
                                                                                      (FAIL v0))
                                                                                  (FAIL v0)))
                                                                          (FAIL v0)))
                                                                  (FAIL v0))))
                                                      (FAIL v0))
                                                  (FAIL v0))
                                              (FAIL v0))
                                          (FAIL v0))
                                      (FAIL v0))
                                  (if (equal? 'dispatch (car v0))
                                      (if (pair? (cdr v0))
                                          (if (pair? (car (cdr v0)))
                                              (if (equal?
                                                   'IF
                                                   (car (car (cdr v0))))
                                                  (if (pair? (cdr (car (cdr v0))))
                                                      (if (pair? (cdr (cdr (car (cdr v0)))))
                                                          (if (pair? (cdr (cdr (cdr (car (cdr v0))))))
                                                              (if (null? (cdr (cdr (cdr (cdr (car (cdr v0)))))))
                                                                  (if (pair? (cdr (cdr v0)))
                                                                      (if (equal?
                                                                           'wrt
                                                                           (car (cdr (cdr v0))))
                                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                                  (LEAF25
                                                                                   (car (cdr (cdr (cdr v0))))
                                                                                   (car (cdr (cdr (cdr (car (cdr v0))))))
                                                                                   (car (cdr (cdr (car (cdr v0)))))
                                                                                   (car (cdr (car (cdr v0)))))
                                                                                  (FAIL v0))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (if (pair? (cdr (cdr v0)))
                                                                      (if (equal?
                                                                           'wrt
                                                                           (car (cdr (cdr v0))))
                                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                                  (LEAF26
                                                                                   (car (cdr v0)))
                                                                                  (FAIL v0))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (FAIL v0)))
                                                              (if (pair? (cdr (cdr v0)))
                                                                  (if (equal?
                                                                       'wrt
                                                                       (car (cdr (cdr v0))))
                                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                              (LEAF26
                                                                               (car (cdr v0)))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (FAIL v0)))
                                                          (if (pair? (cdr (cdr v0)))
                                                              (if (equal?
                                                                   'wrt
                                                                   (car (cdr (cdr v0))))
                                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                          (LEAF26
                                                                           (car (cdr v0)))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (FAIL v0)))
                                                      (if (pair? (cdr (cdr v0)))
                                                          (if (equal?
                                                               'wrt
                                                               (car (cdr (cdr v0))))
                                                              (if (pair? (cdr (cdr (cdr v0))))
                                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                      (LEAF26
                                                                       (car (cdr v0)))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (FAIL v0)))
                                                  (if (pair? (cdr (cdr v0)))
                                                      (if (equal?
                                                           'wrt
                                                           (car (cdr (cdr v0))))
                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                  (LEAF26
                                                                   (car (cdr v0)))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (FAIL v0))
                                                      (FAIL v0)))
                                              (if (pair? (cdr (cdr v0)))
                                                  (if (equal?
                                                       'wrt
                                                       (car (cdr (cdr v0))))
                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                              (LEAF26
                                                               (car (cdr v0)))
                                                              (FAIL v0))
                                                          (FAIL v0))
                                                      (FAIL v0))
                                                  (FAIL v0)))
                                          (FAIL v0))
                                      (if (equal? 'dispatch* (car v0))
                                          (if (pair? (cdr v0))
                                              (if (equal? 'YES (car (cdr v0)))
                                                  (if (pair? (cdr (cdr v0)))
                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                          (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                              (if (equal?
                                                                   'wrt
                                                                   (car (cdr (cdr (cdr (cdr v0))))))
                                                                  (if (pair? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                      (if (null? (cdr (cdr (cdr (cdr (cdr (cdr v0)))))))
                                                                          (LEAF27
                                                                           (car (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                           (car (cdr (cdr v0))))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (FAIL v0))
                                                      (FAIL v0))
                                                  (if (equal?
                                                       'NO
                                                       (car (cdr v0)))
                                                      (if (pair? (cdr (cdr v0)))
                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                              (if (pair? (cdr (cdr (cdr (cdr v0)))))
                                                                  (if (equal?
                                                                       'wrt
                                                                       (car (cdr (cdr (cdr (cdr v0))))))
                                                                      (if (pair? (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                          (if (null? (cdr (cdr (cdr (cdr (cdr (cdr v0)))))))
                                                                              (LEAF28
                                                                               (car (cdr (cdr (cdr (cdr (cdr v0))))))
                                                                               (car (cdr (cdr (cdr v0)))))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (FAIL v0))
                                                      (FAIL v0)))
                                              (FAIL v0))
                                          (if (equal? 'test (car v0))
                                              (if (pair? (cdr v0))
                                                  (if (pair? (car (cdr v0)))
                                                      (if (equal?
                                                           'and
                                                           (car (car (cdr v0))))
                                                          (if (pair? (cdr (car (cdr v0))))
                                                              (if (pair? (cdr (cdr v0)))
                                                                  (if (equal?
                                                                       'wrt
                                                                       (car (cdr (cdr v0))))
                                                                      (if (pair? (cdr (cdr (cdr v0))))
                                                                          (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                              (LEAF29
                                                                               (car (cdr (cdr (cdr v0))))
                                                                               (cdr (cdr (car (cdr v0))))
                                                                               (car (cdr (car (cdr v0)))))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (if (null? (cdr (car (cdr v0))))
                                                                  (if (pair? (cdr (cdr v0)))
                                                                      (if (equal?
                                                                           'wrt
                                                                           (car (cdr (cdr v0))))
                                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                                  (LEAF30
                                                                                   (car (cdr (cdr (cdr v0)))))
                                                                                  (FAIL v0))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (FAIL v0)))
                                                          (if (pair? (cdr (car (cdr v0))))
                                                              (if (null? (cdr (cdr (car (cdr v0)))))
                                                                  (if (pair? (cdr (cdr v0)))
                                                                      (if (equal?
                                                                           'wrt
                                                                           (car (cdr (cdr v0))))
                                                                          (if (pair? (cdr (cdr (cdr v0))))
                                                                              (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                                  (LEAF33
                                                                                   (car (cdr (cdr (cdr v0))))
                                                                                   (car (cdr (car (cdr v0))))
                                                                                   (car (car (cdr v0))))
                                                                                  (FAIL v0))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (if (pair? (cdr (cdr (car (cdr v0)))))
                                                                      (if (null? (cdr (cdr (cdr (car (cdr v0))))))
                                                                          (if (pair? (cdr (cdr v0)))
                                                                              (if (equal?
                                                                                   'wrt
                                                                                   (car (cdr (cdr v0))))
                                                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                                          (LEAF34
                                                                                           (car (cdr (cdr (cdr v0))))
                                                                                           (car (cdr (cdr (car (cdr v0)))))
                                                                                           (car (cdr (car (cdr v0))))
                                                                                           (car (car (cdr v0))))
                                                                                          (FAIL v0))
                                                                                      (FAIL v0))
                                                                                  (FAIL v0))
                                                                              (FAIL v0))
                                                                          (FAIL v0))
                                                                      (FAIL v0)))
                                                              (FAIL v0)))
                                                      (FAIL v0))
                                                  (FAIL v0))
                                              (if (equal? 'aaand-test (car v0))
                                                  (if (pair? (cdr v0))
                                                      (if (equal?
                                                           'YES
                                                           (car (cdr v0)))
                                                          (if (pair? (cdr (cdr v0)))
                                                              (if (pair? (cdr (cdr (cdr v0))))
                                                                  (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                      (LEAF31
                                                                       (car (cdr (cdr (cdr v0))))
                                                                       (car (cdr (cdr v0))))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (FAIL v0))
                                                          (if (equal?
                                                               'NO
                                                               (car (cdr v0)))
                                                              (if (pair? (cdr (cdr v0)))
                                                                  (if (pair? (cdr (cdr (cdr v0))))
                                                                      (if (null? (cdr (cdr (cdr (cdr v0)))))
                                                                          (LEAF32)
                                                                          (FAIL v0))
                                                                      (FAIL v0))
                                                                  (FAIL v0))
                                                              (FAIL v0)))
                                                      (FAIL v0))
                                                  (if (equal? 'NIL? (car v0))
                                                      (if (pair? (cdr v0))
                                                          (if (null? (car (cdr v0)))
                                                              (if (null? (cdr (cdr v0)))
                                                                  (LEAF35)
                                                                  (FAIL v0))
                                                              (if (null? (cdr (cdr v0)))
                                                                  (LEAF36)
                                                                  (FAIL v0)))
                                                          (FAIL v0))
                                                      (if (equal?
                                                           'CONS?
                                                           (car v0))
                                                          (if (pair? (cdr v0))
                                                              (if (pair? (car (cdr v0)))
                                                                  (if (null? (cdr (cdr v0)))
                                                                      (LEAF37)
                                                                      (FAIL v0))
                                                                  (if (null? (cdr (cdr v0)))
                                                                      (LEAF38)
                                                                      (FAIL v0)))
                                                              (FAIL v0))
                                                          (if (equal?
                                                               'NUM?
                                                               (car v0))
                                                              (if (pair? (cdr v0))
                                                                  (if (number?
                                                                       (car (cdr v0)))
                                                                      (if (null? (cdr (cdr v0)))
                                                                          (LEAF39
                                                                           (car (cdr v0)))
                                                                          (FAIL v0))
                                                                      (if (null? (cdr (cdr v0)))
                                                                          (LEAF40)
                                                                          (FAIL v0)))
                                                                  (FAIL v0))
                                                              (if (equal?
                                                                   'SYM?
                                                                   (car v0))
                                                                  (if (pair? (cdr v0))
                                                                      (if (symbol?
                                                                           (car (cdr v0)))
                                                                          (if (null? (cdr (cdr v0)))
                                                                              (LEAF41
                                                                               (car (cdr v0)))
                                                                              (FAIL v0))
                                                                          (if (null? (cdr (cdr v0)))
                                                                              (LEAF42)
                                                                              (FAIL v0)))
                                                                      (FAIL v0))
                                                                  (if (equal?
                                                                       'ATM?
                                                                       (car v0))
                                                                      (if (pair? (cdr v0))
                                                                          (if (pair? (car (cdr v0)))
                                                                              (if (null? (cdr (cdr v0)))
                                                                                  (LEAF43)
                                                                                  (FAIL v0))
                                                                              (if (null? (cdr (cdr v0)))
                                                                                  (LEAF44)
                                                                                  (FAIL v0)))
                                                                          (FAIL v0))
                                                                      (if (equal?
                                                                           'EQ?
                                                                           (car v0))
                                                                          (if (pair? (cdr v0))
                                                                              (if (pair? (cdr (cdr v0)))
                                                                                  (if (equal?
                                                                                       (car (cdr v0))
                                                                                       (car (cdr (cdr v0))))
                                                                                      (if (null? (cdr (cdr (cdr v0))))
                                                                                          (LEAF45
                                                                                           (car (cdr v0)))
                                                                                          (FAIL v0))
                                                                                      (if (null? (cdr (cdr (cdr v0))))
                                                                                          (LEAF46)
                                                                                          (FAIL v0)))
                                                                                  (FAIL v0))
                                                                              (FAIL v0))
                                                                          (FAIL v0))))))))))))))))))
      (FAIL v0)))
(define (DISPATCH1 a-0 a-1 v-test)
  (if (equal? 'take v-test)
      (if (equal? 0 a-0)
          (if (equal? 'from a-1)
              (FAIL (cons v-test (cons a-0 (cons a-1 '()))))
              (FAIL (cons v-test (cons a-0 (cons a-1 '())))))
          (if (number? a-0)
              (if (equal? 'from a-1)
                  (FAIL (cons v-test (cons a-0 (cons a-1 '()))))
                  (FAIL (cons v-test (cons a-0 (cons a-1 '())))))
              (FAIL (cons v-test (cons a-0 (cons a-1 '()))))))
      (if (equal? 'drop v-test)
          (if (equal? 0 a-0)
              (if (equal? 'from a-1)
                  (FAIL (cons v-test (cons a-0 (cons a-1 '()))))
                  (FAIL (cons v-test (cons a-0 (cons a-1 '())))))
              (if (number? a-0)
                  (if (equal? 'from a-1)
                      (FAIL (cons v-test (cons a-0 (cons a-1 '()))))
                      (FAIL (cons v-test (cons a-0 (cons a-1 '())))))
                  (FAIL (cons v-test (cons a-0 (cons a-1 '()))))))
          (if (equal? 'lookup v-test)
              (if (equal? 'in a-1)
                  (FAIL (cons v-test (cons a-0 (cons a-1 '()))))
                  (FAIL (cons v-test (cons a-0 (cons a-1 '())))))
              (if (equal? 'extract v-test)
                  (if (null? a-0)
                      (if (equal? 'from a-1)
                          (FAIL (cons v-test (cons a-0 (cons a-1 '()))))
                          (FAIL (cons v-test (cons a-0 (cons a-1 '())))))
                      (if (pair? a-0)
                          (if (equal? 'A (car a-0))
                              (if (equal? 'from a-1)
                                  (FAIL (cons v-test (cons a-0 (cons a-1 '()))))
                                  (FAIL (cons v-test (cons a-0 (cons a-1 '())))))
                              (if (equal? 'D (car a-0))
                                  (if (equal? 'from a-1)
                                      (FAIL (cons v-test
                                                  (cons a-0 (cons a-1 '()))))
                                      (FAIL (cons v-test
                                                  (cons a-0 (cons a-1 '())))))
                                  (FAIL (cons v-test (cons a-0 (cons a-1 '()))))))
                          (FAIL (cons v-test (cons a-0 (cons a-1 '()))))))
                  (if (equal? 'run-program v-test)
                      (LEAF10 a-1 a-0)
                      (if (equal? 'step* v-test)
                          (if (pair? a-0)
                              (if (pair? (cdr a-0))
                                  (if (pair? (car (cdr a-0)))
                                      (if (pair? (cdr (cdr a-0)))
                                          (if (null? (car (cdr (cdr a-0))))
                                              (if (null? (cdr (cdr (cdr a-0))))
                                                  (LEAF11
                                                   (car (car (cdr a-0))))
                                                  (FAIL (cons v-test
                                                              (cons a-0
                                                                    (cons a-1
                                                                          '())))))
                                              (if (null? (cdr (cdr (cdr a-0))))
                                                  (LEAF12
                                                   a-1
                                                   (car (cdr (cdr a-0)))
                                                   (car (cdr a-0))
                                                   (car a-0))
                                                  (FAIL (cons v-test
                                                              (cons a-0
                                                                    (cons a-1
                                                                          '()))))))
                                          (FAIL (cons v-test
                                                      (cons a-0 (cons a-1 '())))))
                                      (if (pair? (cdr (cdr a-0)))
                                          (if (null? (cdr (cdr (cdr a-0))))
                                              (LEAF12
                                               a-1
                                               (car (cdr (cdr a-0)))
                                               (car (cdr a-0))
                                               (car a-0))
                                              (FAIL (cons v-test
                                                          (cons a-0
                                                                (cons a-1 '())))))
                                          (FAIL (cons v-test
                                                      (cons a-0 (cons a-1 '()))))))
                                  (FAIL (cons v-test (cons a-0 (cons a-1 '())))))
                              (FAIL (cons v-test (cons a-0 (cons a-1 '())))))
                          (if (equal? 'step v-test)
                              (FAIL (cons v-test (cons a-0 (cons a-1 '()))))
                              (if (equal? 'dispatch v-test)
                                  (if (pair? a-0)
                                      (if (equal? 'IF (car a-0))
                                          (if (pair? (cdr a-0))
                                              (if (pair? (cdr (cdr a-0)))
                                                  (if (pair? (cdr (cdr (cdr a-0))))
                                                      (if (null? (cdr (cdr (cdr (cdr a-0)))))
                                                          (if (equal? 'wrt a-1)
                                                              (FAIL (cons v-test
                                                                          (cons a-0
                                                                                (cons a-1
                                                                                      '()))))
                                                              (FAIL (cons v-test
                                                                          (cons a-0
                                                                                (cons a-1
                                                                                      '())))))
                                                          (if (equal? 'wrt a-1)
                                                              (FAIL (cons v-test
                                                                          (cons a-0
                                                                                (cons a-1
                                                                                      '()))))
                                                              (FAIL (cons v-test
                                                                          (cons a-0
                                                                                (cons a-1
                                                                                      '()))))))
                                                      (if (equal? 'wrt a-1)
                                                          (FAIL (cons v-test
                                                                      (cons a-0
                                                                            (cons a-1
                                                                                  '()))))
                                                          (FAIL (cons v-test
                                                                      (cons a-0
                                                                            (cons a-1
                                                                                  '()))))))
                                                  (if (equal? 'wrt a-1)
                                                      (FAIL (cons v-test
                                                                  (cons a-0
                                                                        (cons a-1
                                                                              '()))))
                                                      (FAIL (cons v-test
                                                                  (cons a-0
                                                                        (cons a-1
                                                                              '()))))))
                                              (if (equal? 'wrt a-1)
                                                  (FAIL (cons v-test
                                                              (cons a-0
                                                                    (cons a-1
                                                                          '()))))
                                                  (FAIL (cons v-test
                                                              (cons a-0
                                                                    (cons a-1
                                                                          '()))))))
                                          (if (equal? 'wrt a-1)
                                              (FAIL (cons v-test
                                                          (cons a-0
                                                                (cons a-1 '()))))
                                              (FAIL (cons v-test
                                                          (cons a-0
                                                                (cons a-1 '()))))))
                                      (if (equal? 'wrt a-1)
                                          (FAIL (cons v-test
                                                      (cons a-0 (cons a-1 '()))))
                                          (FAIL (cons v-test
                                                      (cons a-0 (cons a-1 '()))))))
                                  (if (equal? 'dispatch* v-test)
                                      (if (equal? 'YES a-0)
                                          (FAIL (cons v-test
                                                      (cons a-0 (cons a-1 '()))))
                                          (if (equal? 'NO a-0)
                                              (FAIL (cons v-test
                                                          (cons a-0
                                                                (cons a-1 '()))))
                                              (FAIL (cons v-test
                                                          (cons a-0
                                                                (cons a-1 '()))))))
                                      (if (equal? 'test v-test)
                                          (if (pair? a-0)
                                              (if (equal? 'and (car a-0))
                                                  (if (pair? (cdr a-0))
                                                      (if (equal? 'wrt a-1)
                                                          (FAIL (cons v-test
                                                                      (cons a-0
                                                                            (cons a-1
                                                                                  '()))))
                                                          (FAIL (cons v-test
                                                                      (cons a-0
                                                                            (cons a-1
                                                                                  '())))))
                                                      (if (null? (cdr a-0))
                                                          (if (equal? 'wrt a-1)
                                                              (FAIL (cons v-test
                                                                          (cons a-0
                                                                                (cons a-1
                                                                                      '()))))
                                                              (FAIL (cons v-test
                                                                          (cons a-0
                                                                                (cons a-1
                                                                                      '())))))
                                                          (FAIL (cons v-test
                                                                      (cons a-0
                                                                            (cons a-1
                                                                                  '()))))))
                                                  (if (pair? (cdr a-0))
                                                      (if (null? (cdr (cdr a-0)))
                                                          (if (equal? 'wrt a-1)
                                                              (FAIL (cons v-test
                                                                          (cons a-0
                                                                                (cons a-1
                                                                                      '()))))
                                                              (FAIL (cons v-test
                                                                          (cons a-0
                                                                                (cons a-1
                                                                                      '())))))
                                                          (if (pair? (cdr (cdr a-0)))
                                                              (if (null? (cdr (cdr (cdr a-0))))
                                                                  (if (equal?
                                                                       'wrt
                                                                       a-1)
                                                                      (FAIL (cons v-test
                                                                                  (cons a-0
                                                                                        (cons a-1
                                                                                              '()))))
                                                                      (FAIL (cons v-test
                                                                                  (cons a-0
                                                                                        (cons a-1
                                                                                              '())))))
                                                                  (FAIL (cons v-test
                                                                              (cons a-0
                                                                                    (cons a-1
                                                                                          '())))))
                                                              (FAIL (cons v-test
                                                                          (cons a-0
                                                                                (cons a-1
                                                                                      '()))))))
                                                      (FAIL (cons v-test
                                                                  (cons a-0
                                                                        (cons a-1
                                                                              '()))))))
                                              (FAIL (cons v-test
                                                          (cons a-0
                                                                (cons a-1 '())))))
                                          (if (equal? 'aaand-test v-test)
                                              (if (equal? 'YES a-0)
                                                  (FAIL (cons v-test
                                                              (cons a-0
                                                                    (cons a-1
                                                                          '()))))
                                                  (if (equal? 'NO a-0)
                                                      (FAIL (cons v-test
                                                                  (cons a-0
                                                                        (cons a-1
                                                                              '()))))
                                                      (FAIL (cons v-test
                                                                  (cons a-0
                                                                        (cons a-1
                                                                              '()))))))
                                              (if (equal? 'NIL? v-test)
                                                  (if (null? a-0)
                                                      (FAIL (cons v-test
                                                                  (cons a-0
                                                                        (cons a-1
                                                                              '()))))
                                                      (FAIL (cons v-test
                                                                  (cons a-0
                                                                        (cons a-1
                                                                              '())))))
                                                  (if (equal? 'CONS? v-test)
                                                      (if (pair? a-0)
                                                          (FAIL (cons v-test
                                                                      (cons a-0
                                                                            (cons a-1
                                                                                  '()))))
                                                          (FAIL (cons v-test
                                                                      (cons a-0
                                                                            (cons a-1
                                                                                  '())))))
                                                      (if (equal? 'NUM? v-test)
                                                          (if (number? a-0)
                                                              (FAIL (cons v-test
                                                                          (cons a-0
                                                                                (cons a-1
                                                                                      '()))))
                                                              (FAIL (cons v-test
                                                                          (cons a-0
                                                                                (cons a-1
                                                                                      '())))))
                                                          (if (equal?
                                                               'SYM?
                                                               v-test)
                                                              (if (symbol? a-0)
                                                                  (FAIL (cons v-test
                                                                              (cons a-0
                                                                                    (cons a-1
                                                                                          '()))))
                                                                  (FAIL (cons v-test
                                                                              (cons a-0
                                                                                    (cons a-1
                                                                                          '())))))
                                                              (if (equal?
                                                                   'ATM?
                                                                   v-test)
                                                                  (if (pair? a-0)
                                                                      (FAIL (cons v-test
                                                                                  (cons a-0
                                                                                        (cons a-1
                                                                                              '()))))
                                                                      (FAIL (cons v-test
                                                                                  (cons a-0
                                                                                        (cons a-1
                                                                                              '())))))
                                                                  (if (equal?
                                                                       'EQ?
                                                                       v-test)
                                                                      (if (equal?
                                                                           a-0
                                                                           a-1)
                                                                          (LEAF45
                                                                           a-0)
                                                                          (LEAF46))
                                                                      (FAIL (cons v-test
                                                                                  (cons a-0
                                                                                        (cons a-1
                                                                                              '()))))))))))))))))))))))
(define (DISPATCH2 v-d v-addr*)
  (if (null? v-addr*)
      (LEAF7 v-d)
      (if (pair? v-addr*)
          (if (equal? 'A (car v-addr*))
              (if (pair? v-d)
                  (LEAF8 (car v-d) (cdr v-addr*))
                  (FAIL (cons 'extract
                              (cons v-addr* (cons 'from (cons v-d '()))))))
              (if (equal? 'D (car v-addr*))
                  (if (pair? v-d)
                      (LEAF9 (cdr v-d) (cdr v-addr*))
                      (FAIL (cons 'extract
                                  (cons v-addr* (cons 'from (cons v-d '()))))))
                  (FAIL (cons 'extract
                              (cons v-addr* (cons 'from (cons v-d '())))))))
          (FAIL (cons 'extract (cons v-addr* (cons 'from (cons v-d '()))))))))
(define (DISPATCH3 a-0 v-test)
  (if (equal? 'take v-test)
      (if (equal? 0 a-0)
          (FAIL (cons v-test (cons a-0 '())))
          (if (number? a-0)
              (FAIL (cons v-test (cons a-0 '())))
              (FAIL (cons v-test (cons a-0 '())))))
      (if (equal? 'drop v-test)
          (if (equal? 0 a-0)
              (FAIL (cons v-test (cons a-0 '())))
              (if (number? a-0)
                  (FAIL (cons v-test (cons a-0 '())))
                  (FAIL (cons v-test (cons a-0 '())))))
          (if (equal? 'lookup v-test)
              (FAIL (cons v-test (cons a-0 '())))
              (if (equal? 'extract v-test)
                  (if (null? a-0)
                      (FAIL (cons v-test (cons a-0 '())))
                      (if (pair? a-0)
                          (if (equal? 'A (car a-0))
                              (FAIL (cons v-test (cons a-0 '())))
                              (if (equal? 'D (car a-0))
                                  (FAIL (cons v-test (cons a-0 '())))
                                  (FAIL (cons v-test (cons a-0 '())))))
                          (FAIL (cons v-test (cons a-0 '())))))
                  (if (equal? 'run-program v-test)
                      (FAIL (cons v-test (cons a-0 '())))
                      (if (equal? 'step* v-test)
                          (if (pair? a-0)
                              (if (pair? (cdr a-0))
                                  (if (pair? (car (cdr a-0)))
                                      (if (pair? (cdr (cdr a-0)))
                                          (if (null? (car (cdr (cdr a-0))))
                                              (if (null? (cdr (cdr (cdr a-0))))
                                                  (FAIL (cons v-test
                                                              (cons a-0 '())))
                                                  (FAIL (cons v-test
                                                              (cons a-0 '()))))
                                              (if (null? (cdr (cdr (cdr a-0))))
                                                  (FAIL (cons v-test
                                                              (cons a-0 '())))
                                                  (FAIL (cons v-test
                                                              (cons a-0 '())))))
                                          (FAIL (cons v-test (cons a-0 '()))))
                                      (if (pair? (cdr (cdr a-0)))
                                          (if (null? (cdr (cdr (cdr a-0))))
                                              (FAIL (cons v-test
                                                          (cons a-0 '())))
                                              (FAIL (cons v-test
                                                          (cons a-0 '()))))
                                          (FAIL (cons v-test (cons a-0 '())))))
                                  (FAIL (cons v-test (cons a-0 '()))))
                              (FAIL (cons v-test (cons a-0 '()))))
                          (if (equal? 'step v-test)
                              (FAIL (cons v-test (cons a-0 '())))
                              (if (equal? 'dispatch v-test)
                                  (if (pair? a-0)
                                      (if (equal? 'IF (car a-0))
                                          (if (pair? (cdr a-0))
                                              (if (pair? (cdr (cdr a-0)))
                                                  (if (pair? (cdr (cdr (cdr a-0))))
                                                      (if (null? (cdr (cdr (cdr (cdr a-0)))))
                                                          (FAIL (cons v-test
                                                                      (cons a-0
                                                                            '())))
                                                          (FAIL (cons v-test
                                                                      (cons a-0
                                                                            '()))))
                                                      (FAIL (cons v-test
                                                                  (cons a-0
                                                                        '()))))
                                                  (FAIL (cons v-test
                                                              (cons a-0 '()))))
                                              (FAIL (cons v-test
                                                          (cons a-0 '()))))
                                          (FAIL (cons v-test (cons a-0 '()))))
                                      (FAIL (cons v-test (cons a-0 '()))))
                                  (if (equal? 'dispatch* v-test)
                                      (if (equal? 'YES a-0)
                                          (FAIL (cons v-test (cons a-0 '())))
                                          (if (equal? 'NO a-0)
                                              (FAIL (cons v-test
                                                          (cons a-0 '())))
                                              (FAIL (cons v-test
                                                          (cons a-0 '())))))
                                      (if (equal? 'test v-test)
                                          (if (pair? a-0)
                                              (if (equal? 'and (car a-0))
                                                  (if (pair? (cdr a-0))
                                                      (FAIL (cons v-test
                                                                  (cons a-0
                                                                        '())))
                                                      (if (null? (cdr a-0))
                                                          (FAIL (cons v-test
                                                                      (cons a-0
                                                                            '())))
                                                          (FAIL (cons v-test
                                                                      (cons a-0
                                                                            '())))))
                                                  (if (pair? (cdr a-0))
                                                      (if (null? (cdr (cdr a-0)))
                                                          (FAIL (cons v-test
                                                                      (cons a-0
                                                                            '())))
                                                          (if (pair? (cdr (cdr a-0)))
                                                              (if (null? (cdr (cdr (cdr a-0))))
                                                                  (FAIL (cons v-test
                                                                              (cons a-0
                                                                                    '())))
                                                                  (FAIL (cons v-test
                                                                              (cons a-0
                                                                                    '()))))
                                                              (FAIL (cons v-test
                                                                          (cons a-0
                                                                                '())))))
                                                      (FAIL (cons v-test
                                                                  (cons a-0
                                                                        '())))))
                                              (FAIL (cons v-test
                                                          (cons a-0 '()))))
                                          (if (equal? 'aaand-test v-test)
                                              (if (equal? 'YES a-0)
                                                  (FAIL (cons v-test
                                                              (cons a-0 '())))
                                                  (if (equal? 'NO a-0)
                                                      (FAIL (cons v-test
                                                                  (cons a-0
                                                                        '())))
                                                      (FAIL (cons v-test
                                                                  (cons a-0
                                                                        '())))))
                                              (if (equal? 'NIL? v-test)
                                                  (if (null? a-0)
                                                      (LEAF35)
                                                      (LEAF36))
                                                  (if (equal? 'CONS? v-test)
                                                      (if (pair? a-0)
                                                          (LEAF37)
                                                          (LEAF38))
                                                      (if (equal? 'NUM? v-test)
                                                          (if (number? a-0)
                                                              (LEAF39 a-0)
                                                              (LEAF40))
                                                          (if (equal?
                                                               'SYM?
                                                               v-test)
                                                              (if (symbol? a-0)
                                                                  (LEAF41 a-0)
                                                                  (LEAF42))
                                                              (if (equal?
                                                                   'ATM?
                                                                   v-test)
                                                                  (if (pair? a-0)
                                                                      (LEAF43)
                                                                      (LEAF44))
                                                                  (if (equal?
                                                                       'EQ?
                                                                       v-test)
                                                                      (FAIL (cons v-test
                                                                                  (cons a-0
                                                                                        '())))
                                                                      (FAIL (cons v-test
                                                                                  (cons a-0
                                                                                        '())))))))))))))))))))))
(define (DISPATCH4 v-d v-ts)
  (if (pair? v-ts)
      (LEAF29 v-d (cdr v-ts) (car v-ts))
      (if (null? v-ts)
          (LEAF30 v-d)
          (FAIL (cons 'test (cons (cons 'and v-ts) (cons 'wrt (cons v-d '()))))))))
(define (DISPATCH5 v-ts v-d a-0)
  (if (equal? 'YES a-0)
      (LEAF31 v-d v-ts)
      (if (equal? 'NO a-0)
          (LEAF32)
          (FAIL (cons 'aaand-test (cons a-0 (cons v-ts (cons v-d '()))))))))
(define (DISPATCH6 v-d v-t)
  (if (pair? v-t)
      (if (equal? 'and (car v-t))
          (if (pair? (cdr v-t))
              (LEAF29 v-d (cdr (cdr v-t)) (car (cdr v-t)))
              (if (null? (cdr v-t))
                  (LEAF30 v-d)
                  (FAIL (cons 'test (cons v-t (cons 'wrt (cons v-d '())))))))
          (if (pair? (cdr v-t))
              (if (null? (cdr (cdr v-t)))
                  (LEAF33 v-d (car (cdr v-t)) (car v-t))
                  (if (pair? (cdr (cdr v-t)))
                      (if (null? (cdr (cdr (cdr v-t))))
                          (LEAF34
                           v-d
                           (car (cdr (cdr v-t)))
                           (car (cdr v-t))
                           (car v-t))
                          (FAIL (cons 'test
                                      (cons v-t (cons 'wrt (cons v-d '()))))))
                      (FAIL (cons 'test (cons v-t (cons 'wrt (cons v-d '())))))))
              (FAIL (cons 'test (cons v-t (cons 'wrt (cons v-d '())))))))
      (FAIL (cons 'test (cons v-t (cons 'wrt (cons v-d '())))))))
(define (DISPATCH7 v-d v-fb)
  (if (pair? v-fb)
      (if (equal? 'IF (car v-fb))
          (if (pair? (cdr v-fb))
              (if (pair? (cdr (cdr v-fb)))
                  (if (pair? (cdr (cdr (cdr v-fb))))
                      (if (null? (cdr (cdr (cdr (cdr v-fb)))))
                          (LEAF25
                           v-d
                           (car (cdr (cdr (cdr v-fb))))
                           (car (cdr (cdr v-fb)))
                           (car (cdr v-fb)))
                          (LEAF26 v-fb))
                      (LEAF26 v-fb))
                  (LEAF26 v-fb))
              (LEAF26 v-fb))
          (LEAF26 v-fb))
      (LEAF26 v-fb)))
(define (DISPATCH8 v-d v-tb)
  (if (pair? v-tb)
      (if (equal? 'IF (car v-tb))
          (if (pair? (cdr v-tb))
              (if (pair? (cdr (cdr v-tb)))
                  (if (pair? (cdr (cdr (cdr v-tb))))
                      (if (null? (cdr (cdr (cdr (cdr v-tb)))))
                          (LEAF25
                           v-d
                           (car (cdr (cdr (cdr v-tb))))
                           (car (cdr (cdr v-tb)))
                           (car (cdr v-tb)))
                          (LEAF26 v-tb))
                      (LEAF26 v-tb))
                  (LEAF26 v-tb))
              (LEAF26 v-tb))
          (LEAF26 v-tb))
      (LEAF26 v-tb)))
(define (DISPATCH9 v-tb v-d v-fb a-0)
  (if (equal? 'YES a-0)
      (LEAF27 v-d v-tb)
      (if (equal? 'NO a-0)
          (LEAF28 v-d v-fb)
          (FAIL (cons 'dispatch*
                      (cons a-0
                            (cons v-tb (cons v-fb (cons 'wrt (cons v-d '()))))))))))
(define (DISPATCH10 v-d v-req)
  (if (pair? v-req)
      (if (equal? 'and (car v-req))
          (if (pair? (cdr v-req))
              (LEAF29 v-d (cdr (cdr v-req)) (car (cdr v-req)))
              (if (null? (cdr v-req))
                  (LEAF30 v-d)
                  (FAIL (cons 'test (cons v-req (cons 'wrt (cons v-d '())))))))
          (if (pair? (cdr v-req))
              (if (null? (cdr (cdr v-req)))
                  (LEAF33 v-d (car (cdr v-req)) (car v-req))
                  (if (pair? (cdr (cdr v-req)))
                      (if (null? (cdr (cdr (cdr v-req))))
                          (LEAF34
                           v-d
                           (car (cdr (cdr v-req)))
                           (car (cdr v-req))
                           (car v-req))
                          (FAIL (cons 'test
                                      (cons v-req (cons 'wrt (cons v-d '()))))))
                      (FAIL (cons 'test
                                  (cons v-req (cons 'wrt (cons v-d '())))))))
              (FAIL (cons 'test (cons v-req (cons 'wrt (cons v-d '())))))))
      (FAIL (cons 'test (cons v-req (cons 'wrt (cons v-d '())))))))
(define (DISPATCH11 v-d v-addr)
  (if (null? v-addr)
      (LEAF7 v-d)
      (if (pair? v-addr)
          (if (equal? 'A (car v-addr))
              (if (pair? v-d)
                  (LEAF8 (car v-d) (cdr v-addr))
                  (FAIL (cons 'extract
                              (cons v-addr (cons 'from (cons v-d '()))))))
              (if (equal? 'D (car v-addr))
                  (if (pair? v-d)
                      (LEAF9 (cdr v-d) (cdr v-addr))
                      (FAIL (cons 'extract
                                  (cons v-addr (cons 'from (cons v-d '()))))))
                  (FAIL (cons 'extract
                              (cons v-addr (cons 'from (cons v-d '())))))))
          (FAIL (cons 'extract (cons v-addr (cons 'from (cons v-d '()))))))))
(define (DISPATCH12 v-d v-tree)
  (if (pair? v-tree)
      (if (equal? 'IF (car v-tree))
          (if (pair? (cdr v-tree))
              (if (pair? (cdr (cdr v-tree)))
                  (if (pair? (cdr (cdr (cdr v-tree))))
                      (if (null? (cdr (cdr (cdr (cdr v-tree)))))
                          (LEAF25
                           v-d
                           (car (cdr (cdr (cdr v-tree))))
                           (car (cdr (cdr v-tree)))
                           (car (cdr v-tree)))
                          (LEAF26 v-tree))
                      (LEAF26 v-tree))
                  (LEAF26 v-tree))
              (LEAF26 v-tree))
          (LEAF26 v-tree))
      (LEAF26 v-tree)))
(define (DISPATCH13 v-prg v-id)
  (if (null? v-prg)
      (LEAF4)
      (if (pair? v-prg)
          (if (pair? (car v-prg))
              (if (equal? v-id (car (car v-prg)))
                  (if (pair? (cdr (car v-prg)))
                      (if (null? (cdr (cdr (car v-prg))))
                          (LEAF5 (car (cdr (car v-prg))) v-id)
                          (LEAF6 (cdr v-prg) v-id))
                      (LEAF6 (cdr v-prg) v-id))
                  (LEAF6 (cdr v-prg) v-id))
              (LEAF6 (cdr v-prg) v-id))
          (FAIL (cons 'lookup (cons v-id (cons 'in (cons v-prg '()))))))))
(define (DISPATCH14 v-R v-n)
  (if (equal? 0 v-n)
      (LEAF2 v-R)
      (if (number? v-n)
          (if (pair? v-R)
              (LEAF3 (cdr v-R) (car v-R) v-n)
              (FAIL (cons 'drop (cons v-n (cons 'from (cons v-R '()))))))
          (FAIL (cons 'drop (cons v-n (cons 'from (cons v-R '()))))))))
(define (DISPATCH15 v-R v-n)
  (if (equal? 0 v-n)
      (LEAF0)
      (if (number? v-n)
          (if (pair? v-R)
              (LEAF1 (cdr v-R) (car v-R) v-n)
              (FAIL (cons 'take (cons v-n (cons 'from (cons v-R '()))))))
          (FAIL (cons 'take (cons v-n (cons 'from (cons v-R '()))))))))
(define (DISPATCH16 v-prg a-0)
  (if (pair? a-0)
      (if (pair? (cdr a-0))
          (if (pair? (car (cdr a-0)))
              (if (pair? (cdr (cdr a-0)))
                  (if (null? (car (cdr (cdr a-0))))
                      (if (null? (cdr (cdr (cdr a-0))))
                          (LEAF11 (car (car (cdr a-0))))
                          (FAIL (cons 'step* (cons a-0 (cons v-prg '())))))
                      (if (null? (cdr (cdr (cdr a-0))))
                          (LEAF12
                           v-prg
                           (car (cdr (cdr a-0)))
                           (car (cdr a-0))
                           (car a-0))
                          (FAIL (cons 'step* (cons a-0 (cons v-prg '()))))))
                  (FAIL (cons 'step* (cons a-0 (cons v-prg '())))))
              (if (pair? (cdr (cdr a-0)))
                  (if (null? (cdr (cdr (cdr a-0))))
                      (LEAF12
                       v-prg
                       (car (cdr (cdr a-0)))
                       (car (cdr a-0))
                       (car a-0))
                      (FAIL (cons 'step* (cons a-0 (cons v-prg '())))))
                  (FAIL (cons 'step* (cons a-0 (cons v-prg '()))))))
          (FAIL (cons 'step* (cons a-0 (cons v-prg '())))))
      (FAIL (cons 'step* (cons a-0 (cons v-prg '()))))))
(define (DISPATCH17 v-R v-prg v-C v-D)
  (if (pair? v-C)
      (if (pair? (car v-C))
          (if (equal? 'CALL (car (car v-C)))
              (if (pair? (cdr (car v-C)))
                  (if (symbol? (car (cdr (car v-C))))
                      (if (pair? (cdr (cdr (car v-C))))
                          (if (number? (car (cdr (cdr (car v-C)))))
                              (if (null? (cdr (cdr (cdr (car v-C)))))
                                  (LEAF13
                                   v-prg
                                   (cdr v-C)
                                   (car (cdr (cdr (car v-C))))
                                   (car (cdr (car v-C)))
                                   v-R
                                   v-D)
                                  (FAIL (cons 'step
                                              (cons v-D
                                                    (cons v-R
                                                          (cons v-C
                                                                (cons v-prg
                                                                      '())))))))
                              (FAIL (cons 'step
                                          (cons v-D
                                                (cons v-R
                                                      (cons v-C
                                                            (cons v-prg '())))))))
                          (FAIL (cons 'step
                                      (cons v-D
                                            (cons v-R
                                                  (cons v-C (cons v-prg '())))))))
                      (FAIL (cons 'step
                                  (cons v-D
                                        (cons v-R (cons v-C (cons v-prg '())))))))
                  (FAIL (cons 'step
                              (cons v-D (cons v-R (cons v-C (cons v-prg '())))))))
              (if (pair? v-D)
                  (if (equal? 'RET (car (car v-C)))
                      (if (null? (cdr (car v-C)))
                          (LEAF14 (cdr v-C) v-R (cdr v-D))
                          (FAIL (cons 'step
                                      (cons v-D
                                            (cons v-R
                                                  (cons v-C (cons v-prg '())))))))
                      (if (equal? 'DISP (car (car v-C)))
                          (if (pair? (cdr (car v-C)))
                              (if (null? (cdr (cdr (car v-C))))
                                  (LEAF15
                                   (cdr v-C)
                                   (car (cdr (car v-C)))
                                   v-R
                                   (cdr v-D)
                                   (car v-D))
                                  (FAIL (cons 'step
                                              (cons v-D
                                                    (cons v-R
                                                          (cons v-C
                                                                (cons v-prg
                                                                      '())))))))
                              (FAIL (cons 'step
                                          (cons v-D
                                                (cons v-R
                                                      (cons v-C
                                                            (cons v-prg '())))))))
                          (if (equal? 'LD (car (car v-C)))
                              (if (pair? (cdr (car v-C)))
                                  (if (null? (cdr (cdr (car v-C))))
                                      (LEAF16
                                       (cdr v-C)
                                       (car (cdr (car v-C)))
                                       v-R
                                       (cdr v-D)
                                       (car v-D))
                                      (FAIL (cons 'step
                                                  (cons v-D
                                                        (cons v-R
                                                              (cons v-C
                                                                    (cons v-prg
                                                                          '())))))))
                                  (FAIL (cons 'step
                                              (cons v-D
                                                    (cons v-R
                                                          (cons v-C
                                                                (cons v-prg
                                                                      '())))))))
                              (if (pair? v-R)
                                  (if (equal? 'ST (car (car v-C)))
                                      (if (null? (cdr (car v-C)))
                                          (LEAF17
                                           (cdr v-C)
                                           (cdr v-R)
                                           (car v-R)
                                           (cdr v-D)
                                           (car v-D))
                                          (FAIL (cons 'step
                                                      (cons v-D
                                                            (cons v-R
                                                                  (cons v-C
                                                                        (cons v-prg
                                                                              '())))))))
                                      (if (pair? (cdr v-R))
                                          (if (equal? 'CONS (car (car v-C)))
                                              (if (null? (cdr (car v-C)))
                                                  (LEAF18
                                                   (cdr v-C)
                                                   (cdr (cdr v-R))
                                                   (car (cdr v-R))
                                                   (car v-R)
                                                   v-D)
                                                  (FAIL (cons 'step
                                                              (cons v-D
                                                                    (cons v-R
                                                                          (cons v-C
                                                                                (cons v-prg
                                                                                      '())))))))
                                              (if (pair? (car v-R))
                                                  (if (equal?
                                                       'CAR
                                                       (car (car v-C)))
                                                      (if (null? (cdr (car v-C)))
                                                          (LEAF19
                                                           (cdr v-C)
                                                           (cdr v-R)
                                                           (car (car v-R))
                                                           v-D)
                                                          (FAIL (cons 'step
                                                                      (cons v-D
                                                                            (cons v-R
                                                                                  (cons v-C
                                                                                        (cons v-prg
                                                                                              '())))))))
                                                      (if (equal?
                                                           'CDR
                                                           (car (car v-C)))
                                                          (if (null? (cdr (car v-C)))
                                                              (LEAF20
                                                               (cdr v-C)
                                                               (cdr v-R)
                                                               (cdr (car v-R))
                                                               v-D)
                                                              (FAIL (cons 'step
                                                                          (cons v-D
                                                                                (cons v-R
                                                                                      (cons v-C
                                                                                            (cons v-prg
                                                                                                  '())))))))
                                                          (FAIL (cons 'step
                                                                      (cons v-D
                                                                            (cons v-R
                                                                                  (cons v-C
                                                                                        (cons v-prg
                                                                                              '()))))))))
                                                  (if (number? (car v-R))
                                                      (if (number?
                                                           (car (cdr v-R)))
                                                          (if (equal?
                                                               'ADD
                                                               (car (car v-C)))
                                                              (if (null? (cdr (car v-C)))
                                                                  (LEAF21
                                                                   (cdr v-C)
                                                                   (cdr (cdr v-R))
                                                                   (car (cdr v-R))
                                                                   (car v-R)
                                                                   v-D)
                                                                  (FAIL (cons 'step
                                                                              (cons v-D
                                                                                    (cons v-R
                                                                                          (cons v-C
                                                                                                (cons v-prg
                                                                                                      '())))))))
                                                              (if (equal?
                                                                   'SUB
                                                                   (car (car v-C)))
                                                                  (if (null? (cdr (car v-C)))
                                                                      (LEAF22
                                                                       (cdr v-C)
                                                                       (cdr (cdr v-R))
                                                                       (car (cdr v-R))
                                                                       (car v-R)
                                                                       v-D)
                                                                      (FAIL (cons 'step
                                                                                  (cons v-D
                                                                                        (cons v-R
                                                                                              (cons v-C
                                                                                                    (cons v-prg
                                                                                                          '())))))))
                                                                  (if (equal?
                                                                       'MUL
                                                                       (car (car v-C)))
                                                                      (if (null? (cdr (car v-C)))
                                                                          (LEAF23
                                                                           (cdr v-C)
                                                                           (cdr (cdr v-R))
                                                                           (car (cdr v-R))
                                                                           (car v-R)
                                                                           v-D)
                                                                          (FAIL (cons 'step
                                                                                      (cons v-D
                                                                                            (cons v-R
                                                                                                  (cons v-C
                                                                                                        (cons v-prg
                                                                                                              '())))))))
                                                                      (if (equal?
                                                                           'MOD
                                                                           (car (car v-C)))
                                                                          (if (null? (cdr (car v-C)))
                                                                              (LEAF24
                                                                               (cdr v-C)
                                                                               (cdr (cdr v-R))
                                                                               (car (cdr v-R))
                                                                               (car v-R)
                                                                               v-D)
                                                                              (FAIL (cons 'step
                                                                                          (cons v-D
                                                                                                (cons v-R
                                                                                                      (cons v-C
                                                                                                            (cons v-prg
                                                                                                                  '())))))))
                                                                          (FAIL (cons 'step
                                                                                      (cons v-D
                                                                                            (cons v-R
                                                                                                  (cons v-C
                                                                                                        (cons v-prg
                                                                                                              '()))))))))))
                                                          (FAIL (cons 'step
                                                                      (cons v-D
                                                                            (cons v-R
                                                                                  (cons v-C
                                                                                        (cons v-prg
                                                                                              '())))))))
                                                      (FAIL (cons 'step
                                                                  (cons v-D
                                                                        (cons v-R
                                                                              (cons v-C
                                                                                    (cons v-prg
                                                                                          '())))))))))
                                          (if (pair? (car v-R))
                                              (if (equal? 'CAR (car (car v-C)))
                                                  (if (null? (cdr (car v-C)))
                                                      (LEAF19
                                                       (cdr v-C)
                                                       (cdr v-R)
                                                       (car (car v-R))
                                                       v-D)
                                                      (FAIL (cons 'step
                                                                  (cons v-D
                                                                        (cons v-R
                                                                              (cons v-C
                                                                                    (cons v-prg
                                                                                          '())))))))
                                                  (if (equal?
                                                       'CDR
                                                       (car (car v-C)))
                                                      (if (null? (cdr (car v-C)))
                                                          (LEAF20
                                                           (cdr v-C)
                                                           (cdr v-R)
                                                           (cdr (car v-R))
                                                           v-D)
                                                          (FAIL (cons 'step
                                                                      (cons v-D
                                                                            (cons v-R
                                                                                  (cons v-C
                                                                                        (cons v-prg
                                                                                              '())))))))
                                                      (FAIL (cons 'step
                                                                  (cons v-D
                                                                        (cons v-R
                                                                              (cons v-C
                                                                                    (cons v-prg
                                                                                          '()))))))))
                                              (FAIL (cons 'step
                                                          (cons v-D
                                                                (cons v-R
                                                                      (cons v-C
                                                                            (cons v-prg
                                                                                  '())))))))))
                                  (FAIL (cons 'step
                                              (cons v-D
                                                    (cons v-R
                                                          (cons v-C
                                                                (cons v-prg
                                                                      '()))))))))))
                  (if (pair? v-R)
                      (if (pair? (cdr v-R))
                          (if (equal? 'CONS (car (car v-C)))
                              (if (null? (cdr (car v-C)))
                                  (LEAF18
                                   (cdr v-C)
                                   (cdr (cdr v-R))
                                   (car (cdr v-R))
                                   (car v-R)
                                   v-D)
                                  (FAIL (cons 'step
                                              (cons v-D
                                                    (cons v-R
                                                          (cons v-C
                                                                (cons v-prg
                                                                      '())))))))
                              (if (pair? (car v-R))
                                  (if (equal? 'CAR (car (car v-C)))
                                      (if (null? (cdr (car v-C)))
                                          (LEAF19
                                           (cdr v-C)
                                           (cdr v-R)
                                           (car (car v-R))
                                           v-D)
                                          (FAIL (cons 'step
                                                      (cons v-D
                                                            (cons v-R
                                                                  (cons v-C
                                                                        (cons v-prg
                                                                              '())))))))
                                      (if (equal? 'CDR (car (car v-C)))
                                          (if (null? (cdr (car v-C)))
                                              (LEAF20
                                               (cdr v-C)
                                               (cdr v-R)
                                               (cdr (car v-R))
                                               v-D)
                                              (FAIL (cons 'step
                                                          (cons v-D
                                                                (cons v-R
                                                                      (cons v-C
                                                                            (cons v-prg
                                                                                  '())))))))
                                          (FAIL (cons 'step
                                                      (cons v-D
                                                            (cons v-R
                                                                  (cons v-C
                                                                        (cons v-prg
                                                                              '()))))))))
                                  (if (number? (car v-R))
                                      (if (number? (car (cdr v-R)))
                                          (if (equal? 'ADD (car (car v-C)))
                                              (if (null? (cdr (car v-C)))
                                                  (LEAF21
                                                   (cdr v-C)
                                                   (cdr (cdr v-R))
                                                   (car (cdr v-R))
                                                   (car v-R)
                                                   v-D)
                                                  (FAIL (cons 'step
                                                              (cons v-D
                                                                    (cons v-R
                                                                          (cons v-C
                                                                                (cons v-prg
                                                                                      '())))))))
                                              (if (equal? 'SUB (car (car v-C)))
                                                  (if (null? (cdr (car v-C)))
                                                      (LEAF22
                                                       (cdr v-C)
                                                       (cdr (cdr v-R))
                                                       (car (cdr v-R))
                                                       (car v-R)
                                                       v-D)
                                                      (FAIL (cons 'step
                                                                  (cons v-D
                                                                        (cons v-R
                                                                              (cons v-C
                                                                                    (cons v-prg
                                                                                          '())))))))
                                                  (if (equal?
                                                       'MUL
                                                       (car (car v-C)))
                                                      (if (null? (cdr (car v-C)))
                                                          (LEAF23
                                                           (cdr v-C)
                                                           (cdr (cdr v-R))
                                                           (car (cdr v-R))
                                                           (car v-R)
                                                           v-D)
                                                          (FAIL (cons 'step
                                                                      (cons v-D
                                                                            (cons v-R
                                                                                  (cons v-C
                                                                                        (cons v-prg
                                                                                              '())))))))
                                                      (if (equal?
                                                           'MOD
                                                           (car (car v-C)))
                                                          (if (null? (cdr (car v-C)))
                                                              (LEAF24
                                                               (cdr v-C)
                                                               (cdr (cdr v-R))
                                                               (car (cdr v-R))
                                                               (car v-R)
                                                               v-D)
                                                              (FAIL (cons 'step
                                                                          (cons v-D
                                                                                (cons v-R
                                                                                      (cons v-C
                                                                                            (cons v-prg
                                                                                                  '())))))))
                                                          (FAIL (cons 'step
                                                                      (cons v-D
                                                                            (cons v-R
                                                                                  (cons v-C
                                                                                        (cons v-prg
                                                                                              '()))))))))))
                                          (FAIL (cons 'step
                                                      (cons v-D
                                                            (cons v-R
                                                                  (cons v-C
                                                                        (cons v-prg
                                                                              '())))))))
                                      (FAIL (cons 'step
                                                  (cons v-D
                                                        (cons v-R
                                                              (cons v-C
                                                                    (cons v-prg
                                                                          '())))))))))
                          (if (pair? (car v-R))
                              (if (equal? 'CAR (car (car v-C)))
                                  (if (null? (cdr (car v-C)))
                                      (LEAF19
                                       (cdr v-C)
                                       (cdr v-R)
                                       (car (car v-R))
                                       v-D)
                                      (FAIL (cons 'step
                                                  (cons v-D
                                                        (cons v-R
                                                              (cons v-C
                                                                    (cons v-prg
                                                                          '())))))))
                                  (if (equal? 'CDR (car (car v-C)))
                                      (if (null? (cdr (car v-C)))
                                          (LEAF20
                                           (cdr v-C)
                                           (cdr v-R)
                                           (cdr (car v-R))
                                           v-D)
                                          (FAIL (cons 'step
                                                      (cons v-D
                                                            (cons v-R
                                                                  (cons v-C
                                                                        (cons v-prg
                                                                              '())))))))
                                      (FAIL (cons 'step
                                                  (cons v-D
                                                        (cons v-R
                                                              (cons v-C
                                                                    (cons v-prg
                                                                          '()))))))))
                              (FAIL (cons 'step
                                          (cons v-D
                                                (cons v-R
                                                      (cons v-C
                                                            (cons v-prg '()))))))))
                      (FAIL (cons 'step
                                  (cons v-D
                                        (cons v-R (cons v-C (cons v-prg '())))))))))
          (FAIL (cons 'step (cons v-D (cons v-R (cons v-C (cons v-prg '())))))))
      (FAIL (cons 'step (cons v-D (cons v-R (cons v-C (cons v-prg '()))))))))
(define (DISPATCH18 v-program v-expr)
  (LEAF12
   v-program
   (cons (cons 'CALL (cons '0 (cons '1 '()))) '())
   (cons v-expr '())
   '()))
(define (DISPATCH19 v-e v-addr)
  (if (null? v-addr)
      (LEAF7 v-e)
      (if (pair? v-addr)
          (if (equal? 'A (car v-addr))
              (if (pair? v-e)
                  (LEAF8 (car v-e) (cdr v-addr))
                  (FAIL (cons 'extract
                              (cons v-addr (cons 'from (cons v-e '()))))))
              (if (equal? 'D (car v-addr))
                  (if (pair? v-e)
                      (LEAF9 (cdr v-e) (cdr v-addr))
                      (FAIL (cons 'extract
                                  (cons v-addr (cons 'from (cons v-e '()))))))
                  (FAIL (cons 'extract
                              (cons v-addr (cons 'from (cons v-e '())))))))
          (FAIL (cons 'extract (cons v-addr (cons 'from (cons v-e '()))))))))
(define (DISPATCH20 v-alist v-key)
  (if (null? v-alist)
      (LEAF4)
      (if (pair? v-alist)
          (if (pair? (car v-alist))
              (if (equal? v-key (car (car v-alist)))
                  (if (pair? (cdr (car v-alist)))
                      (if (null? (cdr (cdr (car v-alist))))
                          (LEAF5 (car (cdr (car v-alist))) v-key)
                          (LEAF6 (cdr v-alist) v-key))
                      (LEAF6 (cdr v-alist) v-key))
                  (LEAF6 (cdr v-alist) v-key))
              (LEAF6 (cdr v-alist) v-key))
          (FAIL (cons 'lookup (cons v-key (cons 'in (cons v-alist '()))))))))
(define (DISPATCH21 v-xs v-n)
  (if (equal? 0 (- v-n 1))
      (LEAF2 v-xs)
      (if (number? (- v-n 1))
          (if (pair? v-xs)
              (LEAF3 (cdr v-xs) (car v-xs) (- v-n 1))
              (FAIL (cons 'drop (cons (- v-n 1) (cons 'from (cons v-xs '()))))))
          (FAIL (cons 'drop (cons (- v-n 1) (cons 'from (cons v-xs '()))))))))
(define (DISPATCH22 v-xs v-n)
  (if (equal? 0 (- v-n 1))
      (LEAF0)
      (if (number? (- v-n 1))
          (if (pair? v-xs)
              (LEAF1 (cdr v-xs) (car v-xs) (- v-n 1))
              (FAIL (cons 'take (cons (- v-n 1) (cons 'from (cons v-xs '()))))))
          (FAIL (cons 'take (cons (- v-n 1) (cons 'from (cons v-xs '()))))))))
(define (FAIL a) (display `(NO MATCH FOUND FOR ,a)) (newline) (error "halt"))
(begin (write (DISPATCH0 (read))) (newline))

