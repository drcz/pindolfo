(use-modules (grand scheme))
(add-to-load-path "./")
(define compiled (@ (pindolf compiler) compiled))
(define run (@ (pindolf vm) run))

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

(define (inverted-lookup val #;in binding)
  (match binding
    (() #f)
    (((k . expr) . binding*) (if (equal? val expr) k
                              #;otherwise (inverted-lookup val binding*)))))

(e.g. (inverted-lookup 23 '((a . 23) (b . 'elo))) ===> a)
(e.g. (inverted-lookup '(+ x y) '((a . 23) (b . (+ x y)))) ===> b)
(e.g. (inverted-lookup '(y o l o) '((a . 23) (b . 'elo))) ===> #f)


(define (swap-vals old #;for new #;in binding)
  (match binding
    (() `())
    (((k . v) . binding*)
     `((,k . ,(if (equal? old v) new v)) . ,(swap-vals old new binding*)))))

(e.g. (swap-vals 23 42 '((a . 16) (b . 23) (c . 42) (d . 23)))
      ===> ((a . 16) (b . 42) (c . 42) (d . 42)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define (structural-pred? p) (member? p '(NIL? SYM? NUM? ATM? CONS?)))

(define (all-addresses #;in meta)
  (define (addrs-in test)
    (match test
      (('EQ? (? address? a) (? address? a*)) `(,a ,a*))
      (('EQ? (? address? a) _) `(,a))
      (((? structural-pred?) (? address? a)) `(,a))
      (('not test*) (addrs-in test*))))
  (apply union (map addrs-in meta)))

(e.g. (all-addresses '((not (EQ? (CAR *VIEW*) (CDR *VIEW*)))
                       (CONS? *VIEW*)
                       (not (SYM? (CAR (CDR *VIEW*))))))
      ===> ((CAR (CDR *VIEW*))
            *VIEW*
            (CAR *VIEW*)
            (CDR *VIEW*)))

(define (concerning? test address)
  (match test
    (('EQ? _ (? (is _ equal? address))) #t) ;; sure??
    (('EQ? (? (is _ equal? address)) _) #t)
    (((? structural-pred?) (? (is _ equal? address))) #t)
    (('not test*) (concerning? test* address))
    (_ #f)))

(define (address-equality? test)
  (match test
    (('EQ? (? address?) (? address?)) #t)
    (_ #f)))

(define (address-inequality? test)
  (match test
    ((('not 'EQ? (? address?) (? address?))) #t)
    (_ #f)))

;; for each address find all the ones it's equal to (excluding itself)
(define (equivalences #;in meta)
  (let ((meta* (filter (is _ address-equality?) meta)))
    (let feed ((pend meta*)
               (eqs (map (lambda (a) `(,a . ())) (all-addresses #;in meta)))
               (changed? #f))
      (match pend
        (() (if changed?
                (feed meta* eqs #f)
                (map (lambda ((a . as)) `(,a . ,(delete a as))) eqs)))
        ((('EQ? a a*) . pend*) (let* ((a-pre (lookup a eqs))
                                      (a*-pre (lookup a* eqs))
                                      (a-post (union a-pre `(,a* . ,a*-pre)))
                                      (a*-post (union a*-pre `(,a . ,a-pre)))
                                      (changed?* (or changed?
                                                     (< (length a-pre)
                                                        (length a-post))
                                                     (< (length a*-pre)
                                                        (length a*-post))))
                                      (eqs1 (update a a-post eqs))
                                      (eqs2 (update a* a*-post eqs1)))
                                 (feed pend* eqs2 changed?*)))))))

(e.g.
 (let ((a0 '(CDR *VIEW*))
       (a1 '(CAR *VIEW*))
       (a2 '(CAR (CDR *VIEW*)))
       (a3 '(CDR (CDR *VIEW*))))
   (equal? 
    (equivalences `((EQ? ,a1 ,a2)
                    (EQ? ,a2 ,a3)
                    (not (NIL? ,a0))
                    (CONS? ,a2)
                    (EQ? ,a2 23)))
    ; addr . equal to
    `((,a0 . ())
      (,a3 . (,a2 ,a1))
      (,a1 . (,a3 ,a2))
      (,a2 . (,a3 ,a1))))))

;;; omg przecież nierówności się propagują z równości c'nie
;;; czyli jak a0!=a1 i a1==a2 to a0!=a2 c'nie.
(define (inequivalences #;in meta)
  (let ((equivalences 
        (meta* (map cadr (filter (is _ address-inequality?) meta))))
    (let feed ((pend meta*)
               (eqs (map (lambda (a) `(,a . ())) (all-addresses #;in meta)))
               (changed? #f))
      (match pend
        (() (if changed?
                (feed meta* eqs #f)
                eqs #;(map (lambda ((a . as)) `(,a . ,(delete a as))) eqs)))
        ((('EQ? a a*) . pend*) (let* ((a-pre (lookup a eqs))
                                      (a*-pre (lookup a* eqs))
                                      (a-post (union a-pre `(,a* . ,a*-pre)))
                                      (a*-post (union a*-pre `(,a . ,a-pre)))
                                      (changed?* (or changed?
                                                     (< (length a-pre)
                                                        (length a-post))
                                                     (< (length a*-pre)
                                                        (length a*-post))))
                                      (eqs1 (update a a-post eqs))
                                      (eqs2 (update a* a*-post eqs1)))
                                 (feed pend* eqs2 changed?*))))))))

(e.g.
 (let ((a0 '(CDR *VIEW*))
       (a1 '(CAR *VIEW*))
       (a2 '(CAR (CDR *VIEW*)))
       (a3 '(CDR (CDR *VIEW*))))
   (equal? 
    (inequivalences `((not (EQ? ,a1 ,a2))
                      (EQ? ,a2 ,a3)
                    (not (NIL? ,a0))
                    (CONS? ,a2)
                    (EQ? ,a2 23)))
    ; addr . equal to
    `((,a0 . ())
      (,a3 . (,a2 ,a1))
      (,a1 . (,a3 ,a2))
      (,a2 . (,a3 ,a1))))))



(define (desc<-test t)
  (match t
    #;(('EQ? (? address?) (? address? a)) `(EQ? ,a)) ;; nope...
    (('EQ? (? address?) c) `(EQ? ,c))
    (((? structural-pred? p) (? address?)) `(,p))
    (('not t*) `(not ,(desc<-test t*)))))


(define (grouped-by-address meta)
  (map (lambda (addr) `(,addr . ,(filter (is _ concerning? addr) meta)))
       (all-addresses #;in meta)))

(e.g.
 (let ((a1 '(CAR *VIEW*))
       (a2 '(CAR (CDR *VIEW*))))
   (grouped-by-address `((EQ? ,a1 ,a2) (NIL? ,a1) (CONS? ,a2) (EQ? ,a2 23))))
 ===> (((CAR *VIEW*) . ((EQ? (CAR *VIEW*) (CAR (CDR *VIEW*)))
                        (NIL? (CAR *VIEW*))))
       ((CAR (CDR *VIEW*)) . ((EQ? (CAR *VIEW*) (CAR (CDR *VIEW*)))
                              (CONS? (CAR (CDR *VIEW*)))
                              (EQ? (CAR (CDR *VIEW*)) 23)))))

(define (consistent? meta)
    ...)
         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (next-number pp2num)
  (if (null? pp2num)
      0
      (let* ((all-nums (map cdr pp2num))
             (biggest (apply max all-nums)))
        (+ biggest 1))))

(define (check/arrange pp pp2num)
  (match (lookup pp pp2num)
    (#f (let* ((n (next-number pp2num))
               (pp2num* `((,pp . ,n) . ,pp2num)))
          `(,n ,pp2num* NOVUM)))
    ((? number? n) `(,n ,pp2num NIHIL-NOVI))))


(define (ppoint lbl meta) ;;; we'll insert LVA/LTA here, no?
  `(,lbl ,meta))

(define (analysis test meta)
  (let* ((consistent+? (consistent? `(,test ,@meta)))
         (consistent-? (consistent? `((not ,test) ,@meta))))
    (cond ((and consistent+? consistent-?) 'BOTH)
          (consistent+? 'PASSED)
          (consistent-? 'FAILED)
          (else 'IMPOSSIBLE))))

#;(define (extended-code n block code pp2num)
  `(,n ((,n . ,block) . ,code) ,pp2num))
(define (extended-code n block code pp2num)
  (match (inverted-lookup block code)
    (#f `(,n ((,n . ,block) . ,code) ,pp2num))
    (n0 (let* ((pp2num* (swap-vals n #;for n0 #;in pp2num)))
          `(,n0 ,code ,pp2num*)))))

(define (drive-step pp code pp2num src)
  (match (check/arrange pp pp2num)
    ((n _ 'NIHIL-NOVI) ;; it's already computed...
     `(,n ,code ,pp2num))
    ((n pp2num 'NOVUM) ;; driving time!
     (let* (((lbl meta) pp)
            (block (lookup lbl src)))
       (match block
         ((('IF test lbl-t 'ELSE lbl-e))
          (match (analysis test meta)
            ('BOTH
             (let* ((pp0 (ppoint lbl-t `(,test . ,meta)))
                    (pp1 (ppoint lbl-e `((not ,test) . ,meta)))
                    ((n0 code0 pp2num0) (drive-step pp0 code pp2num src))
                    ((n1 code1 pp2num1) (drive-step pp1 code0 pp2num0 src))
                    (block* (if (eq? n0 n1)
                                `((GOTO ,n0))
                                `((IF ,test ,n0 ELSE ,n1)))))
               (extended-code n block* code1 pp2num1)))
            ('PASSED 
             (drive-step (ppoint lbl-t `(,test . ,meta)) code pp2num src))
            ('FAILED 
             (drive-step (ppoint lbl-e `((not ,test) . ,meta)) code pp2num src))))
         ((('GOTO lbl*)) ; we don't jump over, it's there for a reason (CPS or sth)
          (let* ((pp (ppoint lbl* meta))
                 ((n0 code0 pp2num0) (drive-step pp code pp2num src))
                 (block* `((GOTO ,n0))))
            (extended-code n block* code0 pp2num0)))
         (_ 
          (extended-code n (massaged-block block pp2num) code pp2num)))))))

(define (massaged-block block pp2num)
  (match block ;;; if check/arrange didn't find it, perhaps i should compute it?
    ((('GOTO l)) `((GOTO ,(car (check/arrange (ppoint l '()) pp2num)))))
    ((('RETURN e)) block)
    ((('LET v ('CALL l '*VIEW*)) . block*)
     `((LET ,v (CALL ,(car (check/arrange (ppoint l '()) pp2num)) *VIEW*)) ;; TODO!
       . ,(massaged-block block* pp2num)))
    ((('LET v e) . block*)
     `((LET ,v ,e)
       . ,(massaged-block block* pp2num)))))


(define (optimized src)
  (let* ((init-lbl (caar src)) ; he_he
         (init-meta '())
         (init-pp2num '())
         (init-code '())
         ((lbl tgt pp2num) (drive-step (ppoint init-lbl init-meta)
                                       init-code
                                       init-pp2num
                                       src)))
    tgt))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define test1
  (compiled '( (('APD    ()      ?ys) ys)
               (('APD (?x . ?xs) ?ys) `(,x . ,(& (APD ,xs ,ys))))
               )))

;(pretty-print (optimized test1))
;(pretty-print [run (optimized test1) '(APD (q w e) (a s d))])

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


(define test2 (compiled p3))
(pretty-print (optimized test2)) ;;; 11min33sec
;;; but it works correctly and the amount of stuff is most likely due to
;;; either lack of proper analysis (vide workbench.scm monster) or lack of LVA
;;; ...on it!
