(use-modules (grand scheme))

(define (lookup key #;in binding)
  (match binding
    (() #f)
    (((k . expr) . binding*) (if (equal? k key) expr
                              #;otherwise (lookup key binding*)))))

(define var? symbol?)
(define (vartype? x) (member? x '(num sym atm exp)))

;;; the convention is req := (addr . cnd), reqs := [req]
;;; example req: ((CAR (CDR *)) EQ? 'apd)
;;; its addr is (CAR (CDR *)) and its cnd is (EQ? 'apd).

(define (reqs.bindings #;in pattern)
  (let loop ((pend `((,pattern *)))
             (reqs '())
             (binding '()))
    (match pend
      (() `(,reqs . ,binding))
      ((('_ cur) . pend*) (loop pend* reqs binding))
      (((() cur) . pend*)
       (loop pend* `(,@reqs (,cur NIL?)) binding))
      ((((? number? n) cur) . pend*)
       (loop pend* `(,@reqs (,cur EQ? ,n)) binding))
      (((('quote e) cur) . pend*)
       (loop pend* `(,@reqs (,cur EQ? ',e)) binding))
      (((((? vartype? T) (? symbol? v)) cur) . pend*)
         (match (lookup v binding)
           (#f (let* ((binding* `((,v . ,cur) . ,binding))
                      (req (match T
                             ('num `(,cur NUM?))
                             ('sym `(,cur SYM?))
                             ('atm `(,cur ATM?))
                             ('exp #f)))
                      (reqs* (if req `(,@reqs ,req) reqs)))
                 (loop pend* reqs* binding*)))
           (v* (loop pend* `(,@reqs (,cur EQ? ,v*)) binding))))
      ((((p . ps) cur) . pend*)
       (let* ((pend** `((,p (CAR ,cur)) (,ps (CDR ,cur)) . ,pend*))
              (reqs* `(,@reqs (,cur CONS?))))
         (loop pend** reqs* binding))))))

(define (cnd-concerning addr #;in reqs)
  (match reqs
    (() #f)
    (((a . cnd) . reqs*) (if (equal? a addr)
                             cnd ;; there's gonna be at most one.
                             (cnd-concerning addr reqs*)))))

(define (stronger? cnd0 #;than cnd1)
  (match `(,cnd0 ,cnd1) ;; sorry have to mimick PINDOLFO form
    ((('EQ? (? number?)) ('NUM?)) #t)
    ((('EQ? ('quote (? symbol?))) ('SYM?)) #t)
    ((('EQ? _) ('ATM?)) #t)
    ((('NUM?) ('ATM?)) #t)
    ((('SYM?) ('ATM?)) #t)
    ((('NIL?) ('ATM?)) #t)
    (_ #f)))

(define (weaker? cnd0 #;than cnd1) (stronger? cnd1 cnd0))

;;; we don't use it but it helps grasping the idea (prolly?)
(define (consistent? cnd0 #;with cnd1)
  (or (equal? cnd0 cnd1)
      (stronger? cnd0 cnd1)
      (weaker? cnd0 cnd1)))

(define (sat-reqs #;for req #;from reqs)
  (let* (((addr . cnd) req))
    (match (cnd-concerning addr reqs)
      (#f reqs) ; neutral about addr, leave as they are
      (c0 (cond ((or (equal? c0 cnd) (weaker? c0 cnd))
                 ;;; it's not stronger than req so already sat:
                 (filter (lambda (c) (not (equal? c `(,addr . ,c0)))) reqs))
                ((stronger? c0 cnd) ;; stronger must still be checked
                 reqs)
                ;;; otherwise it's NOT consistent:
                (else #f))))))

(define (unsat-reqs #;for req #;from reqs)
  (let* (((addr . cnd) req))
    (match (cnd-concerning addr reqs)
      (#f reqs) ; neutral about addr, leave as they are
      (c0 (if (or (equal? c0 cnd) (stronger? c0 cnd))
              #f ;; it's as strong as req so can't be satisfied, drop!
              reqs))))) ;; otherwise it's not consister or weaker so ok!

(define (clauses->rM program)
  (match program
    (() '())
    (((pattern leaf) . program*)
     (let* (((reqs . binding) (reqs.bindings pattern))
            (leaf* `(BINDING ,binding BODY ,leaf)))
       `((,@reqs ,leaf*) . ,(clauses->rM program*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (qq->cons qq)
  (match qq
    (('unquote e) e) ;; assert (var? e) NIE?!
    ((h . t) `(CONS ,(qq->cons h) ,(qq->cons t)))
    (() '())
    ((? number?) qq)
    ((? symbol?) `(quote ,qq))))

;; lol btw (define (qq->cons qq) (cons<-quasiquote qq (lambda (x) x))) no?!

(e.g. (qq->cons '(REV ,xs (,x . ,rs)))
      ===> (CONS 'REV (CONS xs (CONS (CONS x rs) ()))))
;;; from now on we'll be using expr/mexpr in CONSized form.

(define (expr-for addr mexpr)
  (match addr
    ('* mexpr)
    (('CAR addr*) (match (expr-for addr* mexpr)
                    (('CONS mexpr* _) mexpr*)
                    (mexpr* `(CAR ,mexpr*))))
    (('CDR addr*) (match (expr-for addr* mexpr)
                    (('CONS _ mexpr*) mexpr*)
                    (mexpr* `(CDR ,mexpr*))))))
 
(e.g. (expr-for '(CAR *) '(CONS x 'elo)) ===> x)
(e.g. (expr-for '(CDR *) '(CONS x 'elo)) ===> 'elo)
(e.g. (expr-for '* '(CONS x 'elo)) ===> (CONS x 'elo))
(e.g. (expr-for '(CAR (CAR *)) '(CONS (CONS 'elo 'yolo) y)) ===> 'elo)
(e.g. (expr-for '(CDR (CAR *)) '(CONS (CONS 'elo 'yolo) y)) ===> 'yolo)
(e.g. (expr-for '(CDR *) '(CONS (CONS 'elo 'yolo) y)) ===> y)
(e.g. (expr-for '(CAR (CDR *)) '(CONS (CONS 'elo 'yolo) y)) ===> (CAR y))

(define (ground? expr)
  (match expr
    ((? var?) #f)
    (() #t)
    ((? number?) #t)
    (('quote _) #t)
    (('CAR e) (ground? e)) ;; as of now it should always be #f, no?
    (('CDR e) (ground? e)) ;; - || -
    (('CONS h t) (and (ground? h) (ground? t)))))

(define (reduce-req req mexpr)
  (let* (((addr . cnd) req)
         (addr/cnstr (expr-for addr mexpr)))
    (if (not (ground? addr/cnstr))
        (match `(,cnd ,addr/cnstr)
          ((('CONS?) ('CONS _ _)) #t) ;; is that even possible?
          (_ `(,addr/cnstr . ,cnd))) ;; can't decide now!
        (match `(,cnd ,addr/cnstr)
          ((('CONS?) ('CONS _ _)) #t)
          ((('NIL?) ()) #t)
          ((('NUM?) (? number?)) #t)
          ((('SYM?) ('quote _)) #t)
          ((('ATM?) ('CONS _ _)) #f)
          ((('ATM?) _) #t)
          ((('EQ? e) e) #t) ;;; sure?? hmmm it's gnd so prolly yeah
          (_ #f)))))

;;; THIS!!!
(define (mk-tree rM mexpr) ;;; rM -- ,,requests matrix'' [[req]++leaf]
  (let loop ((rM rM))
    (match rM
      (()
       'FAIL!)
      (((('BINDING bnd 'BODY bd)) . tM*)
       (let* ((bnd* (map (lambda ((v . a))
                           `(,v . ,(expr-for a mexpr))) bnd)))
         `(LEAF (BINDING ,bnd* BODY ,bd))))
      (((r0 . reqs) . _)
       (match (reduce-req r0 mexpr)
         (#t (loop (filter-map (lambda (rs) (sat-reqs r0 rs)) rM)))
         (#f (loop (filter-map (lambda (rs) (unsat-reqs r0 rs)) rM)))
         ((adr . cnd)
          (let* ((sat-rM (filter-map (lambda (rs) (sat-reqs r0 rs)) rM))
                 (unsat-rM (filter-map (lambda (rs) (unsat-reqs r0 rs)) rM))
                 (test `(,@cnd ,adr))
                 (then-t (loop sat-rM))
                 (else-t (loop unsat-rM)))
            `(IF ,test ,then-t ELSE ,else-t))))))))
;;; /THIS


(define p1
  '( (('APD () (exp ys)) ys)
     (('APD ((exp x) . (exp xs)) (exp ys)) `(,x . ,(& (APD ,xs ,ys))))

     (('MUL 0 _) 0)
     (('MUL 1 (num x)) x)
     (('MUL (num x) (num y)) (+ y (& (MUL ,(- x 1) ,y))))

     (('REV (exp xs)) (& (REV ,xs ())))
     (('REV () (exp rs)) rs)
     (('REV ((exp x) . (exp xs)) (exp rs)) (& (REV ,xs (,x . ,rs))))
     ))

(e.g. (mk-tree (clauses->rM p1) (qq->cons '(MUL 3 ,m))) ===>
      (IF (NUM? m)
          (LEAF (BINDING ((y . m)
                          (x . 3))
                    BODY (+ y (& (MUL ,(- x 1) ,y)))))
          ELSE FAIL!))

(e.g. (mk-tree (clauses->rM p1) (qq->cons '(MUL ,n ,2))) ===>
      (IF (EQ? 0 n)
          (LEAF (BINDING ()
                 BODY 0))
          ELSE (IF (EQ? 1 n)
                   (LEAF (BINDING ((x . 2))
                          BODY x))
                   ELSE (IF (NUM? n)
                            (LEAF (BINDING ((y . 2) (x . n))
                                   BODY (+ y (& (MUL ,(- x 1) ,y)))))
                            ELSE FAIL!))))

(e.g. (mk-tree (clauses->rM p1) (qq->cons '(APD () ,bs))) ===>
      (LEAF (BINDING ((ys . bs)) BODY ys)))

(e.g. (mk-tree (clauses->rM p1) (qq->cons '(APD (q w e) ,bs))) ===>
      (LEAF (BINDING ((ys . bs)
                      (xs . (CONS 'w (CONS 'e ())))
                      (x . 'q))
                BODY  `(,x . ,(& (APD ,xs ,ys))))))

(e.g. (mk-tree (clauses->rM p1) (qq->cons '(APD ,as ,bs))) ===>
      (IF (NIL? as)
          (LEAF (BINDING ((ys . bs)) BODY ys))
          ELSE
          (IF (CONS? as)
              (LEAF (BINDING ((ys . bs) (xs . (CDR as)) (x . (CAR as)))
                        BODY `(,x . ,(& (APD ,xs ,ys)))))
              ELSE FAIL!)))

;;; sooo this might actually work!!!! (tbc)
