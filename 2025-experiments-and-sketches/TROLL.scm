(use-modules (grand scheme))

(define (lookup key #;in binding)
  (match binding
    (() #f)
    (((k . expr) . binding*) (if (equal? k key) expr
                              #;otherwise (lookup key binding*)))))

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

;;; THIS IS THE THING:
(define (mk-tree rM) ;;; rM -- ,,requests matrix'' [[req]++leaf]
  (match rM
    (() 'FAIL!)
    (((leaf) . tM*) `(LEAF ,leaf))
    (((r0 . reqs) . tM*)
     (let* ((sat-rM (filter-map (lambda (rs) (sat-reqs r0 rs)) rM))
            (unsat-rM (filter-map (lambda (rs) (unsat-reqs r0 rs)) rM))
            ((addr . cnd) r0)
            (test `(,@cnd ,addr))
            (then-t (mk-tree sat-rM))
            (else-t (mk-tree unsat-rM)))
       `(IF ,test ,then-t ELSE ,else-t)))))
;;; /THIS


;;;;;;;;;;;;;;;;;;;; AAAAAAAAAAAAAAAAAAA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; temporary thingie for testing on the old vmachine:

(define (applications-in expression)
  (define (applications-in-qq qq)
    (match qq
      (('unquote e) (applications-in e))
      ((e . e*) (append (applications-in-qq e) (applications-in-qq e*)))
      (_ '())))
  (match expression
    (() '())
    ('T '())
    ((? symbol?) '())
    ((? number?) '())
    (('quote _) '())
    (('quasiquote qq) (applications-in-qq qq))
    (('+ e e*) (append (applications-in e) (applications-in e*)))
    (('- e e*) (append (applications-in e) (applications-in e*)))
    (('* e e*) (append (applications-in e) (applications-in e*)))
    (('& e) (append (applications-in-qq e) `(,expression)))))

(define (application->vars-map #;for expression)
  (let ((applications (applications-in expression)))
    (map (lambda (app index) `(,app . (V ,index)))
         applications (iota (length applications)))))

(define (cons<-quasiquote qq #;for-subexpressions-using comp-sub)
  (match qq
    (() '())
    (('unquote e) (comp-sub e))
    ((q . q*) `(CONS ,(cons<-quasiquote q comp-sub)
                     ,(cons<-quasiquote q* comp-sub)))
    (e `(quote ,e))))

(define (compiled-subexpression expr #;wrt vars-for-apps)
  (let cmpld ((expr expr))
    (match expr
      (() '())
      ('T 'T)
      ((? symbol? sym) sym)
      ((? number? num) num)
      (('quote e) expr)
      (('quasiquote qq) (cons<-quasiquote qq cmpld))
      (('+ e e*) `(+ ,(cmpld e) ,(cmpld e*)))
      (('- e e*) `(- ,(cmpld e) ,(cmpld e*)))
      (('* e e*) `(* ,(cmpld e) ,(cmpld e*)))
      (('& e) (lookup expr vars-for-apps))
      #;err??)))

(define (compiled-expression expression)
  (let* ((vars4apps (application->vars-map expression))
         (apps-code
          (map (lambda (((& e) . var))
                 `((LET *VIEW*
                        ,(compiled-subexpression `(,'quasiquote ,e)
                                                 vars4apps))
                   (LET ,var
                        (CALL 0 *VIEW*))))
               vars4apps))
         (ret-code
          `((RETURN ,(compiled-subexpression expression
                                             vars4apps))))
         (compilate (fold-right append ret-code apps-code)))
    (match compilate 
      ((code ...
          ('LET v ('CALL lbl '*VIEW*))
          ('RETURN v)) ;;; a tail-call! optimize! o/
       `(,@code (GOTO ,lbl)))
      (_ compilate))))

;;;; TMP TMP TMP
(define (massage-addr a)
  (match a
    ('* '*VIEW*)
    (('CAR a) `(CAR ,(massage-addr a)))
    (('CDR a) `(CDR ,(massage-addr a)))))

(define (massage t)
  (match t
    (('CONS? a) `(CONS? ,(massage-addr a)))
    (('NIL? a) `(NIL? ,(massage-addr a)))
    (('EQ? (? number? n) a) `(EQ? ,n ,(massage-addr a)))
    (('EQ? ('quote e) a) `(EQ? (quote ,e) ,(massage-addr a)))
    (('EQ? a a*) `(EQ? ,(massage-addr a) ,(massage-addr a*)))
    (('NUM? a) `(NUM? ,(massage-addr a)))
    (('SYM? a) `(SYM? ,(massage-addr a)))
    (('ATM? a) `(ATM? ,(massage-addr a)))))

;; don't throw up
(define (tree->FC tree)
  (let loop ((n 0)
             (tree tree))
    (match tree
      (('LEAF ('BINDING binding BODY expr))
       (let* ((block (append (map (lambda ((s . v))
                                    `(LET ,s ,(massage-addr v))) binding)
                             (compiled-expression expr))))
         `((,n . ,block))))
      (('IF t 'FAIL! 'ELSE fb)
       (let* ((code0 (loop (+ n 1) fb))
              (block `((IF ,[massage t] ,(+ n 1) ELSE mismatch))))
         `((,n . ,block)
           ,@code0)))
      (('IF t tb 'ELSE 'FAIL!)
       (let* ((code0 (loop (+ n 1) tb))
              (block `((IF ,[massage t] ,(+ n 1) ELSE mismatch))))
         `((,n . ,block)
           ,@code0)))
      (('IF t tb 'ELSE fb)
       (let* ((code0 (loop (+ n 1) tb))
              (code1 (loop (+ n 1 (length code0)) fb))
              (block `((IF ,[massage t] ,(+ n 1)
                        ELSE ,(+ n 1 (length code0)))))) ;; XD
         `((,n . ,block)
           ,@code0
           ,@code1))))))

;;; he_he
(display "(")(newline)
[map (lambda (c) (pretty-print c) 23)
     (tree->FC (mk-tree (clauses->rM [read])))]
[pretty-print '(mismatch (RETURN 'no-match))]
(display ")")(newline)

