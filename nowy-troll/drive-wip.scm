(use-modules (grand scheme))

(define (member? x xs) (and (member x xs) #t)) ;; :)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hmmm

(define (leaves tree)
  (match tree
    (('IF _ t* 'ELSE t**) `(,@(leaves t*) ,@(leaves t**)))
    (('LEAF l) `(,l))
    ('FAIL! '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; massive copy'n'paste from old ,,compiler''

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

(e.g. (applications-in '`(,x . ,(& (APD ,xs ,ys))))
      ===> ( (& (APD (unquote xs) (unquote ys))) ))

(e.g. (applications-in '(& (PLUS ,(& (VAL ,a)) ,(& (VAL ,b)))))
      ===> ( (& (VAL ,a))
             (& (VAL ,b))
             (& (PLUS ,(& (VAL ,a)) ,(& (VAL ,b)))) ))

(define (application->vars-map #;for expression)
  (let ((applications (applications-in expression)))
    (map (lambda (app index) `(,app . (V ,index)))
         applications (iota (length applications)))))

(e.g. (application->vars-map '(& (PLUS ,(& (VALUE ,a)) ,(& (VALUE ,b)))))
      ===>( ((& (VALUE ,a)) . (V 0))
            ((& (VALUE ,b)) . (V 1))
            ((& (PLUS ,(& (VALUE ,a)) ,(& (VALUE ,b)))) . (V 2)) ))

(define (cons<-quasiquote qq #;for-subexpressions-using comp-sub)
  (match qq
    (() '())
    (('unquote e) (comp-sub e))
    ((q . q*) `(CONS ,(cons<-quasiquote q comp-sub)
                     ,(cons<-quasiquote q* comp-sub)))
    (e `(quote ,e))))
;;; that comp-sub thing is a bit of afterthought for now but the point
;;; is to be able to compile unquoted expressions with whomever called
;;; cons<-quasiquote (it should get inside compiled-subexpression...).

(e.g. (cons<-quasiquote '(+ ,x ,y) (lambda (x) x)) ;; just trivial comp-sub
      ===> (CONS '+ (CONS x (CONS y ()))))
(e.g. (cons<-quasiquote '(a (b ,c) d) (lambda (x) x))
      ===> (CONS 'a (CONS (CONS 'b (CONS c ())) (CONS 'd ()))))

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

(e.g. (compiled-subexpression '`(hi there (+ ,x ,y)) '())
      ===> (CONS 'hi
                 (CONS 'there
                       (CONS (CONS '+ (CONS x (CONS y ())))
                             ()))))

(e.g. (let* ((expression '(+ (& (VAL ,a)) (& (VAL ,b))))
             (vars4apps (application->vars-map expression)))
        (compiled-subexpression expression vars4apps))
      ===> (+ (V 0) (V 1)))

(e.g. (let* ((expression '`(,x . ,(& (APD ,xs ,ys))))
             (vars4apps (application->vars-map expression)))
        (compiled-subexpression expression vars4apps))
      ===> (CONS x (V 0)))

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
    #;(match compilate 
      ((code ...
          ('LET v ('CALL lbl '*VIEW*))
          ('RETURN v)) ;;; a tail-call! optimize! o/
       `(,@code (GOTO ,lbl)))
      (_ compilate))
    compilate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; now...

(define (updated bnd s e)
  (match bnd
    (() `((,s . ,e)))
    (((k . v) . bnd*) (if (equal? k s)
                          `((,k . ,e) . ,bnd*)
                          `((,k . ,v) . ,(updated bnd* s e))))))

(define (inlinexp exp bnd)
  (match exp
    (('CONS h t) `(CONS ,(inlinexp h bnd) ,(inlinexp t bnd)))
    (('+ h t) `(+ ,(inlinexp h bnd) ,(inlinexp t bnd)))
    (('* h t) `(* ,(inlinexp h bnd) ,(inlinexp t bnd)))
    (('- h t) `(- ,(inlinexp h bnd) ,(inlinexp t bnd)))
    (() '())
    ('T 'T)
    ((? number? n) n)
    (('quote _) exp)
    (('CALL 0 e) `(CALL 0 ,(inlinexp e bnd)))
    (v (match (lookup v bnd)
         (#f v)
         (e e)))))
    
(define (inline-lets code)
  (let loop ((code code)
             (bnd '()))
    (match code
      (() '())
      ((('LET v e) . code*) (loop code* (updated bnd
                                                 v
                                                 (inlinexp e bnd))))
      ((('RETURN e)) (inlinexp e bnd) #;`((RETURN ,(inlinexp e bnd))))
      #;((('GOTO _)) code))))


(define (massaged-leaf leaf)
  (let* ((('BINDING bindings 'BODY body) leaf)
         (code `(,@(map (lambda ((s . v)) `(LET ,s ,v)) bindings)
                 ,@(compiled-expression body))))
    #;code
    (inline-lets code)))

(e.g.
 (massaged-leaf [cadr (mk-tree (clauses->rM p1)
                               (qq->cons '(APD (q w e) ,bs)))])
 ===> (CONS 'q
            (CALL 0 (CONS 'APD
                          (CONS (CONS 'w
                                      (CONS 'e ()))
                                (CONS bs ())))))
     #;((LET ys bs)
        (LET xs (CONS 'w (CONS 'e ())))
        (LET x 'q)
        (LET *VIEW* (CONS 'APD (CONS xs (CONS ys ()))))
        (LET (V 0) (CALL 0 *VIEW*))
        (RETURN (CONS x (V 0)))))

(e.g. (massaged-leaf '(BINDING () BODY (& (APD ,xs ,(& (APD ,ys ,zs)))))) 
      ===> 
      (CALL 0
            (CONS 'APD
                  (CONS xs
                        (CONS (CALL 0
                                    (CONS 'APD
                                          (CONS ys
                                                (CONS zs
                                                      ()))))
                              ()))))
         #;((LET *VIEW* (CONS 'APD (CONS ys (CONS zs ()))))
            (LET (V 0) (CALL (0) *VIEW*))
            (LET *VIEW* (CONS 'APD (CONS xs (CONS (V 0) ()))))
            (GOTO 0)))

(define (xtracted-callz mleaf)
  (match mleaf
    (('CALL 0 e) `((,e) . ,(xtracted-callz e)))
    (('CONS h t) `(,@(xtracted-callz h) ,@(xtracted-callz t)))
    (('+ h t) `(,@(xtracted-callz h) ,@(xtracted-callz t)))   
    (('- h t) `(,@(xtracted-callz h) ,@(xtracted-callz t)))
    (('* h t) `(,@(xtracted-callz h) ,@(xtracted-callz t)))
    (_ '())))

(e.g. (xtracted-callz
       (massaged-leaf '(BINDING ()
                        BODY (+ (& (MUL ,n ,m))
                                (& (REM ,n ,m))))))
      ===> (((CONS 'MUL (CONS n (CONS m ()))))
            ((CONS 'REM (CONS n (CONS m ()))))))

(let* ((prog (with-input-from-file "drc.ppf" read) #;p1)
       (tree0 (mk-tree (clauses->rM prog) '*))
       (lvs0 (leaves tree0))
       (lvs0* (map massaged-leaf lvs0))
       (gen1-calls (apply append (map xtracted-callz lvs0*))))
  (map (lambda (c) (write c) (newline) 23) gen1-calls)
  (pretty-print `(XTRACTED ,(length gen1-calls) GEN-1 CALLS))
)
