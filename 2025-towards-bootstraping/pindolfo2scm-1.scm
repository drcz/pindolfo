;;; this is pindolfo->scm compiler number ONE (ugly WIP prototype)
;;; it drives one level deep into the process graph.
(use-modules (grand scheme))

(define (lookup key #;in binding)
  (match binding
    (() #f)
    (((k . expr) . binding*) (if (equal? k key) expr
                              #;otherwise (lookup key binding*)))))

(define (sym+num s n)
  (string->symbol (string-append (symbol->string s) (number->string n))))

(define (sym+sym s s*)
  (string->symbol (string-append (symbol->string s) (symbol->string s*))))

(e.g. (sym+num 'clause 23) ===> clause23)
(e.g. (sym+sym 'var- 'x) ===> var-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define DEBUG-MODE #f)

(define (dbg-massage-proc def) ;; co się kurwa paczysz?!
  (match def
    (('define (proc-name . args) body)
     `(define (,proc-name . ,args)
        (write (list (quote ,proc-name) . ,args)) (newline)
        ,body))
    (_ def)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define var? symbol?)
(define (vartype? x) (member? x '(num sym atm exp)))

(define (binop? x) (member? x '(+ * - / % <)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; first we turn the source program [a list of (pattern action) clauses]
;; into blueprint from which the target program will be built:
;; a sequence of requirements addr.req followed by a leaf [action info]
;; i.e. triplet (<identifier> <bindings (var . addr)> <action's body>)

(define (clause->blueclause (pattern action) clause-id)
  (let walk ((todo `((,pattern *)))
             (reqs '())
             (binding '()))
    (match todo
      (()
       `(,@reqs (,clause-id ,binding ,action)))
      ((('_ addr) . todo*)
       (walk todo* reqs binding))
      (((() addr) . todo*)
       (walk todo* `(,@reqs (,addr . (NIL?))) binding))
      ((((? number? n) addr) . todo*)
       (walk todo* `(,@reqs (,addr . (EQ? ,n))) binding))
      (((('quote (? symbol? s)) addr) . todo*)
       (walk todo* `(,@reqs (,addr . (EQ? (quote ,s)))) binding))
      (((((? vartype? T) (? symbol? v)) addr) . todo*)
       (match (lookup v binding)
           (#f (let* ((binding* `((,v . ,addr) . ,binding))
                      (req (match T
                             ('num `(,addr . (NUM?)))
                             ('sym `(,addr . (SYM?)))
                             ('atm `(,addr . (ATM?)))
                             ('exp #f)))
                      (reqs* (if req `(,@reqs ,req) reqs)))
                 (walk todo* reqs* binding*)))
           (addr* (walk todo* `(,@reqs (,addr . (EQa? ,addr*))) binding))))
      ((((pat . pat*) addr) . todo*)
       (let* ((todo** `((,pat (car ,addr)) (,pat* (cdr ,addr)) . ,todo*))
              (reqs* `(,@reqs (,addr . (CONS?)))))
         (walk todo** reqs* binding))))))

;; for clause identifiers for now we'll use natural numbers m'kay?
(define (program->blueprint program)
  (map clause->blueclause program (iota (length program))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; based on the blueprint we will generate procedures for actions
;; (from leaves, i.e. (id binding action) triplets)...

;; NB to avoid name collisions, the variables used in source program
;;    we prepend with "v-", and the ones we generate for APP results
;;    with "a-".

(define (apps-in action)
  (define (apps-in-qq qq)
    (match qq
      (('unquote e) (apps-in e))
      ((e . e*) `(,@(apps-in-qq e) ,@(apps-in-qq e*)))
      (_ '())))
  (match action 
    (() '())
    ((? symbol?) '())
    ((? number?) '())
    (('quote _) '())
    (('quasiquote qq) (apps-in-qq qq))
    (((? binop?) e e*) `(,@(apps-in e) ,@(apps-in e*)))
    (('rec e) `(,@(apps-in-qq e) ,action))))

(define (apps2vars-map #;for action)
  (let ((apps (apps-in action)))
    (map (lambda (app index) `(,app . ,(sym+num 'a- index)))
         apps (iota (length apps)))))

(define (cons<-quasiquote qq #;for-subexpressions-using comp-sub)
  (match qq
    (() ''()) ;; !!
    (('unquote e) (comp-sub e))
    ((q . q*) `(cons ,(cons<-quasiquote q comp-sub)
                     ,(cons<-quasiquote q* comp-sub)))
    (e `(quote ,e))))

(define (compiled-expression expr #;wrt apps2vars)
  (let cmpld ((expr expr))
    (match expr
      (() ''()) ;; !!
      ((? var? v) (sym+sym 'v- v))
      ((? number? num) num)
      (('quote e) expr)
      (('quasiquote qq) (cons<-quasiquote qq cmpld))
      (('/ e e*) `(quotient ,(cmpld e) ,(cmpld e*))) ;; sorry!
      (('% e e*) `(modulo ,(cmpld e) ,(cmpld e*))) ;; sorry!
      (('< e e*) `((lambda (n m)
                     (if (< n m) 'YES 'NO))
                   ,(cmpld e) ,(cmpld e*))) ;; super sorry!
      (((? binop? o) e e*) `(,o ,(cmpld e) ,(cmpld e*)))
      (('rec e) (lookup expr apps2vars)))))

(define (compiled-action action)
  (let* ((apps2vars (apps2vars-map action))
         (apps-code
          (map (lambda ((('rec e) . var))
                 `(,var (DISPATCH ,(compiled-expression
                                    `(,'quasiquote ,e) apps2vars))))
               apps2vars))
         (ret-code (compiled-expression action apps2vars))
         (compilate (if (null? apps-code)
                        ret-code
                        `(let* ,apps-code ,ret-code))))
    compilate))

(define (compiled-leaf (clause-id binding action))
  (let* ((proc-name (sym+num 'LEAF clause-id))
         (args (map (lambda ((var . addr)) (sym+sym 'v- var)) binding))
         (body (compiled-action action)))
    `(define (,proc-name . ,args) ,body)))

(e.g. (compiled-leaf '(23
                       ((x . aaa) (y . bbb))
                       (rec (APD ,(rec (APD ,x ,y)) ,x))))
      ===>
      (define (LEAF23 v-x v-y)
        (let* ((a-0 (DISPATCH (cons 'APD (cons v-x (cons v-y '())))))
               (a-1 (DISPATCH (cons 'APD (cons a-0 (cons v-x '()))))))
          a-1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ...and now we use the blueprint to generate DISPATCH procedure,
;; i.e. the big nested if thing.

(define (cnd-concerning addr #;in reqs)
  (match reqs
    (() #f)
    (((a . cnd) . reqs*) (if (equal? a addr)
                             cnd ;; there's gonna be at most one.
                             (cnd-concerning addr reqs*)))))

(define (stronger? cnd0 #;than cnd1)
  (match `(,cnd0 ,cnd1)
    ((('EQ? (? number?)) ('NUM?)) #t)
    ((('EQ? ('quote (? symbol?))) ('SYM?)) #t)
    ((('EQ? _) ('ATM?)) #t)
    ((('NUM?) ('ATM?)) #t)
    ((('SYM?) ('ATM?)) #t)
    ((('NIL?) ('ATM?)) #t)
    (_ #f)))

(define (weaker? cnd0 #;than cnd1) (stronger? cnd1 cnd0))

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

(define (qq->cons qq) (cons<-quasiquote qq (lambda (x) x))) ;; lol

(e.g. (qq->cons '(REV ,xs (,x . ,rs)))
      ===> (cons 'REV (cons xs (cons (cons x rs) '()))))

(define (expr-for addr mexpr)
  (match addr
    ((? var?) mexpr) ;; sure?
    (('car addr*) (match (expr-for addr* mexpr)
                    (('cons mexpr* _) mexpr*)
                    (mexpr* `(car ,mexpr*))))
    (('cdr addr*) (match (expr-for addr* mexpr)
                    (('cons _ mexpr*) mexpr*)
                    (mexpr* `(cdr ,mexpr*))))))
 
(e.g. (expr-for '(car *) '(cons x 'elo)) ===> x)
(e.g. (expr-for '(cdr *) '(cons x 'elo)) ===> 'elo)
(e.g. (expr-for '* '(cons x 'elo)) ===> (cons x 'elo))
(e.g. (expr-for '(car (car *)) '(cons (cons 'elo 'yolo) y)) ===> 'elo)
(e.g. (expr-for '(cdr (car *)) '(cons (cons 'elo 'yolo) y)) ===> 'yolo)
(e.g. (expr-for '(cdr *) '(cons (cons 'elo 'yolo) y)) ===> y)
(e.g. (expr-for '(car (cdr *)) '(cons (cons 'elo 'yolo) y)) ===> (car y))

(define (ground? expr)
  (match expr
    ((? var?) #f)
    (() #t)
    ((? number?) #t)
    (('quote _) #t)
    ((_ e) (ground? e))
    ((_ e e*) (and (ground? e) (ground? e*)))))

(define (reduce-req req mexpr)
  (let* (((addr . cnd) req)
         (addr/cnstr (expr-for addr mexpr))
         (RES
          (if (or (not (ground? addr/cnstr))
                  (eq? (car cnd) 'EQa?)) ;;; LOL (for now)
              (match `(,cnd ,addr/cnstr)
                ((('CONS?) ('cons _ _)) #t) ;; TODO is that even possible?
                ((('NIL?) ('cons _ _)) #f) ;; !!
                ((('NUM?) ('cons _ _)) #f) ;; !!
                ((('SYM?) ('cons _ _)) #f) ;; !!
                ((('ATM?) ('cons _ _)) #f) ;; !!
                ((('EQ? (? number?)) ('cons _ _)) #f) ;; !!
                ((('EQ? (? symbol?)) ('cons _ _)) #f) ;; !!
                ((('EQ? ('quote _)) ('cons _ _)) #f) ;; ?!
                ((('EQa? addr*) _)
                 `(,addr/cnstr . (EQa? ,(expr-for addr* mexpr)))) ;; !!
                (_ `(,addr/cnstr . ,cnd))) ;; <- it can't decide now
              (match `(,cnd ,addr/cnstr)
                ((('CONS?) ('cons _ _)) #t)
                ((('NIL?) ()) #t)
                ((('NIL?) ('quote ())) #t)          ;;; !!! tricky
                ((('NUM?) ('quote (? number?))) #t) ;;; !!! stuff!
                ((('NUM?) (? number?)) #t)
                ((('SYM?) ('quote (? symbol?))) #t) ;;; !!!
                ((('ATM?) ('cons _ _)) #f)
                ((('ATM?) _) #t)
                ((('EQ? e) e) #t)
                ((('EQ? ('quote e)) e) #t) ;;; ??? ...just
                ((('EQ? e) ('quote e)) #t) ;;; ??? checkin'
                (_ #f)))))
    ;[pretty-print `(RR ,req ,mexpr => ,RES)] ;; pretty-print-debugging ;)
    RES))


(define (mk-test cnd addr)
  (define (massage-addr a) a) ;; i guess we don't need it anymore?
  (match `(,@cnd ,addr)
    (('CONS? a) `(pair? ,(massage-addr a)))
    (('NIL? a) `(null? ,(massage-addr a)))
    (('EQ? (? number? n) a) `(equal? ,n ,(massage-addr a)))
    (('EQ? ('quote e) a) `(equal? (quote ,e) ,(massage-addr a)))
    (('EQa? a a*) `(equal? ,(massage-addr a) ,(massage-addr a*)))
    (('NUM? a) `(number? ,(massage-addr a)))
    (('SYM? a) `(symbol? ,(massage-addr a)))
    (('ATM? a) `(not (pair? ,(massage-addr a))))))

(define (mk-leaf (clause-id binding action)) ;; just the call
  (let* ((proc-name (sym+num 'LEAF clause-id))
         (args (map (lambda ((var . addr)) addr) binding)))
    `(,proc-name . ,args)))

(define (decission-tree blueprint mexpr)
  (let walk ((bp blueprint))
    (match bp
      (()
       `(FAIL ,mexpr))
      ((((cid bnd act)) . _)
       (let* ((bnd* (map (lambda ((v . a)) `(,v . ,(expr-for a mexpr))) bnd)))
         (mk-leaf `(,cid ,bnd* ,act))))
      (((r0 . reqs) . _)
       (match (reduce-req r0 mexpr)
         (#t (walk (filter-map (lambda (rs) (sat-reqs r0 rs)) bp)))
         (#f (walk (filter-map (lambda (rs) (unsat-reqs r0 rs)) bp)))
         ((addr . cnd)
          (let* ((sat-bp (filter-map (lambda (rs) (sat-reqs r0 rs)) bp))
                 (unsat-bp (filter-map (lambda (rs) (unsat-reqs r0 rs)) bp))
                 (test (mk-test cnd addr))
                 (then-t (walk sat-bp))
                 (else-t (walk unsat-bp)))
            `(if ,test ,then-t ,else-t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; now we want to project that main tree for each ,,application'' in
;;; each leaf so that instead of DISPATCH0 they have their dedicated
;;; subtree to walk. we don't project leaves, just the decission tree.

(define (vars-in mexpr) ;; mexpr already in cons form
  (match mexpr 
    ((? var? v) `(,v))
    (('quote _) '())
    ((_ e) (vars-in e)) ;; car, cdr
    ((_ h t) (union (vars-in h) (vars-in t))) ;;; cons, +, -, *, no?
    (_ '())))

[e.g. (vars-in (qq->cons '(APD ,xs ,ys))) ===> (ys xs)]

;;; seen is (mexpr . def) alist where def is just scheme definition
;;; of dispatch computed for mexpr so (define (disp-id . args) body)
;;; this way we can keep track of all the ,,already computed'' (i.e.
;;; _seen_) tree projections, and once they're all computed we can
;;; extract the code trivially (map cdr seen) y'know...

(define (mk-dispatch mexpr seen blueprint) ;; -> (call seen*)
  (match (lookup mexpr seen) ;; TODO normalize mexpr?
    (('define (disp-id . args) body) ;; already computed
     `((,disp-id . ,args) seen))
    (#f ;; fresh one, compute, add to seen and generate call:
     (let* ((body (decission-tree blueprint mexpr))
            (args (vars-in mexpr))
            (disp-id (sym+num 'DISPATCH (length seen)))
            (def `(define (,disp-id . ,args) ,body))
            (seen* `(,@seen (,mexpr . ,def))))
       `((,disp-id . ,args) ,seen*)))))

;;; it's just a sketch no worries (no thinking => no overthinking \o/)
(define (apps-from leaf-proc)
  (match leaf-proc
    (('define _ ('let* as body))
     (delete-duplicates (map (lambda ((var app)) app) as)))
    (('define _ _) '())))

(define (substitute-apps-inside leaf-proc seen)
  (match leaf-proc
    (('define head ('let* as body))
     (let* ((as* (map (lambda ((var ('DISPATCH mexpr)))
                        (match (lookup mexpr seen)
                          (('define (disp-id . args) body)
                           `(,var (,disp-id . ,args)))
                          (#f `(,var (DISPATCH ,mexpr)))))
                      as)))
       `(define ,head (let* ,as* ,body))))
    (_ leaf-proc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; so now we can put all the pieces in place:

(define (compiled program)
  (let* ((blueprint (program->blueprint program))
         (fail-proc `(define (FAIL a)
                       (display `(NO MATCH FOUND FOR ,a))
                       (newline)
                       (error "halt")))
         ((init-call seen) (mk-dispatch 'v0 '() blueprint))
         ;;; init-call is just (DISPATCH0 v0) so we ignore it for now
         (leaves (map last blueprint)) ;; TODO filter reachable?
         (leaf-procs0 (map compiled-leaf leaves))
         (all-apps (apply union (map apps-from leaf-procs0)))
         (seen* (let loop ((todo all-apps)
                           (seen seen))
                  (match todo
                    (() seen)
                    ((('DISPATCH mexpr) . todo*)
                     (let* (((_ seen*) (mk-dispatch mexpr
                                                    seen
                                                    blueprint)))
                       (loop todo* seen*))))))
         (dispatch-procs (map cdr seen*))
         (leaf-procs (map (lambda (lp) (substitute-apps-inside lp seen*))
                           leaf-procs0))
         (target-src `(,@leaf-procs
                       ,@dispatch-procs
                       ,fail-proc
                       (begin (write (DISPATCH0 (read))) (newline)))))
    (if DEBUG-MODE
        (map dbg-massage-proc target-src)
        target-src)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display ";; pindolfo2scm v1.3") (newline)
(map pretty-print (compiled (read)))
(newline)
