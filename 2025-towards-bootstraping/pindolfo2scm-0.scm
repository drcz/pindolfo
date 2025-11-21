;;; this is pindolfo->scm compiler number zero (prototype collage)
;;; -- i.e. a weakest possible one which doesn't optimise anything
;;; but sets up framework for the next one.
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
(define var? symbol?)
(define (vartype? x) (member? x '(num sym atm exp)))

(define (binop? x) (member? x '(+ * - %)))

(define DISPATCH-VARNAME 'v0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; first we turn the source program [a list of (pattern action) clauses]
;; into blueprint from which the target program will be built:
;; a sequence of requirements addr.req followed by a leaf [action info]
;; i.e. triplet (<identifier> <bindings (var . addr)> <action's body>)

(define (clause->blueclause (pattern action) clause-id)
  (let walk ((todo `((,pattern ,DISPATCH-VARNAME)))
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
         (addr* (walk todo* `(,@reqs (,addr . (EQ? ,addr*))) binding))))
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
    (() ''())
    (('unquote e) (comp-sub e))
    ((q . q*) `(cons ,(cons<-quasiquote q comp-sub)
                     ,(cons<-quasiquote q* comp-sub)))
    (e `(quote ,e))))

(define (compiled-expression expr #;wrt apps2vars)
  (let cmpld ((expr expr))
    (match expr
      (() ''())
      ((? var? v) (sym+sym 'v- v))
      ((? number? num) num)
      (('quote e) expr)
      (('quasiquote qq) (cons<-quasiquote qq cmpld))
      (('% e e*) `(modulo ,(cmpld e) ,(cmpld e*))) ;; sorry
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
          a-1))) ;;; <- see? the 2nd app of DISPATCH could be outside

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

(define (mk-test cnd addr)
  (define (massage-addr a) a) ;; i guess we don't need it anymore?
  (match `(,@cnd ,addr)
    (('CONS? a) `(pair? ,(massage-addr a)))
    (('NIL? a) `(null? ,(massage-addr a)))
    (('EQ? (? number? n) a) `(equal? ,n ,(massage-addr a)))
    (('EQ? ('quote e) a) `(equal? (quote ,e) ,(massage-addr a)))
    (('EQ? a a*) `(equal? ,(massage-addr a) ,(massage-addr a*)))
    (('NUM? a) `(number? ,(massage-addr a)))
    (('SYM? a) `(symbol? ,(massage-addr a)))
    (('ATM? a) `(not (pair? ,(massage-addr a))))))

(define (mk-leaf (clause-id binding action)) ;; just the call
  (let* ((proc-name (sym+num 'LEAF clause-id))
         (args (map (lambda ((var . addr)) addr) binding)))
    `(,proc-name . ,args)))

(define (decission-tree blueprint)
  (match blueprint
    (() `(FAIL ,DISPATCH-VARNAME))
    (((leaf) . _) (mk-leaf leaf))
    (((r0 . reqs) . blueprint*)
     (let* ((sat-bp (filter-map (lambda (rs) (sat-reqs r0 rs)) blueprint))
            (unsat-bp (filter-map (lambda (rs) (unsat-reqs r0 rs)) blueprint))
            ((addr . cnd) r0)
            (test (mk-test cnd addr))
            (then-t (decission-tree sat-bp))
            (else-t (decission-tree unsat-bp)))
       `(if ,test ,then-t ,else-t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; so now we can put all the pieces in place:

(define (compiled program)
  (let* ((blueprint (program->blueprint program))
         (leaves (map last blueprint))
         (leaf-procs (map compiled-leaf leaves))
         (dispatch-proc `(define (DISPATCH ,DISPATCH-VARNAME)
                           ,(decission-tree blueprint)))
         (fail-proc `(define (FAIL a)
                       (display `(NO MATCH FOUND FOR ,a))
                       (newline)
                       (error "halt"))))
    `(,@leaf-procs
      ,fail-proc
      ,dispatch-proc
      (begin (write (DISPATCH (read))) (newline)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display ";; pindolfo2scm v0.1") (newline)
(map pretty-print (compiled (read)))
(newline)
