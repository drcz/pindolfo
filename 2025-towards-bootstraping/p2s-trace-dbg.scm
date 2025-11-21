;;; just a fast/trashy thing from yesterday: lacking better idea we've
;;; converted pnd->scm-0 compiler into ,,html trace generator''. we'll
;;; see if this helps in debugging  pindolfo programs soon...

;;; literally this:
;;; > guile p2s-trace-dbg.scm < tests/classics-0.ppf > classics0-trace.scm
;;; > echo "(REV (dercz likes pindolfo))" | guile classics0-trace.scm > c0.html
;;; > firefox c0.html

;;; maybe it should respect tail calls and not always embed these <details>...
;;; but it doesn't. we'll get back here once we know what we want rly

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


(define (htmls-spitting body) ;; whatever
  (match body
    (('let* bs e)
     (let* ((bs* (append-map (lambda ((v d))
                               `((_ (begin (display (quote ,v)) (display " <- ")))
                                 (,v ,d))) bs))
            (e* `(begin
                   (display "res: ") (pretty-print ,e) (display "</details><br/>") (newline) ,e)))
       `(let* ,bs* ,e*)))
    (e `(begin
          (display "res: <strong>")
          (pretty-print ,e)
          (display "</strong></details><br/>")
          (newline) 
          ,e))))
    
(define (compiled-leaf (clause-id binding action) PROGRAM)
  (let* ((proc-name (sym+num 'LEAF clause-id))
         (args (map (lambda ((var . addr)) (sym+sym 'v- var)) binding))
         (body (htmls-spitting (compiled-action action))))
    `(define (,proc-name . ,args)
       (display "<details><summary>")
       (pretty-print (quote ,(car (list-ref PROGRAM clause-id)))) ;; the pattern y'know
       (display "</summary>")(newline)
       ,@(append-map (lambda (a) `((display "<b>") (display (quote ,a)) (display "</b> <- ") (display ,a) (display "<br/>") (newline)))
                    args)
       ,body))) ;; sorry


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
         (leaf-procs (map (lambda (l) (compiled-leaf l program)) leaves))
         (dispatch-proc `(define (DISPATCH ,DISPATCH-VARNAME)
                           (display "<strong>") (display ,DISPATCH-VARNAME) (display "</strong>") (newline)
                           ,(decission-tree blueprint)))
         (fail-proc `(define (FAIL a)
                       (display "<br/><hr/><strong>") (display `(NO MATCH FOUND FOR ,a)) (display "</strong>") (newline)
                       (error "halt"))))
    `(,@leaf-procs
      ,fail-proc
      ,dispatch-proc
      (begin (write (DISPATCH (read))) (newline)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(write '(use-modules (ice-9 pretty-print))) (newline)
(write '(display "<html>
<head>
<style>
details { background-color: #ddeeff;
          border: 1px solid #000;
          border-radius: 4px;
          padding-left: 7px; }
summary { padding: 3px; }
</style>
</head>
<body>")) (write '(newline)) (newline)

(map pretty-print (compiled (read)))

(write '(display "</body></html>\n")) (newline)
