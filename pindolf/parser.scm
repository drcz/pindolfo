(define-module (pindolf parser)
  #:use-module (grand scheme)
  #:export (parsed))

;;; it just makes more sense:
(define member? member)
(define string<-symbol symbol->string)
(define string<-list list->string)
(define list<-string string->list)
(define symbol<-string string->symbol)
;;; see?
(define (list<-symbol s) (list<-string (string<-symbol s)))
(define (symbol<-list cs) (symbol<-string (string<-list cs)))

(e.g. (list<-symbol 'Żółć-23) ===> (#\Ż #\ó #\ł #\ć #\- #\2 #\3))
(e.g. (symbol<-list '(#\B #\a #\R #\y #\Ł #\a)) ===> BaRyŁa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; now this is just a crude tool to avoid silly mistakes i've been
;;; doing for the last few days. before i come up with something
;;; fancier...
;;; also, since i've no idea how to take control over (read), which
;;; btw won't be a long-term solution anyway, let's [un-]simplify
;;; the syntax here too (massaged-pattern).

(define (vartype? x) (member? x '(num sym atm exp)))

(define (vartype<-symbol s)
  (match (list<-symbol s)
    ((#\% . var) `(num ,(symbol<-list var)))
    ((#\$ . var) `(sym ,(symbol<-list var)))
    ((#\@ . var) `(atm ,(symbol<-list var)))
    ((#\? . var) `(exp ,(symbol<-list var)))
    (_ s)))

(e.g. (vartype<-symbol '%n) ===> (num n))
(e.g. (vartype<-symbol '$key) ===> (sym key))
(e.g. (vartype<-symbol '@not-a-pair) ===> (atm not-a-pair))
(e.g. (vartype<-symbol '?x) ===> (exp x))
(e.g. (vartype<-symbol 'wat?) ===> wat?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (massaged-pattern p)
  (match p ;;; it's _much_ better to be explicit here y'know
    ('_ p)
    (() p)
    ('T p)
    ((? number?) p)
    (('num _) p)
    (('sym _) p)
    (('atm _) p)
    (('exp _) p)
    (('quote e) p) ;;; nb not going deeper!
    ((? symbol?) (vartype<-symbol p))
    ((p . ps) `(,(massaged-pattern p) . ,(massaged-pattern ps)))))

(e.g. (massaged-pattern '('elo %n $op %n* '= ?x))
      ===> ('elo (num n) (sym op) (num n*) '= (exp x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (vars-in-pattern p)
  (match (massaged-pattern p) ;; massaged just-in-case
    (((? vartype?) v) `(,v))
    (('quote _) '())
    ((p . ps) (union (vars-in-pattern p) (vars-in-pattern ps)))
    (_ '())))

(e.g. (same-set? (vars-in-pattern '('hi! %n %m)) '(m n)))
(e.g. (same-set? (vars-in-pattern '('ho! $sym '= $sym)) '(sym)))
(e.g. (same-set? (vars-in-pattern '(%n (@a . ?as) $s)) '(n a as s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (vars.types-in-pattern p)
  (match (massaged-pattern p)
    (((? vartype? t) (? symbol? v)) `((,v . ,t)))
    (('quote _) '())
    ((p . ps) (union (vars.types-in-pattern p)
                     (vars.types-in-pattern ps)))
    (_ '())))

(e.g. (same-set?
       (vars.types-in-pattern (massaged-pattern '(%n (@a . ?a) $n)))
       '((a . atm) (a . exp) (n . sym) (n . num))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (vars-in-expression e)
  (define (vars-in-qq qq)
    (match qq
      (('unquote e) (vars-in-expression e))
      ((e . e*) (union (vars-in-qq e) (vars-in-qq e*)))
      (_ '())))
  (match e
    (() '())
    ('T '())
    ((? number?) '())
    ((? symbol?) `(,e))
    (('quote _) '())
    (('quasiquote qq) (vars-in-qq qq))
    (('+ e e*) (union (vars-in-expression e)
                      (vars-in-expression e*)))
    (('- e e*) (union (vars-in-expression e)
                      (vars-in-expression e*)))
    (('& e) (vars-in-qq e))))

(e.g. (same-set? (vars-in-expression '(+ (& (val ,e)) (& (cmp ,e*))))
                 '(e e*)))
(e.g. (same-set? (vars-in-expression '`(it's just (,h . ,h))) '(h)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ((equals? x) y) (equal? x y))

(define (lookups k kvs)
  (match kvs
    (() '())
    ((((? (equals? k)) . t) . kvs*) `(,t . ,(lookups k kvs*)))
    ((_ . kvs*) (lookups k kvs*))))

(e.g. (lookups 'a '((a . 3) (b . 42) (a . 997))) ===> (3 997))
(e.g. (lookups 'b '((a . 3) (b . 42) (a . 997))) ===> (42))
(e.g. (lookups 'c '((a . 3) (b . 42) (a . 997))) ===> ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (consistent-types? ts)
  (= (length (set ts)) 1))

(e.g. (consistent-types? '(num num num)))
(e.g. (not (consistent-types? '(num sym))))
(e.g. (not (consistent-types? '(num exp)))) ;; sure?

(define (inconsistent-pattern? p)
  (let* ((p (massaged-pattern p))
         (vts (vars.types-in-pattern p)))
    (let checking ((vs (vars-in-pattern p))
                   (log '()))                  
    (match vs
      (()
       log)
      ((v . vs*)
       (match (lookups v vts)
         (() (checking vs* `(,@log (no type for ,v))))
         ((t) (checking vs* log))
         ((ts ...)
          (checking vs*
                    (if (consistent-types? ts)
                        log
                        `(,@log (inconsistent types for ,v ,ts)))))))))))

(e.g. (inconsistent-pattern? '('+ %n %n)) ===> ())

(e.g. (inconsistent-pattern? '('+ %n $n)) ===>
      ((inconsistent types for n (sym num))))

(e.g. (inconsistent-pattern? '(@x or ?x and @y with $y)) ===>
      ((inconsistent types for y (sym atm))
       (inconsistent types for x (exp atm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (inconsistent-clause? pat expr)
  (let* ((pvs (vars-in-pattern pat))
         (evs (vars-in-expression expr))
         (unbnd (map (lambda (v) `(unbound variable ,v))
                     (difference evs pvs)))
         (incns (inconsistent-pattern? pat))
         (problems (append incns unbnd)))
    problems))

(e.g. (inconsistent-clause? '('+ %n $n) '(+ n m))
      ===> ((inconsistent types for n (sym num))
            (unbound variable m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pattern? x)
  (match x
    ('_ #t)
    (() #t)
    ('T #t)
    ((? number?) #t)
    (('num _) #t)
    (('sym _) #t)
    (('atm _) #t)
    (('exp _) #t)
    (('quote e) #t) ;;; nb not going deeper!
    ((? symbol?) (match (vartype<-symbol x)
                   (('num _) #t)
                   (('sym _) #t)
                   (('atm _) #t)
                   (('exp _) #t)
                   (_ #f)))
    ((x . x*) (and (pattern? x) (pattern? x*)))))

(e.g. (pattern? '('+ %n %m)))
(e.g. (pattern? '('+ (num n) (exp m))))
(e.g. (not (pattern? '(j 23))))
(e.g. (not (pattern? '`(a ,b))))

(define (expression? x)
  (define (correct-qq? qq)
    (match qq
      (('unquote e) (expression? e))
      ((q . q*) (and (correct-qq? q)
                     (correct-qq? q*)))
      (_ #t)))
  (match x
    (() #t)
    ('T #t)
    ((? symbol?) #t)
    ((? number?) #t)
    (('quote e) #t)
    (('quasiquote qq) (correct-qq? qq))
    (('+ e e*) (and (expression? e) (expression? e*)))
    (('- e e*) (and (expression? e) (expression? e*)))
    (('& expr) (correct-qq? expr))
    (_ #f)))

(e.g. (expression? '(- n 23)))
(e.g. (expression? '(& (compute ,stuff ,(+ n 3)))))
(e.g. (not (expression? '(* 2 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (parsed code)
  (let check-parse-massage ((code code)
                            (code++ '())
                            (clause 0))
    (match code
      (()
       code++)
      ((((? pattern? p) (? expression? e)) . code*)
       (let* ((p* (massaged-pattern p))
              (problems (inconsistent-clause? p* e)))
         (if (> (length problems) 0)
             `(PARSE-ERROR in cluase ,clause . ,problems)
             (check-parse-massage code*
                                  `(,@code++ (,p* ,e))
                                  (1+ clause)))))
      ((((? pattern?) x) . _)
        `(PARSE-ERROR in clause ,clause -- not an expression ,x))
      (((x (? expression?)) . _)
        `(PARSE-ERROR in clause ,clause -- not a pattern ,x))
      (x
        `(PARSE-ERROR in caluse ,clause -- malformed clause ,x)))))

(e.g. (parsed '((('apd     ()     ?ys) ys)
                (('apd (?x . ?xs) ?ys) `(,x . ,(& (apd ,xs ,ys))))))
      ===> ((('apd () (exp ys)) ys)
            (('apd ((exp x) . (exp xs)) (exp ys))
                   `(,x . ,(& (apd ,xs ,ys))))))

(e.g. (parsed '((('foo %n %m) (+ n m))
                (('foo $x ?xs) `(,x ,y))))
      ===> (PARSE-ERROR in cluase 1 ((unbound variable y))))

(e.g. (parsed '((('foo %n %m) (+ n m))
                (('foo $x ?xs))))
      ===> (PARSE-ERROR in caluse 1
                   -- malformed clause ((('foo $x ?xs)))))

(e.g. (parsed '((('bad %n $n) (+ n n))))
      ===> (PARSE-ERROR in cluase 0 ((inconsistent types for n (sym num)))))

;;; that should do!
