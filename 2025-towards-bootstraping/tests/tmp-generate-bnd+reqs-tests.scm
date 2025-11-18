(use-modules (grand scheme))

(define (lookup key #;in binding)
  (match binding
    (() #f)
    (((k expr) . binding*) (if (equal? k key) expr
                               #;otherwise (lookup key binding*)))))

;(define var? symbol?)
(define (vartype? x) (member? x '(num sym atm exp)))


(define (reqs+bnd pattern)
  (let walk ((todo `((,pattern *)))
             (reqs '())
             (binding '()))
    (match todo
      (()
       `(REQS: ,[reverse reqs] BND: ,binding))
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
           (#f (let* ((binding* `((,v ,addr) . ,binding))
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

(map (lambda (p) (write `(bnd+reqs ,p)) (display " | ") (write (reqs+bnd p)) (newline))
     '(
       'abc
       (atm x)
       23
       _
       ()
       ('a 'b)
       ('a ('b . 'c))
       ('a . (exp bs))
       ((sym x) . (exp xs))
       ((num n) (num n))
       (6 _ 9)
       (_ . _)
       (_ _)
       (_ _ . _)
       ((sym x) _ (sym x))
       ('apd () (exp ys))
       ('apd ((sym x) . (exp xs)) (exp ys))
       ('hi (sym x) (exp y) (sym x))
       ))


