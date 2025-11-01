(use-modules (grand scheme))

(define (metaexpr->facts me)
  (let traverse ((pend `((,me *ROOT*)))             
                 (facts '())
                 (vars '()))
    (match pend
      (() `(,facts ,vars))
      (((me addr) . pend*)
       (match me
         (('quote e) (traverse pend* ;; że (sym? ,addr) nie dodawać?
                               `((eq? ,addr ,e) . ,facts)
                               vars))
         (() (traverse pend*
                       `((null? ,addr) . ,facts)
                       vars))
         ((? number? n) (traverse pend* ;; że (num? ,addr) nie dodawać?
                               `((eq? ,addr ,n) . ,facts)
                               vars))
         (('num v) (traverse pend*
                             `((num? ,addr) . ,facts)
                             `((,v . ,addr) . ,vars)))
         (('sym v) (traverse pend*
                             `((sym? ,addr) . ,facts)
                             `((,v . ,addr) . ,vars)))
         (('atm v) (traverse pend*
                             `((atom? ,addr) . ,facts)
                             `((,v . ,addr) . ,vars)))
         (('expr v) (traverse pend*
                              facts
                             `((,v . ,addr) . ,vars)))
         ((h . t) (traverse `((,h (car ,addr))
                              (,t (cdr ,addr))
                              . ,pend*)
                            `((cons? ,addr) . ,facts)
                            vars)))))))

(e.g. (metaexpr->facts '('mul (num x) 4)) ===>
      ( ;; facts:
       ((null? (cdr (cdr (cdr *ROOT*))))
        (eq? (car (cdr (cdr *ROOT*))) 4)
        (cons? (cdr (cdr *ROOT*)))
        (num? (car (cdr *ROOT*)))
        (cons? (cdr *ROOT*))
        (eq? (car *ROOT*) mul)
        (cons? *ROOT*))
        ;; vars:
       ((x . (car (cdr *ROOT*)))) ))


(define (var? x)
  (match x
    (('num _) #t)
    (('sym _) #t)
    (('atm _) #t)
    (('expr _) #t)
    (_ #f)))

(define (lookup var #;in binding)
  (match binding
    (() #f)
    (((k . v) . binding*) (if (equal? k var) v (lookup var binding*)))))

(e.g. (lookup '(var x) '(((num n) . 23) ((var x) . 'elo))) ===> 'elo)

(define (update binding var val)
  (match binding
    (() `((,var . ,val)))
    (((k . v) . binding*) (if (equal? k var)
                              `((,k . ,val) . ,binding*)
                              `((,k . ,v) . ,(update binding* var val))))))

(e.g. (update '(((num n) . 23) ((var x) . 'elo)) '(var x) '(cons a b))
      ===> (((num n) . 23) ((var x) cons a b)))

(e.g. (lookup '(var x)
              (update '(((num n) . 23) ((var x) . 'elo)) '(var x) '(cons a b)))
      ===> (cons a b))
      
(define (assq00 s b) ;;; XD
  (match b
    (() #f)
    (((k . v) . b*) (if (equal? s k) `(,k . ,v) (assq00 s b*)))))


(define (substituted binding #;in expr)
  (match expr
    (() '())
    (('quote _) expr)
    ((? number?) expr)
    ((? var? v) (match (assq00 v binding)
                  (#f expr)
                  ((_ . e) e)))
    ((o a b) `(,o ,(substituted binding a) ,(substituted binding b)))
    ((o a) `(,o ,(substituted binding a)))))

(e.g. (substituted '(((num n) . 23) ((expr x) cons a b))
                   '(cons (+ (num n) (num n)) (cons (expr x) '())))
      ===> (cons (+ 23 23) (cons (cons a b) (quote ()))))

(define (simplified expr)
  (match expr
    (('quote _) expr)
    ((? number?) expr)
    ((? var?) expr)
    (('car ('cons e _)) (simplified e))
    (('cdr ('cons _ e)) (simplified e))
    (('cons ('car e) ('cdr e)) (simplified e))
    (('+ (? number? n) (? number? m)) (+ n m))
    (('* (? number? n) (? number? m)) (* n m))
    (('- (? number? n) (? number? m)) (- n m))
    (('+ 0 e) (simplified e))
    (('+ e 0) (simplified e))
    (('- e 0) (simplified e))
    (('* 1 e) (simplified e))
    (('* e 1) (simplified e))
    ((o a b) `(,o ,(simplified a) ,(simplified b)))
    ((o a) `(,o ,(simplified a)))
    (_ expr))) ;; can there be anything else? well, apps...

(e.g. (simplified '(cdr (cons (cons 2 3) (* (- 8 7) (num x))))) ===> (* 1 (num x)))
(e.g. (simplified '(* 1 (num x))) ===> (num x))
;;; heh, fikspunkt jeszczo i tak

(define (approximate-type expr)
  (match expr
    (('car ('cons a b)) (approximate-type a))
    (('cdr ('cons a b)) (approximate-type b))
    (('+ _ _) 'num)
    (('- _ _) 'num)
    (('* _ _) 'num)
    (('quote (? symbol?)) 'sym)
    ((? var?) (car expr))
    (_ 'expr)))
;;; nooo nie wiem

(define (inlined leaf)
  (let traverse ((bnd '())
                 (code leaf))
    (match code
      ((expr) (substituted bnd expr))
      ((('let v e) . code*) (let* ((e* (simplified (substituted bnd e)))
                                   (T 'expr #;(approximate-type e*))
                                   (bnd* (update bnd `(,T ,v) e*)))
                              (traverse bnd* code*))))))

(e.g. (substituted '(((expr x) . (cons 'dupa (expr v0)))
                     ((expr y) . (car (expr x)))
                     ((expr z) . (cons (expr y) (expr y))))
             '(cons (expr z) (cons (expr x) (expr z))))
      ===> (cons (cons (expr y) (expr y))
                 (cons (cons 'dupa (expr v0))
                       (cons (expr y) (expr y)))))


(e.g. (inlined '((let x (cons 'dupa (expr v0)))
                 (let y (car (expr x)))
                 (let z (cons (expr y) (expr y)))
                 (cons (expr z) (cons (expr x) (expr z)))))
      ===> (cons (cons 'dupa 'dupa)
                 (cons (cons 'dupa (expr v0))
                       (cons 'dupa 'dupa))))
