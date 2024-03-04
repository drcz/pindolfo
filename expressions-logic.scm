(use-modules (grand scheme))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; this is an experiment in reasoning about s-exp's descriptions

;;; the descriptions take the following forms:
(define (valid-description? d)
  (match d
    (('CONS?) #t)
    (('NIL?) #t)
    (('ATM?) #t)
    (('SYM?) #t)
    (('NUM?) #t)
    (('EQ? (? number?)) #t)
    (('EQ? (? symbol?)) #t)
    (('not (? valid-description?)) #t)
    (_ #f)))

;;; whatever
(define (normalized d)
  (match d
    (('not ('not d*)) (normalized d*))
    (_ d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; it's not hard to figure out when two descriptions can't be
;;; simultainously satisfied:
(define ((contradicts? d0) d1)
  (match `(,d0 ,d1)
    ((d* ('not d*)) #t)
    ((('not d*) d*) #t)
    ((('CONS?) ('NIL?)) #t)
    ((('CONS?) ('ATM?)) #t)
    ((('CONS?) ('SYM?)) #t)
    ((('CONS?) ('NUM?)) #t)
    ((('CONS?) ('EQ? _)) #t)
    ((('NIL?) ('CONS?)) #t)
    ((('NIL?) ('SYM?)) #t)
    ((('NIL?) ('NUM?)) #t)
    ((('NIL?) ('EQ? ())) #f) ;; !
    ((('NIL?) ('EQ? _)) #t)
    ((('ATM?) ('CONS?)) #t)
    ((('SYM?) ('CONS?)) #t)
    ((('SYM?) ('NIL?)) #t)
    ((('SYM?) ('NUM?)) #t)
    ((('SYM?) ('EQ? (? number?))) #t)
    ((('SYM?) ('EQ? ())) #t)
    ((('NUM?) ('CONS?)) #t)
    ((('NUM?) ('NIL?)) #t)
    ((('NUM?) ('SYM?)) #t)
    ((('NUM?) ('EQ? (? symbol?))) #t)
    ((('NUM?) ('EQ? ())) #t)
    ((('EQ? x) ('EQ? x)) #f)
    ((('EQ? x) ('EQ? y)) #t)
    ((('EQ? _) ('CONS?)) #t)
    ((_ _) #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some stuff following from singleton descriptions:
(define (implies d)
  (match d
    (('CONS?) '((not (ATM?))
                (not (NIL?))
                (not (SYM?))
                (not (NUM?))))
    (('not ('CONS?)) '((ATM?)))
    (('NIL?) '((ATM?)
               (not (SYM?))
               (not (NUM?))
               (not (CONS?))))
    (('ATM?) '((not (CONS?))))
    (('not ('ATM?)) '((CONS?)))
    (('NUM?) '((ATM?)
               (not (SYM?))
               (not (NIL?))
               (not (CONS?))))
    (('SYM?) '((ATM?)
               (not (NUM?))
               (not (NIL?))
               (not (CONS?))))
    (('EQ? ()) '((ATM?)
                 (NIL?)
                 (not (SYM?))
                 (not (NUM?))
                 (not (CONS?)))) ;; shouldn't happen...?
    (('EQ? (? number?)) '((ATM?)
                          (NUM?)
                          (not (NIL?))
                          (not (SYM?))
                          (not (CONS?))))
    (('EQ? (? symbol?)) '((ATM?)
                          (SYM?)
                          (not (NIL?))
                          (not (NUM?))
                          (not (CONS?))))
    (_ '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; and the heavieast stuff; mind they're mutually exclusive:
(define (infered ds)
  (cond ((and (member? '(not (NUM?)) ds)
              (member? '(not (NIL?)) ds)
              (member? '(ATM?) ds))
         '((SYM?)))
        ((and (member? '(not (SYM?)) ds)
              (member? '(not (NIL?)) ds)
              (member? '(ATM?) ds))
         '((NUM?)))
        ((and (member? '(not (SYM?)) ds)
              (member? '(not (NUM?)) ds)
              (member? '(ATM?) ds))
         '((NIL?)))
        (else '())))

;;; one more thing could in principle be inferred given BSV but
;;; we're really not into that since pindolfina programs are now
;;; open wrt available symbols...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; now we can compute entailments (kind of):
(define (inf-closure ds)
  (let* ((ds* (apply union ds (map implies ds)))
         (ds** (union ds* (infered ds*))))
    ds**))

(e.g. (inf-closure '((not (CONS?)) (EQ? qwe)))
      ===> ((not (NUM?)) (not (NIL?)) (SYM?) (ATM?) (not (CONS?)) (EQ? qwe)))

(e.q. (inf-closure '((not (CONS?)) (not (NUM?)) (not (NIL?))))
      ===> ((SYM?) (ATM?) (not (CONS?)) (not (NUM?)) (not (NIL?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; and now ladies and gentlement, the big thing:
(define (inconsistent? ds)
  (let ((ds* (inf-closure ds)))
    (any (lambda (d) (any (contradicts? d) ds*)) ds*)))

(e.g. (inconsistent? '((CONS?) (ATM?))))
(e.g. (inconsistent? '((not (CONS?)) (not (ATM?)))))
(e.g. (inconsistent? '((CONS?) (SYM?))))
(e.g. (inconsistent? '((EQ? x) (NUM?))))
(e.g. (inconsistent? '((EQ? x) (not (ATM?)))))

;;; good night.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
