;;; before we materialize the grand idea of aggresive compilation
;;; some simple ``reference implementation'' will get handy.
;;; how costly is pindolf right now? should it copy'n'sweep or
;;; count references? how would i know if skipping only some tests
;;; achieves anything? what to compare to once we have some idea?

;;; the plan:
;;; -> CPSize source to avoid non-tail CALLs 4now
;;; -> compile to FCL-ish form (w/ tc-optimization)
;;; -> transpile the result to crude C
;;; and this is what this script is about...

(use-modules (grand scheme))
(add-to-load-path "./")
(define parsed (@ (pindolf parser) parsed))
(define cpsized (@ (pindolf transformations) cpsized))
(define compiled (@ (pindolf compiler) compiled))

(define test1
  '( (('apd () ?ys) ys)
     (('apd (?x . ?xs) ?ys) `(,x . ,(& (apd ,xs ,ys)))) ))

;[pretty-print(compiled(cpsized(parsed test1)))]



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nb it doesn't (and will not) have to work with arbitrary FCL*
;;; just the form we get from current compiler, with CPSized and tc
;;; (no CALLs, outermost LETs etc). EQs have const on the 2nd arg.
;;; ALSO let's assume for now there are no quoted complex espressions,
;;; that any such quote is explicitly constructed as quasi-quotes do
;;; eg '(a b c) as (CONS 'a (CONS 'b (CONS 'c ()))) m'kay?

;;; ...also no arithmetics for now XD since we don't want to deal
;;; with boxing and un-boxing numerals? or should we??


(define (str<-label l) ;;; it's a list of numbers y'know
  (string-append "lbl_" (string-join (map number->string l) "_")))
(e.g. (str<-label '(0)) ===> "lbl_0")
(e.g. (str<-label '(2 3)) ===> "lbl_2_3")

(define (str<-var l)
  (match l
    ('*VIEW* "_view_")
    (v (string-append "var_" (symbol->string v)))))

(e.g. (map str<-var '(x y z *VIEW*))
      ===> ("var_x" "var_y" "var_z" "_view_"))

(define (all-vars-in block)
  (match block
    ((('LET v e) . block*) (union `(,v) (all-vars-in block*)))
    (_ '())))

(define (all-vars #;in code)
  (apply union (map (lambda ((l . b)) (all-vars-in b)) code)))

(e.g. (all-vars (compiled(cpsized(parsed test1))))
      ===> (res0 xs x ys *VIEW* K))

(define (var-declarations vs)
  (apply string-append
         (map (lambda (v) (string-append "  SExp *" (str<-var v) ";\n"))
              vs)))

(e.g. (var-declarations '(x y z *VIEW*)) ===>
"  SExp *var_x;
  SExp *var_y;
  SExp *var_z;
  SExp *_view_;
")

(define (transpiled-term t)
  (match t
    ((? symbol? s) (str<-var s))
    (('CAR t*) (string-append "CAR(" (transpiled-term t*) ")"))
    (('CDR t*) (string-append "CDR(" (transpiled-term t*) ")"))
    (_ `(ERROR: transpiled-term ,t)))) ;; sure?
  
(e.g. (transpiled-term '(CAR (CDR (CDR *VIEW*))))
      ===> "CAR(CDR(CDR(_view_)))")

(define (transpiled-cond c)
  (match c
    (('EQ? t (? number? n))
     (string-append "NUMVAL(" (transpiled-term t) ") == "
                    (number->string n)))
    (('EQ? t ('quote (? symbol? s))) ;;; strcmp ???
     (string-append "are_equal("
                    (transpiled-term t) ","
                    "mk_sym(\"" (symbol->string s) "\")" ")"))
    (('EQ? t t*)
     (string-append "are_equal("
                    (transpiled-term t) ","
                    (transpiled-term t*)")"))
    (('NIL? t)
     (string-append (transpiled-term t) " == NIL"))
    (('NUM? t)
     (string-append "TYPE(" (transpiled-term t) ") == NUM"))
    (('SYM? t)
     (string-append "TYPE(" (transpiled-term t) ") == SYM"))
    (('CONS? t)
     (string-append "TYPE(" (transpiled-term t) ") == CONS"))
    (('ATM? t)
     (string-append "TYPE(" (transpiled-term t) ") != CONS"))
    (_ `(ERROR: transpiled-cond ,c))))

(e.g. (transpiled-cond '(CONS? (CDR (CAR *VIEW*))))
      ===> "TYPE(CDR(CAR(_view_))) == CONS")
(e.g. (transpiled-cond '(EQ? (CAR (CDR *VIEW*))
                             (CAR (CDR (CDR *VIEW*)))))
      ===> "are_equal(CAR(CDR(_view_)),CAR(CDR(CDR(_view_))))")
(e.g. (transpiled-cond '(ATM? (CDR (CDR *VIEW*))))
      ===> "TYPE(CDR(CDR(_view_))) != CONS")


(define (transpiled-exp e)
  (match e
    (() "NIL")
    ('T "mk_sym(\"T\")") ;; sure?
    ((? symbol? s) (str<-var s))
    ((? number? n)
     (string-append "mk_num(" (number->string n) ")")) ;; hmm
    (('quote (? symbol? s))
     (string-append "mk_sym(\"" (symbol->string s) "\")"))
    (('CAR e)
     (string-append "CAR(" (transpiled-exp e) ")"))
    (('CDR e)
     (string-append "CDR(" (transpiled-exp e) ")"))
    (('CONS e e*)
     (string-append "mk_cons("
                    (transpiled-exp e) ","
                    (transpiled-exp e*) ")"))
    ;;; wait these can't ever show up on the right side atm!
    #;(('EQ? e e*)
     (string-append "(are_equal("
                    (transpiled-exp e) ","
                    (transpiled-exp e*) ") ? mk_sym(\"T\") : NIL)"))
    #;(('NIL? e)
     (string-append "(" (transpiled-exp e) " == NIL ? "
                    "mk_sym(\"T\") : NIL)"))
    #;(('ATM? e)
     (string-append "(TYPE(" (transpiled-exp e) ") != CONS ? "
                    "mk_sym(\"T\") : NIL)"))
    ;;; \wait

    ;;; and these ones are absurd but we'll keep them like this for now
    ;;; though at the very least introducing constant numerals on either
    ;;; side would do much goodness... however we're aiming at BAD
    ;;; implementation in order to figure out what really matters...
    (('+ e e*)
     (string-append "mk_num(NUMVAL(" ;;; XD
                    (transpiled-exp e) ")+NUMVAL("
                    (transpiled-exp e*) "))"))
    (('- e e*)
     (string-append "mk_num(NUMVAL(" ;;; XD
                    (transpiled-exp e) ")-NUMVAL("
                    (transpiled-exp e*) "))"))
    (('* e e*)
     (string-append "mk_num(NUMVAL(" ;;; XD
                    (transpiled-exp e) ")*NUMVAL("
                    (transpiled-exp e*) "))"))))

(e.g. (transpiled-exp '(CONS 'apd (CONS xs (CONS ys ())))) ===>
      "mk_cons(mk_sym(\"apd\"),mk_cons(var_xs,mk_cons(var_ys,NIL)))")

(e.g. (transpiled-exp '(+ (* x 7) y)) ===> ;; lol
 "mk_num(NUMVAL(mk_num(NUMVAL(var_x)*NUMVAL(mk_num(7))))+NUMVAL(var_y))")


(define (transpiled-block block)
  (match block
    ((('RETURN e))
     (string-append "return " (transpiled-exp e) ";"))
    ((('GOTO l))
     (string-append "goto " (str<-label l) ";"))
    ((('IF e l0 'ELSE l1))
     (string-append "if(" (transpiled-cond e) ") "
                    "goto " (str<-label l0) "; "
                    "else goto " (str<-label l1) ";"))
    ((('LET v e) . block*)
     (string-append
      (string-append (str<-var v) " = " (transpiled-exp e) ";\n  ")
      (transpiled-block block*)))))

(e.g. (transpiled-block
       '( (LET res0 (CAR (CDR *VIEW*)))
          (LET K (CAR (CDR (CDR (CAR *VIEW*)))))
          (LET ys (CAR (CDR (CDR (CAR (CDR (CAR *VIEW*)))))))
          (LET xs (CDR (CAR (CDR (CAR (CDR (CAR *VIEW*)))))))
          (LET x (CAR (CAR (CDR (CAR (CDR (CAR *VIEW*)))))))
          (LET *VIEW* (CONS K (CONS (CONS x res0) ())))
          (GOTO (0)) ))
     ===> "var_res0 = CAR(CDR(_view_));
  var_K = CAR(CDR(CDR(CAR(_view_))));
  var_ys = CAR(CDR(CDR(CAR(CDR(CAR(_view_))))));
  var_xs = CDR(CAR(CDR(CAR(CDR(CAR(_view_))))));
  var_x = CAR(CAR(CDR(CAR(CDR(CAR(_view_))))));
  _view_ = mk_cons(var_K,mk_cons(mk_cons(var_x,var_res0),NIL));
  goto lbl_0;")

(e.g. (transpiled-block '((IF (CONS? *VIEW*) (2 1) ELSE (3 0)))) ===>
      "if(TYPE(_view_) == CONS) goto lbl_2_1; else goto lbl_3_0;")

(e.g. (transpiled-block '((LET x (CAR (CDR *VIEW*)))
                          (RETURN x)))
      ===> "var_x = CAR(CDR(_view_));
  return var_x;")


(define (transpiled-blocks bs)
  (match bs
    (() "")
    (((l . b) . bs*) (string-append "  " (str<-label l) ":\n"
                                    "  " (transpiled-block b) "\n"
                                    (transpiled-blocks bs*)))))


;(transpiled-blocks (compiled(cpsized(parsed test1))))
                                  

(define (c<-pindolf src)
  (match (parsed src)
    (('PARSE-ERROR . msg) `(PARSE ERROR! . ,msg)) ;; XD
    (src* (and-let* ((src** (cpsized src*))
                     (tgt (compiled src**))
                     (vars (all-vars #;in tgt)))                     
            (string-append
             "#include <stdio.h>\n"
             "#include \"sexp.h\"\n"
             "#include \"parser.h\"\n"
             "\n"
             "SExp *run(SExp *init_expr) {\n"
             (var-declarations vars)
             "  " (str<-var '*VIEW*) " = init_expr;\n" ; hmm
             (transpiled-blocks tgt) "\n"
             "}\n\n"
             "void main() {\n"
             "  init();\n"
             "  printf(\"> \");"
             "  write_se(run(read_se(stdin)),stdout);\n"
             "  printf(\"\\n\");\n"
             "}\n")))))

(display (c<-pindolf test1))
(newline)

;;;; omg it seems to work
