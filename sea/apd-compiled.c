#include <stdio.h>
#include "sexp.h"
#include "parser.h"

SExp *run(SExp *init_expr) {
  SExp *var_res0;
  SExp *var_xs;
  SExp *var_x;
  SExp *var_ys;
  SExp *_view_;
  SExp *var_K;
  _view_ = init_expr;
  lbl_0:
  goto lbl_0_0;
  lbl_0_0:
  if(TYPE(_view_) == CONS) goto lbl_0_1; else goto lbl_1_0;
  lbl_0_1:
  if(are_equal(CAR(_view_),mk_sym("apd"))) goto lbl_0_2; else goto lbl_1_0;
  lbl_0_2:
  if(TYPE(CDR(_view_)) == CONS) goto lbl_0_3; else goto lbl_1_0;
  lbl_0_3:
  if(CAR(CDR(_view_)) == NIL) goto lbl_0_4; else goto lbl_1_0;
  lbl_0_4:
  if(TYPE(CDR(CDR(_view_))) == CONS) goto lbl_0_5; else goto lbl_1_0;
  lbl_0_5:
  if(TYPE(CDR(CDR(CDR(_view_)))) == CONS) goto lbl_0_6; else goto lbl_1_0;
  lbl_0_6:
  if(CDR(CDR(CDR(CDR(_view_)))) == NIL) goto lbl_0_7; else goto lbl_1_0;
  lbl_0_7:
  var_K = CAR(CDR(CDR(CDR(_view_))));
  var_ys = CAR(CDR(CDR(_view_)));
  _view_ = mk_cons(var_K,mk_cons(var_ys,NIL));
  goto lbl_0;
  lbl_1_0:
  if(TYPE(_view_) == CONS) goto lbl_1_1; else goto lbl_2_0;
  lbl_1_1:
  if(are_equal(CAR(_view_),mk_sym("apd"))) goto lbl_1_2; else goto lbl_2_0;
  lbl_1_2:
  if(TYPE(CDR(_view_)) == CONS) goto lbl_1_3; else goto lbl_2_0;
  lbl_1_3:
  if(TYPE(CAR(CDR(_view_))) == CONS) goto lbl_1_4; else goto lbl_2_0;
  lbl_1_4:
  if(TYPE(CDR(CDR(_view_))) == CONS) goto lbl_1_5; else goto lbl_2_0;
  lbl_1_5:
  if(TYPE(CDR(CDR(CDR(_view_)))) == CONS) goto lbl_1_6; else goto lbl_2_0;
  lbl_1_6:
  if(CDR(CDR(CDR(CDR(_view_)))) == NIL) goto lbl_1_7; else goto lbl_2_0;
  lbl_1_7:
  var_K = CAR(CDR(CDR(CDR(_view_))));
  var_ys = CAR(CDR(CDR(_view_)));
  var_xs = CDR(CAR(CDR(_view_)));
  var_x = CAR(CAR(CDR(_view_)));
  _view_ = mk_cons(mk_sym("apd"),mk_cons(var_xs,mk_cons(var_ys,mk_cons(mk_cons(mk_sym("KONT0"),mk_cons(mk_cons(mk_sym("apd"),mk_cons(mk_cons(var_x,var_xs),mk_cons(var_ys,NIL))),mk_cons(var_K,NIL))),NIL))));
  goto lbl_0;
  lbl_2_0:
  if(TYPE(_view_) == CONS) goto lbl_2_1; else goto lbl_3_0;
  lbl_2_1:
  if(TYPE(CAR(_view_)) == CONS) goto lbl_2_2; else goto lbl_3_0;
  lbl_2_2:
  if(are_equal(CAR(CAR(_view_)),mk_sym("KONT0"))) goto lbl_2_3; else goto lbl_3_0;
  lbl_2_3:
  if(TYPE(CDR(CAR(_view_))) == CONS) goto lbl_2_4; else goto lbl_3_0;
  lbl_2_4:
  if(TYPE(CAR(CDR(CAR(_view_)))) == CONS) goto lbl_2_5; else goto lbl_3_0;
  lbl_2_5:
  if(are_equal(CAR(CAR(CDR(CAR(_view_)))),mk_sym("apd"))) goto lbl_2_6; else goto lbl_3_0;
  lbl_2_6:
  if(TYPE(CDR(CAR(CDR(CAR(_view_))))) == CONS) goto lbl_2_7; else goto lbl_3_0;
  lbl_2_7:
  if(TYPE(CAR(CDR(CAR(CDR(CAR(_view_)))))) == CONS) goto lbl_2_8; else goto lbl_3_0;
  lbl_2_8:
  if(TYPE(CDR(CDR(CAR(CDR(CAR(_view_)))))) == CONS) goto lbl_2_9; else goto lbl_3_0;
  lbl_2_9:
  if(CDR(CDR(CDR(CAR(CDR(CAR(_view_)))))) == NIL) goto lbl_2_10; else goto lbl_3_0;
  lbl_2_10:
  if(TYPE(CDR(CDR(CAR(_view_)))) == CONS) goto lbl_2_11; else goto lbl_3_0;
  lbl_2_11:
  if(CDR(CDR(CDR(CAR(_view_)))) == NIL) goto lbl_2_12; else goto lbl_3_0;
  lbl_2_12:
  if(TYPE(CDR(_view_)) == CONS) goto lbl_2_13; else goto lbl_3_0;
  lbl_2_13:
  if(CDR(CDR(_view_)) == NIL) goto lbl_2_14; else goto lbl_3_0;
  lbl_2_14:
  var_res0 = CAR(CDR(_view_));
  var_K = CAR(CDR(CDR(CAR(_view_))));
  var_ys = CAR(CDR(CDR(CAR(CDR(CAR(_view_))))));
  var_xs = CDR(CAR(CDR(CAR(CDR(CAR(_view_))))));
  var_x = CAR(CAR(CDR(CAR(CDR(CAR(_view_))))));
  _view_ = mk_cons(var_K,mk_cons(mk_cons(var_x,var_res0),NIL));
  goto lbl_0;
  lbl_3_0:
  if(TYPE(_view_) == CONS) goto lbl_3_1; else goto lbl_4_0;
  lbl_3_1:
  if(are_equal(CAR(_view_),mk_sym("_id_"))) goto lbl_3_2; else goto lbl_4_0;
  lbl_3_2:
  if(TYPE(CDR(_view_)) == CONS) goto lbl_3_3; else goto lbl_4_0;
  lbl_3_3:
  if(CDR(CDR(_view_)) == NIL) goto lbl_3_4; else goto lbl_4_0;
  lbl_3_4:
  var_x = CAR(CDR(_view_));
  return var_x;
  lbl_4_0:
  return mk_sym("no-match");

}

void main() {
  init();
  printf("> ");  write_se(run(read_se(stdin)),stdout);
  printf("\n");
}

