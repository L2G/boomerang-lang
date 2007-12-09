/*********************************************************/
/* The Harmony Project                                   */
/* harmony@lists.seas.upenn.edu                          */
/*                                                       */
/* interface.cpp - C code for GENGEPI OCaml interface    */
/*********************************************************/
/* $Id$ */

/* --------------- includes --------------- */
/* standard headers */
#include <stdio.h>
  
/* caml headers */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>

/* GENEPI headers */
#include "genepi/genepi.h"

extern genepi_engine * genepi_plugin_init(void);

#define VALUE_TO_SET(s) ((genepi_set *)s)
#define SET_TO_VALUE(s) ((value)s)

/* --------------- constants and structs --------------- */
int GENEPI_flags = 0;

/* GENEPI_init : unit -> unit 
 * () = GENEPI_init():
 *   pre: none
 *   post: initialize GENEPI 
 */
value GENEPI_init(value u) { 
  CAMLparam1(u);
  genepi_engine * engine = genepi_plugin_init();
  genepi_set_engine(engine);
  genepi_set_init();  
  CAMLreturn(Val_unit);
}


/* GENEPI_terminate : unit -> unit 
 * () = GENEPI_terminate():
 *   pre: none
 *   post: terminate GENEPI 
 */
value GENEPI_terminate(value u) {
  CAMLparam1(u);
  genepi_set_terminate(GENEPI_flags);
  CAMLreturn(Val_unit);
}

/* --- helpers --- */
int length(value xs) {  
  CAMLlocal1(xrest);
  int xs_length=0;
  xrest = xs;
  while(Is_block(xrest)) {
    xs_length++;
    xrest = Field(xrest,1);
  }
  return xs_length;
}

int *vals_to_ints(value xs, int xs_length) {
  CAMLlocal1(xrest);
  int *xs_int = (int *) malloc(sizeof(int) * xs_length);
  int i = 0;
  xrest = xs;
  while(Is_block(xrest)) {
    xs_int[i++] = Int_val(Field(xrest,0));
    xrest = Field(xrest,1);    
  }
  return xs_int;  
}

value GENEPI_top(value n) {
  CAMLparam1(n);
  CAMLreturn(SET_TO_VALUE(genepi_set_top(Int_val(n))));
}

value GENEPI_bot(value n) {
  CAMLparam1(n);
  CAMLreturn(SET_TO_VALUE(genepi_set_bot(Int_val(n))));
}

value GENEPI_linear_constraint(value xs, value c) {
  CAMLparam2(xs,c);
  
  int xs_length = length(xs);
  int *xs_int = vals_to_ints(xs, xs_length);
  int c_int = Int_val(c);
  genepi_set *res = genepi_set_linear_equality(xs_int, xs_length, c_int);
  free(xs_int);
  CAMLreturn(SET_TO_VALUE(res));
}

value GENEPI_intersection(value s1, value s2) {
  CAMLparam2(s1,s2);
  CAMLreturn(SET_TO_VALUE(genepi_set_intersection(VALUE_TO_SET(s1), VALUE_TO_SET(s2))));
}

value GENEPI_union(value s1, value s2) {
  CAMLparam2(s1,s2);
  CAMLreturn(SET_TO_VALUE(genepi_set_union(VALUE_TO_SET(s1), VALUE_TO_SET(s2))));
}

value GENEPI_complement(value s1) {
  CAMLparam1(s1);
  CAMLreturn(SET_TO_VALUE(genepi_set_complement(VALUE_TO_SET(s1))));
}

value GENEPI_project(value s1, value xs) {
  CAMLparam2(s1,xs);
  
  int xs_length = length(xs);
  int *xs_int = vals_to_ints(xs, xs_length);
  genepi_set *res = genepi_set_project(VALUE_TO_SET(s1), xs_int, xs_length);
  
  free(xs_int);

  CAMLreturn(SET_TO_VALUE(res));
}

value GENEPI_inv_project(value s1, value xs) {
  CAMLparam2(s1,xs);
  
  int xs_length = length(xs);
  int *xs_int = vals_to_ints(xs, xs_length);
  genepi_set *res = genepi_set_invproject(VALUE_TO_SET(s1), xs_int, xs_length);
  
  free(xs_int);

  CAMLreturn(SET_TO_VALUE(res));
}

value GENEPI_is_empty(value s) { 
  CAMLparam1(s);
  if(genepi_set_is_empty(VALUE_TO_SET(s))) { 
    CAMLreturn(Val_true); 
  } else {
    CAMLreturn(Val_false);
  }
}

value GENEPI_width(value s) {
  CAMLparam1(s);
  CAMLreturn(Val_int(genepi_set_get_width(VALUE_TO_SET(s))));
}
