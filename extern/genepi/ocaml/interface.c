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
#include <stdlib.h>
  
/* caml headers */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>

/* GENEPI headers */
#include "genepi.h"
#include "genepi-loader.h"

#define VALUE_TO_SET(s) ((genepi_set *)s)
#define SET_TO_VALUE(s) ((value)s)

/* --------------- constants and structs --------------- */

/* garbage collection params */
#define GC_MIN 1
#define GC_MAX 1000

/* --------------- custom ops --------------- */
int fail_compare(value v1, value v2) {
  caml_invalid_argument("genepi_interface: cannot compare GENEPI data structures");
}
long fail_hash(value v) {
  caml_invalid_argument("genepi_interface: cannot hash GENEPI data structures");
}
void fail_serialize(value v, unsigned long *w32, unsigned long *w64) {
  caml_invalid_argument("genepi_interface: cannot serialize GENEPI data structures");
}
unsigned long fail_deserialize(void *dst) {
  caml_invalid_argument("genepi_interface: cannot deserialize GENEPI data structures");
}

void finalize_genepi(value v) {
  return;
}

/* ops garbage collected values */
struct custom_operations ops = {
  "libgenepi-ocaml.t", 
  finalize_genepi, 
  fail_compare, 
  fail_hash, 
  fail_serialize, 
  fail_deserialize
};

/* ops for non-garbage collected values */
struct custom_operations ops_no_finalize = {
  "libgenepi-ocaml.t without finalization", 
  NULL,
  fail_compare, 
  fail_hash, 
  fail_serialize, 
  fail_deserialize
};

/* --------------- c functions --------------- */

int GENEPI_flags = 0;

#define PLUGIN_PATH "GENEPI_PLUGIN_PATH"
#define DEFAULT_ENGINE "GENEPI_DEFAULT_ENGINE"

const genepi_engine *load_engine(const char *name) {
  int num_engines = 0;
  int i = 0;
  genepi_engine **engines = genepi_loader_get_engines(&num_engines);
  for(i = 0; i < num_engines; i++) {
    if( strcmp(engines[i]->name,name) == 0 )
      return engines[i];
  }
  return NULL;
}

/* GENEPI_init : unit -> unit 
 * () = GENEPI_init():
 *   pre: none
 *   post: initialize GENEPI 
 */
value GENEPI_init(value u) { 
  CAMLparam1(u);

  const char *path = getenv(PLUGIN_PATH);
  const char *default_engine = getenv(DEFAULT_ENGINE);

  if(path == NULL) {
    fprintf(stderr, "Environment variable %s must be set\n", PLUGIN_PATH);
    exit(1);
  }

  if(default_engine == NULL) {
    fprintf(stderr, "Environment variable %s must be set\n", DEFAULT_ENGINE);
    exit(1);
  }

  genepi_loader_init();
  genepi_loader_load_directory(path);
  const genepi_engine * engine = load_engine(default_engine);
  if(engine == NULL) {
    fprintf(stderr, "Could not find a GENEPI engine\n");
    exit(1);    
  }  
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
  genepi_loader_terminate();
    
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
  
value GENEPI_is_empty(value s) { 
  CAMLparam1(s);
  if(genepi_set_is_empty(VALUE_TO_SET(s))) { 
    CAMLreturn(Val_true); 
  } else {
    CAMLreturn(Val_false);
  }
}

value GENEPI_print(value s) {
  CAMLparam1(s);
  genepi_set_display_data_structure(VALUE_TO_SET(s), stdout);
  CAMLreturn(Val_unit);
}
