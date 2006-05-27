/*********************************************************/
/* The Harmony Project                                   */
/* harmony@lists.seas.upenn.edu                          */
/*                                                       */
/* omega.c - C code for Omega Library OCaml itnerface    */
/*********************************************************/
/* $Id$ */

/* --------------- includes --------------- */
extern "C" {
  
/* standard headers */
#include <stdio.h>
  
/* caml headers */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
}

/* omega headers */
#include "omega.h"

/* --------------- constants and structs --------------- */

/* garbage collection params */
#define GC_MIN 1
#define GC_MAX 5000

/* tag constants */
#define REL 1
#define AND 2
#define OR  3
#define NOT 4

/* union of omega types */
typedef union {
  Relation *relation;
  F_And *f_and;
  F_Or *f_or;
  F_Not *f_not;
} u;

/* wrapped variables */
typedef struct {
  Var_Decl *x;
} var;

/* tagged, wrapped unions */
typedef struct {
  char tag;
  u data;
} t;

/* --------------- custom ops --------------- */
int fail_compare(value v1, value v2) {
  caml_invalid_argument("omega_interface: cannot compare Omega data structures");
}
long fail_hash(value v) {
  caml_invalid_argument("omega_interface: cannot hash Omega data structures");
}
void fail_serialize(value v, unsigned long *w32, unsigned long *w64) {
  caml_invalid_argument("omega_interface: cannot serialize Omega data structures");
}
unsigned long fail_deserialize(void *dst) {
  caml_invalid_argument("omega_interface: cannot deserialize Omega data structures");
}

void finalize_value(value v) {
  t *x=(t *)Data_custom_val(v);  
  if (x != NULL && x->tag == REL) {
    Relation *r = x->data.relation;
    if (r != NULL) { 
      delete r;
    }
    x->data.relation = NULL;
  }
  return;
}

/* ops garbage collected values */
struct custom_operations ops = {
  "libomega-ocaml.t", 
  finalize_value, 
  fail_compare, 
  fail_hash, 
  fail_serialize, 
  fail_deserialize
};

/* ops for non-garbage collected values */
struct custom_operations ops_no_finalize = {
  "libomega-ocaml.t no finalization", 
  NULL,
  fail_compare, 
  fail_hash, 
  fail_serialize, 
  fail_deserialize
};

extern "C" {

/* --------------- c functions --------------- */

/* OMEGA_empty : int -> t 
 * s = OMEGA_empty(n):
 *   pre: n >= 0
 *   post: s is a new relation (set) with n output variables
 */
  value OMEGA_empty(value vs) {
    CAMLparam1(vs);
    CAMLlocal1(res);

    int vs_int = Int_val(vs);

    /* allocate res_t */
    res=alloc_custom(&ops,sizeof(t),1,GC_MAX);
    t *res_t = (t *)Data_custom_val(res);

    /* intialize res_t */
    res_t->data.relation = new Relation(vs_int);
    res_t->tag = REL;

    CAMLreturn(res);
  }

  /* OMEGA_get_var : t -> int -> var
   * x = OMEGA_get_var(s,n)
   *   pre: s is a relation with >= n+1 variables
   *   post: x points to the n+1st variable of o.
   *   note: get_var is 0-indexed, Omega structures are 1-indexed.
   */
  value OMEGA_get_var(value s, value n) {
    CAMLparam2(s,n);
    CAMLlocal1(res);

    t *s_t = (t *)Data_custom_val(s);
    
    if (s_t->tag == REL) {
      /* allocate res_x */
      res=alloc_custom(&ops_no_finalize,sizeof(var),1,GC_MAX);
      var *res_x = (var *)Data_custom_val(res);      

      /* initialize res_x 
       * note that library is 1-indexed; x is 0-indexed 
       */
      res_x->x = s_t->data.relation->set_var(Int_val(n)+1); 

      CAMLreturn(res);
    } else {
      caml_invalid_argument("get_var: first argument must be a relation");
    }
  }

  /* OMEGA_add_exists : t -> string -> (t*var)
   * (t,y) = OMEGA_add_exists(s,x)
   *   pre: none
   *   post: t is added as a new existential child of s with variable y
   */
  value OMEGA_add_exists(value s, value x) {
    CAMLparam2(s,x);
    CAMLlocal3(res,res_t,res_y);

    t *s_t = (t *)Data_custom_val(s);
    const char *x_str = (const char *)String_val(x);
    
    /* allocate components of returned tuple */
    res_t=alloc_custom(&ops_no_finalize,sizeof(t),1,GC_MAX);
    res_y=alloc_custom(&ops_no_finalize,sizeof(var),1,GC_MAX);
    t *res_t_t=(t *)Data_custom_val(res_t);    
    var *res_y_var=(var *)Data_custom_val(res_y);

    /* intialize res_t_t */
    F_Exists *f_ex = NULL;
    switch(s_t->tag) {
    case REL: f_ex = s_t->data.relation->add_exists(); break;
    case AND: f_ex = s_t->data.f_and->add_exists(); break;
    case OR : f_ex = s_t->data.f_or->add_exists(); break;
    case NOT: f_ex = s_t->data.f_not->add_exists(); break;
    }
    /* add an AND constraint; we'll need to assert that the bound
     * variable >= 0 below */
    F_And *f_a = f_ex->add_and();
    res_t_t->tag = AND;
    res_t_t->data.f_and = f_a;

    /* initialize res_y_var */
    Variable_ID var_y = f_ex->declare(x_str);
    res_y_var->x = var_y;

    /* add constraint that quantified variable >= 0 */
    GEQ_Handle geq_h = f_a->add_GEQ();
    geq_h.update_coef(var_y,1);
    geq_h.finalize();

    /* allocate and initialize return tuple */
    res = alloc_tuple(2);
    Store_field(res,0,res_t);
    Store_field(res,1,res_y);
    
    CAMLreturn(res);  
  }
  
  /* OMEGA_add_and : t -> t
   * t = OMEGA_add_and(s)
   *   pre: none
   *   post: t is added as a new conjunct below s
   */
  value OMEGA_add_and(value s) {
    CAMLparam1(s);
    CAMLlocal1(res);

    t *s_t = (t *)Data_custom_val(s);

    if (s_t->tag == AND) {
      /* return s if it's already an AND */
      CAMLreturn(s);
    } else {    
      /* otherwise add an AND child */
      
      /* allocate res_t for return */
      res=alloc_custom(&ops_no_finalize,sizeof(t),GC_MIN,GC_MAX);
      t *res_t=(t *)Data_custom_val(res);
      
      /* initialize res_t */
      F_And *f_a = NULL;
      switch(s_t->tag) {
      case REL: f_a=s_t->data.relation->add_and(); break;
      case OR : f_a=s_t->data.f_or->add_and(); break;
      case NOT: f_a=s_t->data.f_not->add_and(); break;
      }
      res_t->data.f_and=f_a;
      res_t->tag = AND;
            
      CAMLreturn(res);  
    }
  }
    
  /* OMEGA_add_or : t -> t
   * t = OMEGA_add_or(s)
   *   pre: none
   *   post: t is added as a new disjunct below s
   */
  value OMEGA_add_or(value s) {
    CAMLparam1(s);
    CAMLlocal1(res);

    t *s_t = (t *)Data_custom_val(s);

    if (s_t->tag == OR) {
      /* return s if it's already an OR */
      CAMLreturn(s);
    } else {    
      /* otherwise add an OR child */

      /* allocate res_t for return */
      res=alloc_custom(&ops_no_finalize,sizeof(t),GC_MIN,GC_MAX);
      t *res_t=(t *)Data_custom_val(res);
      
      /* initialize res_t */
      F_Or *f_o = NULL;
      switch(s_t->tag) {
      case REL: f_o=s_t->data.relation->add_or(); break;
      case AND : f_o=s_t->data.f_or->add_or(); break;
      case NOT: f_o=s_t->data.f_not->add_or(); break;
      }
      res_t->data.f_or=f_o;
      res_t->tag = OR;
            
      CAMLreturn(res);  
    }
  }

  /* OMEGA_add_not : t -> t
   * t = OMEGA_add_not(s)
   *   pre: none
   *   post: t is added as a new negation node below s
   */
  value OMEGA_add_not(value s) {
    CAMLparam1(s);
    CAMLlocal1(res);
    t *s_t = (t *)Data_custom_val(s);
    
    /* allocate res_t */
    res=alloc_custom(&ops_no_finalize,sizeof(t),GC_MIN,GC_MAX);
    t *res_t=(t *)Data_custom_val(res);

    /* initialize res_t */
    F_Not *f_n = NULL;
    switch(s_t->tag) {
    case REL: f_n=s_t->data.relation->add_not(); break;
    case AND: f_n=s_t->data.f_and->add_not(); break;
    case OR : f_n=s_t->data.f_or->add_not(); break;
    case NOT: f_n=s_t->data.f_not->add_not(); break;
    }
    res_t->data.f_not = f_n;
    res_t->tag = NOT;

    CAMLreturn(res);  
  }

  /* OMEGA_add_eq : t -> (var*int) list -> int -> unit
   * () = OMEGA_add_not(s,xicis,c)
   *   pre: xicis is a list [(x0,c0),..(xk,ck)] of var*int pairs of s
   *        where
   *          - each xi is declared by the top-level relation or
   *            bound by an existential
   *          - (not a pre-condition, but true for how we call this function)
   *            each ci is 0 or 1
   *   post: the constraint sum(ci*xi) + c = 0 is ANDed with s
   */
  value OMEGA_add_eq(value s, value xicis, value c) {
    CAMLparam3(s,xicis,c);
    CAMLlocal2(xrest,xici);

    t *s_t = (t *)Data_custom_val(s);
    
    /* initialize an F_And handle */
    /* (The Omega library only allows EQ constraints to be added to
     * AND nodes) 
     */
    F_And *f_a = NULL;    
    switch(s_t->tag) {
    case AND: f_a = s_t->data.f_and; break;
    case REL: f_a = s_t->data.relation->add_and(); break;
    case OR:  f_a = s_t->data.f_or->add_and(); break;
    case NOT: f_a = s_t->data.f_not->add_and(); break;
    }
    /* add an EQ constraint */
    EQ_Handle eq_h = f_a->add_EQ();
    
    /* loop over the list, adding constraints for each variable */
    xrest = xicis;
    while(Is_block(xrest)) {
      xici = Field(xrest,0);      
      xrest = Field(xrest,1);
      int ci = Int_val(Field(xici,0));
      var* xi_var = (var *)Data_custom_val(Field(xici, 1));
      eq_h.update_coef(xi_var->x,ci);
    }

    /* add the sum for c */
    eq_h.update_const(Int_val(c));
    eq_h.finalize ();

    CAMLreturn(Val_unit);
  }

  /* OMEGA_add_geq : t -> (var*int) list -> int -> unit
   * () = OMEGA_add_not(s,xicis,c)
   *   pre: xicis is a list [(x0,c0),..(xk,ck)] of var*int pairs of s
   *        where
   *          - each xi is declared by the top-level relation or
   *            bound by an existential
   *          - (not a pre-condition, but true for how we call this function)
   *            each ci is 0 or 1
   *   post: the constraint sum(ci*xi) + c = 0 is ANDed with s
   */
  value OMEGA_add_geq(value s, value xicis, value c) {
    CAMLparam3(s,xicis,c);
    CAMLlocal2(xrest,xici);

    t *s_t = (t *)Data_custom_val(s);
    
    /* initialize an F_And handle */
    /* (The Omega library only allows GEQ constraints to be added to
     * AND nodes) 
     */
    F_And *f_a = NULL;    
    switch(s_t->tag) {
    case AND: f_a = s_t->data.f_and; break;
    case REL: f_a = s_t->data.relation->add_and(); break;
    case OR:  f_a = s_t->data.f_or->add_and(); break;
    case NOT: f_a = s_t->data.f_not->add_and(); break;
    }
    /* add an GEQ constraint */
    GEQ_Handle geq_h = f_a->add_GEQ();
    
    /* loop over the list, adding constraints for each variable */
    xrest = xicis;
    while(Is_block(xrest)) {
      xici = Field(xrest,0);      
      xrest = Field(xrest,1);
      int ci = Int_val(Field(xici,0));
      var* xi_var = (var *)Data_custom_val(Field(xici, 1));
      geq_h.update_coef(xi_var->x,ci);
    }

    /* add the sum for c */
    geq_h.update_const(Int_val(c));
    geq_h.finalize ();

    CAMLreturn(Val_unit);
  }
  
  /* OMEGA_finalize : t -> unit
   * () = OMEGA_finalize(s)
   *   pre: none
   *   post: s is finalized 
   */
  value OMEGA_finalize(value s) {
    CAMLparam1(s);
    t *s_t = (t *)Data_custom_val(s);
    
    /* just finalize whatever structure we have */
    switch(s_t->tag) {
    case REL : s_t->data.relation->finalize(); break;
    case AND : s_t->data.f_and->finalize(); break;
    case OR  : s_t->data.f_or->finalize(); break;
    case NOT : s_t->data.f_not->finalize(); break;
    }
    CAMLreturn(Val_unit);  
  }

  /* OMEGA_satisfiable : t -> bool
   * b = OMEGA_satisfiable(s)
   *   pre: s is a top-level relation
   *   post: b is true iff t is satisfiable 
   */
  value OMEGA_satisfiable(value s) {
    CAMLparam1(s);
    t *s_t = (t *)Data_custom_val(s);
    
    switch(s_t->tag) {
    case REL: 
      if (s_t->data.relation->is_satisfiable()) CAMLreturn(Val_true);
      break;
    default:
      caml_invalid_argument("satisfiable: can only test satisfiability of a top-level relation");
      break;    
    }
    CAMLreturn(Val_false);  
  }

  /* ----------------- satisfiable at valuation ------------------ */

  /* ----- constants ----- */

  /* bpi: bits per int used in Omega.BitVector */
  static int bpi = 0;
  
  /* masks: array of bit masks where masks[i] has i set and all other bits 0 */
  static int *masks = NULL;

  /* bits_initialize()
   * pre: none
   * post: initializes bpi and masks
   */
  void bits_initialize() {
    /* initialize bpi by calling back to Omega.Valuation.get_bpi */
    bpi = Int_val(callback(*caml_named_value("Valuation.get_bpi"), Val_unit));
    
    /* allocate array for bpi entries of bit_j */
    masks = (int *)malloc(sizeof(int) * bpi);

    /* initialize entries in bit_j using shift lefts */
    int mask = 1;
    for (int j=0; j < bpi; j++) { 
      masks[j] = mask; 
      mask = mask << 1;
    }
  }
  
  /* OMEGA_fast_sat : t -> Omega.Valuation.t -> bool
   * b = OMEGA_fast_sat(s,v)
   *   pre: s is a top-level relation
   *        v is a valuation of all the variables in s
   *   post: b is true iff t is satisfiable at v
   */
  value OMEGA_fast_sat(value s, value v) {
    CAMLparam2(s,v);
    CAMLlocal5(v_bits, v_bit_data, v_int_data, v_mem_opt, v_mems_data);

    t *s_t = (t *)Data_custom_val(s);

    if (masks == NULL) { bits_initialize (); }
    
    if (s_t->tag == REL) {
      /* make a local copy of the relation */
      Relation s_copy = copy(*s_t->data.relation);
      
      /* unpack the valuation */
      v_bits = Field(v,0);
      v_int_data = Field(v,1);
      v_mem_opt = Field(v,2);
      int num_ints = Int_val(Field(v,3));

      /* unpack the bit part of the valuation */
      v_bit_data = Field(v_bits, 0);
      int num_bits = Int_val(Field(v_bits,1));

      /* unpack the mem_opt part of the valuation */
      bool check_mems = false;
      if(Is_block(v_mem_opt)) {
        check_mems = true;
        v_mems_data = Field(Field(v_mem_opt,0),0);
      } 

      /* loop over the valuation, ANDing in constraints */     
      int v_length = num_bits + num_ints;
      for (int i=0; i < v_length ; i++) {        
        if(check_mems) {
          int v_mems_idx = Int_val(Field(v_mems_data, i/bpi));
          int check_i = v_mems_idx & masks[i % bpi];          
          if (check_i == 0) { 
            /* if xi is not included in the valuation, 
             * AND in constraint that xi >= 0 and continue              
             */
            GEQ_Handle geq = s_copy.and_with_GEQ();
            geq.update_coef(s_copy.set_var(i+1),1);                       
            continue;
          }
        }
        
        /* otherwise AND in the constraint that xi=vi */
        EQ_Handle eq = s_copy.and_with_EQ();
        eq.update_coef(s_copy.set_var(i+1), 1);        
        
        /* get vi, either from bit_data or from int_data */
        /* note that vi is negated because xi=vi === xi-vi=0 */
        if(i < num_bits) {
          int v_idx = Int_val(Field(v_bit_data, i/bpi));
          int pre_res = v_idx & masks[i % bpi];

          if (pre_res == 0) { eq.update_const(0); }
          else { eq.update_const(-1); }
        } else {
          int idx = i - num_bits;
          eq.update_const (-1 * Int_val(Field(v_int_data, idx)));
        }
        eq.finalize();
      }

      /* finally, check s_copy for satisfiability */
      CAMLreturn(Val_bool(s_copy.is_satisfiable()));
    } else {
      caml_invalid_argument("fast_sat: argument must be top-level relation");
    }
  }

  /* OMEGA_print : t -> unit
   * () = OMEGA_print(s)
   *   pre: none
   *   post: a representation of s is printed to stdout
   *   note: printing may trigger simplification by the Omega Library,
   *   so printing twice may produce different output!
   */
  value OMEGA_print(value s) {
    CAMLparam1(s);
    t *s_t = (t *)Data_custom_val(s);
    
    switch(s_t->tag) {
    case REL: s_t->data.relation->print_with_subs(stdout); break;
    default:
      caml_invalid_argument("satisfiable: can only print a top-level relation");break;    
    }
    CAMLreturn(Val_unit);  
  }
}
