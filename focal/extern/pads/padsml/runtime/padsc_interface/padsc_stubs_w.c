/***********************************************************************
*                                                                      *
*             This software is part of the padsml package              *
*           Copyright (c) 2006-2007 Knowledge Ventures Corp.           *
*                         All Rights Reserved                          *
*        This software is licensed by Knowledge Ventures Corp.         *
*           under the terms and conditions of the license in           *
*                    www.padsproj.org/License.html                     *
*                                                                      *
*  This program contains certain software code or other information    *
*  ("AT&T Software") proprietary to AT&T Corp. ("AT&T").  The AT&T     *
*  Software is provided to you "AS IS". YOU ASSUME TOTAL RESPONSIBILITY*
*  AND RISK FOR USE OF THE AT&T SOFTWARE. AT&T DOES NOT MAKE, AND      *
*  EXPRESSLY DISCLAIMS, ANY EXPRESS OR IMPLIED WARRANTIES OF ANY KIND  *
*  WHATSOEVER, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF*
*  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, WARRANTIES OF  *
*  TITLE OR NON-INFRINGEMENT.  (c) AT&T Corp.  All rights              *
*  reserved.  AT&T is a registered trademark of AT&T Corp.             *
*                                                                      *
*                   Network Services Research Center                   *
*                   Knowledge Ventures Labs Research                   *
*                           Florham Park NJ                            *
*                                                                      *
*            Yitzhak Mandelbaum <yitzhak@research.att.com>>            *
*                                                                      *
***********************************************************************/
// Wrapper for padsc_stubs.c that includes pads.h before anything else
// in padsc_stubs.c. 
// Extended to include functions not found in pads.c.

#include "padsc.h"
#include "pads-internal.h"
#include "padsc_stubs.c"

/* Add dummy defintion of P_lib_init as there is no generated code to define this function.
   For more information, see pads.h. 
 */
P_NOGEN;

/* Function wrappers for macros. Prefix function name with i (for idl)*/
int iP_POS_EQ(Ppos_t first, Ppos_t second)
{return P_POS_EQ(first,second);}

int iP_POS_GT(Ppos_t first, Ppos_t second)
{return P_POS_EQ(first,second);}

/* PADS has no fclose corresponding to fopen. We hack it by adding it by hand here. */
Perror_t P_fclose(SfioPtr io){
  if (io == sfstdin || sfstdout || sfstderr)
    return P_OK; /* Do nothing. These streams can't be closed. */
  /* Otherwise, try to close it. */
  switch (sfclose(io)){
  case 0: return P_OK;
  case -1: return P_ERR;
  default: return P_ERR;
  }
}

Perror_t Pregexp_alloc(P_t *pads,Pregexp_t **regexp){
  Pregexp_t *tmp_re = (Pregexp_t *)malloc(sizeof(Pregexp_t));
  if (tmp_re == NULL)
    return P_ERR;

  tmp_re->valid = 0;

  *regexp = tmp_re;
  return P_OK;
}

Perror_t Pregexp_free(P_t *pads, Pregexp_t *regexp){
  free(regexp);
  return P_OK;
}

void P_get_error_levels(int *lev_info, int *lev_warn, int *lev_error, int *lev_fatal){
  *lev_info = P_LEV_INFO;
  *lev_warn = P_LEV_WARN;
  *lev_error = P_LEV_ERR;
  *lev_fatal = P_LEV_FATAL;
}

/* Conversion function used when Pstring was abstract.*/
/* CAMLprim value Pstring2caml_string(value v_ps) */
/* { */
/*   CAMLparam1(v_ps); */
/*   CAMLlocal1(caml_string); */
/*   char *str; */
/*   Pstring ps; */

/*   /\* Extract the Pstring from the caml value. *\/ */
/*   camlidl_ml2c_padsc_Pstring(v_ps,&ps, NULL); */

/*   caml_string = caml_alloc_string(ps.len); */
/*   str = String_val(caml_string); */
/*   memcpy(str, ps.str, ps.len); */
/*   CAMLreturn(caml_string); */
/* } */
/* static char default_s[7] = {'D','E','F','A','U','L','T'}; */
/* static Pstring default_pstring = {default_s,7,0,0}; */

/* Pstring Pstring_get_default(){return default_pstring;} */

