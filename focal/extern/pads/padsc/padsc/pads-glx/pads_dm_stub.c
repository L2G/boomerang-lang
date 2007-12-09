/***********************************************************************
*                                                                      *
*              This software is part of the pads package               *
*           Copyright (c) 2005-2007 AT&T Knowledge Ventures            *
*                         All Rights Reserved                          *
*         This software is licensed by AT&T Knowledge Ventures         *
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
*                          AT&T Labs Research                          *
*                           Florham Park NJ                            *
*                                                                      *
*                Mary Fernandez <mff@research.att.com>                 *
*                 Robert Gruber <bob.gruber@gmail.com>                 *
*            Yitzhak Mandelbaum <yitzhakm@cs.princeton.edu>            *
*              Kathleen Fisher <kfisher@research.att.com>              *
*                                                                      *
***********************************************************************/
#include "pads_dm.h"
#include <caml/alloc.h>
#include <caml/camlidlruntime.h>

/* 
  Hand-written stub code used by pads_c.idl.
  -- YHM.
 */

char *pads_error_string; 


/* c2ml_atomicValue(cAtomicValue *av)

   Convert a C atomicValue into a Caml atomicValue. 
   For now, we are assuming that PADS _is not_ memoizing the C atomicValue. 
*/
value c2ml_atomicValue(cAtomicValue *av) {
  item i = *(item *)av;
  value v = *i; 
  remove_global_root(i); 
  free(i);
  return v;
}

/* ml2c_atomicValue(value item, cAtomicValue *out)

   Convert a Caml atomicValue into a C atomicValue. 

   The temporary allocation is necessary so that the Caml garbage
   collector has a handle on the memory location in the C heap
   that points to the Caml object.
*/
void ml2c_atomicValue(value /* caml atomicValue */ item, cAtomicValue *out) {
  *out = malloc(sizeof(value));
  **out = item;
  register_global_root(*out);  
}

value local_camlidl_c2ml_pads_c_nodeRep(nodeRep * _c2)
{
    return camlidl_c2ml_pads_c_nodeRep(_c2,(camlidl_ctx)NULL);
  /*
value _v1;
  _v1 = camlidl_alloc((sizeof(nodeRep) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((nodeRep *) Bp_val(_v1)) = *_c2;
  return _v1;
*/
}

galax_err
padsDocument(processing_context pc, char *uri, char *psource_file, nodeRep nr, item *itp)
{
  CAMLparam0();
  CAMLlocal2(nrv, caml_result);
  int argct = 4;
  value args[argct+1];

  static value * pads_document_closure = NULL;

  if (pads_document_closure == NULL) {
    pads_document_closure = caml_named_value("pads_document");
  }

  /* The third argument is a native C value that is passed through to
     the data model constructor. */

  nrv = local_camlidl_c2ml_pads_c_nodeRep(&nr); 
  args[0] = *pc;
  args[1] = copy_string(uri);
  args[2] = copy_string(psource_file);
  args[3] = nrv;
  args[4] = (value)0;

  caml_result = callbackN_exn(*pads_document_closure, argct, args);
  /*  caml_result = callback3_exn(*pads_document_closure, *pc, copy_string(uri), nrv); */

  if (Is_exception_result(caml_result)) {
    pads_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    pads_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

galax_err
walkPadsDocument(item i)
{
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * walk_pads_document_closure = NULL;
  if (walk_pads_document_closure == NULL) {
    walk_pads_document_closure = caml_named_value("walk_pads_document");
  }
  caml_result = callback_exn(*walk_pads_document_closure, *i); 
  if (Is_exception_result(caml_result)) {
    pads_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    pads_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

void ml2c_nodeRepOpt(value input, nodeRepOpt * output)
{
value in2;
  if (input == Val_int(0)) {
    (*output) = NULL;
  } else {
    in2 = Field(input, 0);
    camlidl_ml2c_pads_c_nodeRep(in2, output, (camlidl_ctx)NULL);
  }
}

value c2ml_nodeRepOpt(nodeRepOpt * input)
{
value out;
value tmpv;
  if ((*input) == NULL) {
    out = Val_int(0);
  } else {
    tmpv = camlidl_c2ml_pads_c_nodeRep(input, (camlidl_ctx)NULL);
    Begin_root(tmpv)
      out = camlidl_alloc_small(1, 0);
      Field(out, 0) = tmpv;
    End_roots();
  }
  return out;
}
