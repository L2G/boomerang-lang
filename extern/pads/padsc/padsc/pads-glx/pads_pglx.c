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
#include "pglx.h"


/*
nodeRep root(void) { 
  printf("root() should never be called directly in pads_pglx.c\n"); 
  exit(-1); 
}
*/

const char *name(nodeRep x) { 
  return PGLX_generic_name(x);
}

/* This is wrong but OK for now.  We need to return text nodes
   as the children of nodes with typed content. */
const char *kind(nodeRep x) { 
  return PGLX_generic_kind(x);
}
/* *** */
atomicValue typed_value(nodeRep x) { 
  return PGLX_generic_typed_value(x); 
}

/* *** 
nodeRepArray children(nodeRep x) { 
  return PGLX_generic_children(x);
}
*/

nodeRepOpt kth_child(nodeRep x, childIndex idx){
  return PGLX_generic_kth_child(x,idx);
}

nodeRepOpt kth_child_named(nodeRep x, childIndex idx, const char *name){
  /*  error(2, "In kth_child_named %d %s\n", idx, name);  */
  return PGLX_generic_kth_child_named(x,idx,name);
}

nodeRep parent(nodeRep x) { 
  return PGLX_generic_parent(x);
}

padsNID get_id(nodeRep x){
  return PGLX_generic_get_id(x);
}

void free_node(nodeRep *x) {
  PGLX_node_free(*x);
}
