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
*                 Robert Gruber <bob.gruber@gmail.com>                 *
*              Kathleen Fisher <kfisher@research.att.com>              *
*            Yitzhak Mandelbaum <yitzhakm@cs.princeton.edu>            *
*                                                                      *
***********************************************************************/
#ifndef __FREE_LIST_IMP_H__
#define __FREE_LIST_IMP_H__

/*
 * This file is a template. Define the types/macros below 
 * to instantiate it.
 *
 
typedef <TY> FreeList_node_t;
#define FLDEF
#define FLNEXT(n) 
#define FLFREE(n) 
#define FLCLEAR(n)
#define FLALLOC() 

*/

#include "free-list-internal.h"

// for internal use
static FreeList_node_t *FreeList_remove(FreeList_head_t *head);

void FreeList_init(FreeList_head_t *head, unsigned int max_size){
  head->list_head = (FreeList_node_t *)NULL;
  head->list_size = 0;
  head->max_size = max_size;
}

void FreeList_add(FreeList_head_t *head, FreeList_node_t *n){
  if (head->list_size < head->max_size){
    FLNEXT(n) = head->list_head;
    head->list_head = n;
    head->list_size++;
  } else {
    FLFREE(n);
  }
}

FreeList_node_t *FreeList_alloc(FreeList_head_t *head){
  FreeList_node_t *result;

  if (head->list_size > 0){
    result = FreeList_remove(head);
  }else
    result = FLALLOC();

  return result;
}

////////////////////////////////////////
// Internal use
//

static FreeList_node_t *FreeList_remove(FreeList_head_t *head){
  FreeList_node_t *result = 
    head->list_head;
  head->list_head = FLNEXT(result);
  FLCLEAR(result);
  head->list_size--;

  return result;
}

#endif /* __FREE_LIST_IMP_H__ */ 
