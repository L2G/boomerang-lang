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
/*
 * linear-read nodes types.
 * 
 * Yitzhak Mandelbaum
 * AT&T Labs Research
 */

#ifndef __LINEAR_NODE_H__
#define __LINEAR_NODE_H__

typedef struct PDCI_linear_node_s PDCI_linear_node_t;

// ======================================================================
// Core Data Structures
// ======================================================================


//////////////////////////////////////
// Type: PDCI_linear_node_t
//
// Construction Notes:
//   linear array -
//        P_PS_setPartial(&(content.pd))
//   ...
//

struct PDCI_linear_node_s {
  PDCI_childIndex_t   next_idx_read;    // first unread array index

  // Data structures for reading elements

  /* ??? The mask field might be unnecessary, as read_one doesn't use it */
  void                *elt_m; 
  void                *elt_rep;
  void                *elt_pd;

  // Extra parameters for read_one function.
  void                *ro_params;
};

// ======================================================================
// Macros
// ======================================================================

#define PDCI_NEW_LINEAR_NODE(padsIN)\
  ((PDCI_linear_node_t *)calloc(1,sizeof(PDCI_linear_node_t)))

#define PDCI_INIT_LINEAR_NODE(resultIN, mIN,pdIN,repIN, ro_paramsIN) \
  do {  \
    (resultIN)->next_idx_read = 0; \
    (resultIN)->elt_m = (mIN); \
    (resultIN)->elt_pd = (pdIN); \
    (resultIN)->elt_rep = (repIN); \
    (resultIN)->ro_params = (ro_paramsIN);\
  } while (0)

#define PDCI_MK_LINEAR_NODE(resultIN, padsIN, mIN,pdIN,repIN, ro_paramsIN, whatfn) \
  do {  \
    if (!(resultIN = PDCI_NEW_LINEAR_NODE(padsIN))) { \
      failwith("PADS/Galax ALLOC_ERROR: in " whatfn); \
    } \
    PDCI_INIT_LINEAR_NODE(resultIN,mIN,pdIN,repIN, ro_paramsIN); \
  } while (0)


// ======================================================================
// Function Headers
// ======================================================================


#endif /*   __LINEAR_NODE_H__    */
