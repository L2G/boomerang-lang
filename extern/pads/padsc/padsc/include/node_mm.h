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

#ifndef __NODE_MM_H__
#define __NODE_MM_H__

#define FREE_LIST_DEFAULT_MAX_SIZE 1000

/* !!! The type PDCI_node_t must be defined before including this file. !!! */

typedef struct NodeMM_s NodeMM_t;

NodeMM_t *NodeMM_newMM();

/*
 * If max_size == 0, then the default max size is used.
 */
void NodeMM_initMM(P_t *pads, unsigned int max_size);

void NodeMM_freeMM(P_t *pads);

PDCI_node_t *NodeMM_alloc(P_t *pads);

void NodeMM_free(P_t *pads, PDCI_node_t *n);

// If the array is longer than MAX_INT, then free it in pieces.
void NodeMM_freeArray(P_t *pads, PDCI_node_t **nArray, unsigned int length);

PDCI_node_t *NodeMM_get_alias(PDCI_node_t *n);

#endif /* __NODE_MM_H__ */
