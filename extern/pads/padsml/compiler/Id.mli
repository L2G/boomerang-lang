(***********************************************************************
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
***********************************************************************)
(*
   Name: Id.mli
   Created: 12/14 2004
   Author: David Walker
*)
(* type of identifiers *)
type id
  
(* generates unique id *)
val freshid : string -> id
  
(* generates id equivalent to string arg *)       
val makeid : string -> id
  
(* eqid id1 id2 if id1 and id2 are the same identifier *)
val eqid : id -> id -> bool  
  
(* ltid id1 id2 if id1 is less than id2 *)
val ltid : id -> id -> bool
  
(* return 0 if equal; -1 if less; 1 if greater *)
val compareid : id -> id -> int
  
(* returns string representation of identifier.
   id2string (makeid s) == s
   id2string (freshid s) == s ^ "''" ^ i  for some integer i *)
val id2string : id -> string

