(***********************************************************************
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
*              Kathleen Fisher <kfisher@research.att.com>              *
*                 Robert Gruber <bob.gruber@gmail.com>                 *
*            Yitzhak Mandelbaum <yitzhakm@cs.princeton.edu>            *
*                                                                      *
***********************************************************************)
local 
  type 'a pp =  Tables.tidtab -> OldPrettyPrint.ppstream -> 'a -> unit

  type ('aidinfo,'a,'b) adornment_pp = ('aidinfo -> 'a) -> 'aidinfo -> 'b

  type ('pTyInfo, 'aidinfo, 'a, 'b) padornment_pp = ('pTyInfo -> 'aidinfo -> 'a) -> 'aidinfo -> 'b
in
signature PPASTPAIDADORNMENT = sig
  type aidinfo
  type paidinfo
  val ppExpressionAdornment: (aidinfo,Ast.coreExpression pp,Ast.expression pp) adornment_pp
  val ppStatementAdornment : (aidinfo,Ast.coreStatement pp,Ast.statement pp) adornment_pp
  val ppExternalDeclAdornment: string option -> paidinfo -> (* PADS *)
      (PTys.pTyInfo option, aidinfo,Ast.coreExternalDecl pp,Ast.externalDecl pp) padornment_pp
end
end
