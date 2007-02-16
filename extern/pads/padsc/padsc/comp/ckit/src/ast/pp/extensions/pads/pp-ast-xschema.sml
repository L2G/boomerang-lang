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
  fun isPads loc = 
      case loc 
      of SourceMap.UNKNOWN => false
      |  SourceMap.LOC r => (let val fname = OS.Path.file (#srcFile r) 
				 val isPads = case OS.Path.splitBaseExt fname
				     of {base, ext = SOME extension} => extension = "p"
				     | _  => false
			     in
				 isPads
			     end)
     
  structure PPAstPaidAdornment : PPASTPAIDADORNMENT =
  struct
    type aidinfo = unit
    type paidinfo = Tables.paidtab

    fun ppExpressionAdornment ppCoreExpr aidinfo tidtab pps (Ast.EXPR (coreExpr,_,_)) = 
	ppCoreExpr aidinfo tidtab pps coreExpr

    fun ppStatementAdornment ppCoreStmt aidinfo tidtab pps  (Ast.STMT (coreStmt,_,_)) = 
	ppCoreStmt aidinfo tidtab pps coreStmt


    fun ppExternalDeclAdornment srcFileOpt paidinfo ppCoreExternalDecl aidinfo tidtab pps
	  (Ast.DECL (coreExtDecl,_,paid:Paid.uid,loc:SourceMap.location)) = 
	  if isPads loc then 
	      ppCoreExternalDecl (Paidtab.find(paidinfo,paid)) aidinfo tidtab pps coreExtDecl
	  else ()

end
in
  structure PPXSchemaAst = PPAstXschemaFn(structure PPAstPaidAdornment=PPAstPaidAdornment)
end
