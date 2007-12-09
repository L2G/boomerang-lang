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
structure Select =
struct
   structure PT = ParseTree

   datatype selectInfo = Select of {selName : string, tyName : string, args : IntInf.int list,
				    offset : IntInf.int, size : IntInf.int}

   datatype pathNode = Id of string | Dot of string | Sub of IntInf.int 
   type path = pathNode list

   fun sexprToPath (p) : path = 
       let fun cnvExp (exp, accumP) = 
	       case exp
	       of PT.Id s => (Id s):: accumP
	       |  PT.Binop (PT.Dot, e1, PT.Id f) => cnvExp(e1, (Dot f) :: accumP)
	       |  PT.Binop (PT.Sub, e1, PT.IntConst i)  => cnvExp(e1, (Sub  i) :: accumP)
	       |  p => raise Fail "Ill-formed path expression."
       in
	   cnvExp(p, [])
       end


   fun selectToString (Select {selName, tyName, args, offset, size}) =
       let fun argsToString [] = ""
             | argsToString [i] = IntInf.toString i
             | argsToString (i::is) = (IntInf.toString i)^","^(argsToString is)
       in
	   selName^"="^tyName^"(:"^(argsToString args)^":)"^"["^(IntInf.toString offset)^","^(IntInf.toString size)^"]"
       end

   fun selectListToString sl = 
       let fun h sl = 
	   case sl 
           of [] => ""
           |  [s] => (selectToString s)
           |  (s::sl) => ((selectToString s)^"|"^(h sl))
       in
	   "{"^(h sl)^"}"
       end

   fun cmp (Select {offset=offset1, size=size1,...}, Select {offset=offset2, size=size2,...})  = 
       if IntInf.< (offset1, offset2) then LESS
       else if IntInf.>(offset1, offset2) then GREATER
       else if IntInf.<(size1, size2) then LESS
       else if IntInf.>(size1, size2) then GREATER
       else EQUAL

   val selectList : selectInfo list ref = ref []

   fun insert (s : selectInfo) = 
       selectList := s::(!selectList)

   fun listSelections () = List.rev(!selectList)

   fun isSelection () = not (List.null (!selectList))
   fun reset () = selectList := nil

end