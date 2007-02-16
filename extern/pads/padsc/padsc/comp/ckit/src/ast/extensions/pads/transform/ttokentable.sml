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
(***************************************************************************

  TOKEN.SML: hash table for token recognition

 ***************************************************************************)


signature TTOKENTABLE =
sig
	structure Tokens : T_TOKENS
	val checkToken : (string * int) -> (Tokens.svalue,int)Tokens.token
end

functor TTokenTable(structure Tokens : T_TOKENS): TTOKENTABLE = 
struct
  
  structure Tokens = Tokens
  type item = (int * int) -> (Tokens.svalue, int) Tokens.token
  exception Keyword
  exception LexError
  val keywords : item AtomTable.hash_table = AtomTable.mkTable(64, Keyword)
    
  val _ = let
    val insert = AtomTable.insert keywords
    fun ins (s, item) = insert (Atom.atom s, item)
  in
    app ins ([
 	      ("Output",     Tokens.OUTPUT),
 	      ("Transform",  Tokens.TRANSFORM),
 	      ("TyName",     Tokens.TYNAME),
 	      ("TyDecl",     Tokens.TYDECL),
 	      ("String",     Tokens.STR),
 	      ("IO",         Tokens.IO),
	      ("Palternate", Tokens.PALTERNATE),
	      ("Parray",     Tokens.PARRAY),
	      ("Penum",      Tokens.PENUM),
	      ("Pstruct",    Tokens.PSTRUCT),
	      ("Ptypedef",   Tokens.PTYPEDEF),
	      ("Punion",     Tokens.PUNION),
	      ("Begin",      Tokens.BEGIN),
	      ("End",        Tokens.END),
	      ("Pre",        Tokens.PRE),
	      ("Each",       Tokens.EACH),
	      ("Post",       Tokens.POST)
              ])


  end

  fun checkToken (s, pos) = let
    val endPos = pos + size s
    val name = Atom.atom s
  in
    case (AtomTable.find keywords name)
      of (SOME tokFn) => tokFn(pos, endPos)
       | NONE => Tokens.ID(s,pos,endPos)
  (* end case *)
  end

end
