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
structure TParser : TPARSER = 
struct

  (* instantiate parser structures *)
  structure TLrVals = TLrValsFun(structure Token = LrParser.Token)

  structure TTokTable = TTokenTable(structure Tokens = TLrVals.Tokens)

  structure TLex = TLexFun(structure Tokens = TLrVals.Tokens
			   structure TTokTable = TTokTable)

  structure P = JoinWithArg(structure ParserData = TLrVals.ParserData
			    structure Lex = TLex
			    structure LrParser = LrParser)

  fun parseFile errState f = 
    let val sourceMap = SourceMap.newmap{srcFile=f}

	fun lexErr (p1, p2, msg) =
	  Error.error (errState, SourceMap.location sourceMap (p1, p2), msg)
	fun lexWarn (p1, p2, msg) =
	  Error.warning (errState, SourceMap.location sourceMap (p1, p2), msg)
	fun parseErr (msg, p1, p2) =
	  Error.error (errState, SourceMap.location sourceMap (p1, p2), msg)

	fun inputc instrm i = TextIO.inputN(instrm,i)

	val lexArg = {comLevel = ref 0,
		      sourceMap = sourceMap,
		      charlist = ref ([] : string list),
		      stringstart = ref 0,
		      errWarn = {err=lexErr, warn = lexWarn}
		      }
	val instrm = TextIO.openIn f
	val lookahead = 15

	val lexer = LrParser.Stream.streamify (TLex.makeLexer (inputc instrm) lexArg)
	val (res,_) = P.parse(lookahead, lexer, parseErr, sourceMap) 
	val _ = TextIO.closeIn instrm
     in res
    end
    handle P.ParseError =>
	(TextIO.output(Error.errStream errState,"ParseError raised\n");
	 [])

  val tFile = ref ""

  fun parseString errState s src = 
    let val _ = tFile := (OS.FileSys.tmpName () ^".p")
	val sourceMap = SourceMap.newmap{srcFile=src}
        
        val tStrm = TextIO.openOut (!tFile)
        val () = (TextIO.output(tStrm, (s^"\n")); TextIO.closeOut tStrm)

	fun lexErr (p1, p2, msg) =
	  Error.error (errState, SourceMap.location sourceMap (p1, p2), msg)
	fun lexWarn (p1, p2, msg) =
	  Error.warning (errState, SourceMap.location sourceMap (p1, p2), msg)
	fun parseErr (msg, p1, p2) =
	  Error.error (errState, SourceMap.location sourceMap (p1, p2), msg)

	fun inputc instrm i = TextIO.inputN(instrm,i)

	val lexArg = {comLevel = ref 0,
		      sourceMap = sourceMap,
		      charlist = ref ([] : string list),
		      stringstart = ref 0,
		      errWarn = {err=lexErr, warn = lexWarn}
		      }
	val instrm = TextIO.openIn (!tFile)
	val lookahead = 15

	val lexer = LrParser.Stream.streamify (TLex.makeLexer (inputc instrm) lexArg)
	val (res,_) = P.parse(lookahead, lexer, parseErr, sourceMap) 
	val _ = TextIO.closeIn instrm
	val _ =  OS.FileSys.remove (!tFile) handle e => (); 
     in res
    end
    handle P.ParseError =>
	(TextIO.output(Error.errStream errState,"ParseError raised\n");
         OS.FileSys.remove (!tFile) handle e => (); 
	 [])

end (* structure Parser *)
