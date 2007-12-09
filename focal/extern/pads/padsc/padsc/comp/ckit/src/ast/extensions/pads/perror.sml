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
structure PError : PERROR =
struct
        exception PadsFail

	fun dummy (s : string) : unit = (print "Dummy Error Function.";
					 raise PadsFail)

	val   bugRef = ref dummy
	val  failRef = ref dummy
	val errorRef = ref dummy
        val  warnRef = ref dummy

	fun   bug s = ((!bugRef) s; 
		       raise PadsFail)
	fun  fail s = ((!failRef) s;
		       raise PadsFail)
	fun error s = (!errorRef) s
	fun warn s = (!warnRef) s

	fun setup errorState err warn=
	    let fun   bug' s = Error.bug errorState ("P: " ^ s)
		fun error' s = err ("(Pads) " ^ s)
		fun  fail' s = error' s
                fun warning' s = warn ("(Pads) " ^ s)
	    in
		  bugRef :=   bug';
		 failRef :=  fail';
		errorRef := error';
		 warnRef := warning';
		()
	    end
end
