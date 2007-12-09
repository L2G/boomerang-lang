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
structure PInput = 
struct

type PinputTy = {outputAccum  : bool ref,
		 outputRead   : bool ref,
		 outputWrite  : bool ref,
		 outputXML    : bool ref,
		 outputXSchema: bool ref,
                 outputHist   : bool ref,
                 outputCluster: bool ref,
		 outputExper  : bool ref}


val inputs : PinputTy = {outputAccum  = ref true,
			 outputRead   = ref true,
			 outputWrite  = ref true,
			 outputXML    = ref false,
			 outputXSchema= ref false,
			 outputHist   = ref false,
                         outputCluster= ref false,
			 outputExper  = ref false}

fun emitAccum      status = (#outputAccum inputs) := status
fun emitRead       status = (#outputRead inputs) := status
fun emitWrite      status = (#outputWrite inputs) := status
fun emitHist       status = (#outputHist inputs) := status
fun emitCluster    status = (#outputCluster inputs) := status
fun emitXML        status = (#outputXML inputs) := status
fun emitXSchema    status = (#outputXSchema inputs) := status
fun emitExperiment status = (#outputExper inputs) := status
end
