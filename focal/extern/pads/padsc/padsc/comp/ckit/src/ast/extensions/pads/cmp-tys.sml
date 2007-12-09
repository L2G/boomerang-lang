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
structure PTys = 

struct
  type sTyInfo = {diskSize : TyProps.diskSize,
		  memChar  : TyProps.memChar,
		  endian   : bool,
                  isRecord : bool,
		  containsRecord : bool,
		  largeHeuristic : bool,
		  labels   : TyProps.labelInfo option list,
                  fieldInfo : TyProps.fieldInfoTy list} 

  type pTyInfo = {info     : TyProps.tyInfo,
                  typarams : (string * Ast.ctype) list,
		  diskSize : TyProps.diskSize,
                  compoundDiskSize : TyProps.compoundSize,
		  memChar  : TyProps.memChar,
		  endian   : bool,
                  isRecord : bool,
		  containsRecord: bool,
	          largeHeuristic: bool,
		  isSource : bool,
		  numArgs  : int,
		  repName  : string,
		  repInit  : string option,
		  repRead  : string,
		  repClean : string option,
		  pdName   : string,
                  pdTid    : Tid.uid,
		  pdInit   : string option,
		  pdClean  : string option,
		  accName  : string,
		  accInit  : string,
		  accAdd   : string,
		  accReport: string,
		  accClean : string}

   datatype padsTy = BaseTy of PBaseTys.baseInfoTy | CompoundTy of pTyInfo | CTy


  fun getArrayInfo (tyInfo: pTyInfo) = 
     case #info tyInfo
     of TyProps.ArrayInfo ainfo => ainfo
     | _ => TyProps.defArrayInfo

  fun getUnionInfo (tyInfo: pTyInfo) = 
     case #info tyInfo
     of TyProps.UnionInfo ainfo => ainfo
     | _ => TyProps.defUnionInfo

  fun isOpt(tyInfo:pTyInfo) = 
      case #info tyInfo
      of TyProps.UnionInfo {fromOpt,...} => fromOpt
      | _ => false

  fun isSwitchedUnion(tyInfo:pTyInfo) = 
      case #info tyInfo
      of TyProps.UnionInfo {descriminator,...} => Option.isSome descriminator
      | _ => false

  fun getStructInfo (tyInfo: pTyInfo) = 
     case #info tyInfo
     of TyProps.StructInfo ainfo => ainfo
     | _ => TyProps.defStructInfo
  
  fun mergeTyInfo mergeDiskSizes (r1 : sTyInfo, r2:sTyInfo) =
      {diskSize = mergeDiskSizes (#diskSize r1, #diskSize r2),
       memChar  = TyProps.mergeMemChar(#memChar r1,   #memChar  r2),
       endian   = #endian r1 andalso #endian r2,
       isRecord = #isRecord r1 orelse #isRecord r2,
       containsRecord = #containsRecord r1 orelse #containsRecord r2,
       largeHeuristic = #largeHeuristic r1 orelse #largeHeuristic r2,
       labels = (#labels r1) @ (#labels r2),
       fieldInfo = (#fieldInfo r1) @ (#fieldInfo r2)}

  val minTyInfo : sTyInfo = 
                  {diskSize = TyProps.Size (IntInf.fromInt 0,IntInf.fromInt 0), 
                    memChar = TyProps.Static, 
                     endian = true, 
                   isRecord = false, 
             containsRecord = false,
             largeHeuristic = false, 
		     labels = [],
	          fieldInfo = []}

  type pTyMap = pTyInfo PBaseTys.PBST.map

  val pTys : pTyMap ref = ref PBaseTys.PBST.empty

  fun insert(name:Atom.atom, data:pTyInfo) = 
     pTys := PBaseTys.PBST.insert(!pTys, name, data)

  val find: Atom.atom -> pTyInfo option = fn a => PBaseTys.PBST.find(!pTys, a)

       
end