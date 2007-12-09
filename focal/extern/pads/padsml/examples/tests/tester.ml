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
exception Failure

let handle_res = function
    Pads.Ok p -> p 
  | Pads.Error -> raise Failure

let parse_source parse_fn file_name has_rec = 
  let pads = handle_res (
    if has_rec then Pads.open_handle () 
    else Pads.open_handle_norec ()
  ) in    
  let _ = handle_res (Pads.IO.open_file pads file_name) in     
  let x = parse_fn pads in
  let _ = handle_res (Pads.IO.close pads) in
  let _ = handle_res (Pads.close_handle pads) in
    x

let parse_with_from_file parse_fn file_name = parse_source parse_fn file_name true

let parse_with parse_fn = parse_source parse_fn "/dev/stdin" true

let parse_with_norec parse_fn = parse_source parse_fn "/dev/stdin" false

module Debug_test (Ty:Type.S) =
struct
  module Source = Type.Convert_type (Ty)
  module TDebug = Source.Traverse(Debug_tool)
    
  let in_stream =     
    if Array.length Sys.argv > 1 
    then Sys.argv.(1) 
    else "/dev/stdin"

  let (r,pd) = parse_source Source.parse in_stream true
  let _ = print_endline "Debug output:"
  let sDebug = TDebug.init ()
  let _ = TDebug.traverse r pd sDebug
end

module Debug_test_norec (Ty:Type.S) =
struct
  module Source = Type.Convert_type (Ty)
  module TDebug = Source.Traverse(Debug_tool)
    
  let in_stream =     
    if Array.length Sys.argv > 1 
    then Sys.argv.(1) 
    else "/dev/stdin"

  let (r,pd) = parse_source Source.parse in_stream false
  let _ = print_endline "Debug output:"
  let sDebug = TDebug.init ()
  let _ = TDebug.traverse r pd sDebug
end
