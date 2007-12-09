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
open Generic_tool.Rec_ver
open Generic_untool.Rec_ver

type 'a popt = 'a option
type 'a_pdb popt_pd_body = 'a_pdb Pads.pd option
module Popt (Alpha : Type.S) =
struct
  type rep = Alpha.rep option
  type pd_body = Alpha.pd option
  type pd = pd_body Pads.pd

  let gen_pd =
    function
        Some r ->
          Pads.Datatype.gen_pd (Alpha.gen_pd r) (fun pd -> Some pd)
      | None ->
          Pads.Datatype.gen_pd_empty None

  let parse pads =
    match Pads.Datatype.parse_variant Alpha.parse pads with
        Some (r, p) ->
	  Pads.Datatype.make_rep (Some r),
	  Pads.Datatype.make_pd (Pads.get_pd_hdr p, Some p)
      | None -> 
	  Pads.Datatype.make_rep None,
	  Pads.Datatype.make_pd (Pads.make_empty_pd_hdr pads, None)

  let print =
    function
        Some r ->
	  (function ( _, Some pd) ->
             fun pads -> Alpha.print r pd pads)
      | None ->
	  (function (_, None) ->
             fun pads -> ())

  module Traverse =
  struct
    let init tool () = tool.datatype_t.dt_init ()
    let traverse tool r (hdr, pd_body) state =
      let p_state = tool.datatype_t.dt_start state hdr in
        match r, pd_body with
            Some r, Some pd ->
              let s_opt = tool.datatype_t.dt_project state "Some" in
              let s =
                match s_opt with
                    Some s -> s
                  | None -> Alpha.Traverse.init tool ()
              in
              let s' = Alpha.Traverse.traverse tool r pd s
              in
                tool.datatype_t.process_variant p_state "Some" s'
          | None, None ->
              let s_opt = tool.datatype_t.dt_project state "None" in
              let s =
                match s_opt with
                    Some s -> s
                  | None -> tool.datatype_t.dt_init_empty ()
              in
              let s' = tool.datatype_t.dt_process_empty s in
                tool.datatype_t.process_variant p_state "None" s'
  end
  module Untraverse = struct
    let untraverse untool t = 
      match untool.processDatatype t with 
          "Some", t' -> Some (Alpha.Untraverse.untraverse untool t')
        | "None", _ -> None
        | _ -> assert false (* XXX: report an error instead *)
  end
end
