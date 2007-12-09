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
module type Types = sig
  type rep
  type pd_body
  type pd = Pads.pd_header * pd_body
end

module type Old_S = sig
  include Types

  val parse : (rep,pd_body) Pads.parser
  val print : (rep,pd_body) Pads.printer
  val gen_pd : rep -> pd

  module Traverse (Tool : Generic_tool.S) :
  sig
    val init : unit -> Tool.state
    val traverse : rep -> pd -> Tool.state -> Tool.state
  end
  module Untraverse (Untool : Generic_untool.S) : 
  sig
    val untraverse : Untool.t -> rep
  end
end

module type S = sig
  include Types

  val parse : (rep,pd_body) Pads.parser
  val print : (rep,pd_body) Pads.printer
  val gen_pd : rep -> pd
    
  module Traverse :
  sig
    val init : ('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t -> unit -> 'state
    val traverse : ('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t -> rep -> pd -> 'state -> 'state
  end

  module Untraverse :
  sig 
    val untraverse : 'a Generic_untool.Rec_ver.t -> 'a -> rep
  end
end

module type ValParam = sig
  include Types

  type val_param_type
      
  val parse : val_param_type -> (rep,pd_body) Pads.parser
  val print : val_param_type -> (rep,pd_body) Pads.printer
  val gen_pd : val_param_type -> rep -> pd
    
  module Traverse :
  sig
    val init : ('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t -> unit -> 'state
    val traverse : ('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t -> rep -> pd -> 'state -> 'state
  end
  module Untraverse :
  sig 
    val untraverse : 'a Generic_untool.Rec_ver.t -> 'a -> rep
  end  
end


module Convert_type (Ty: S) =
struct
  type rep = Ty.rep
  type pd_body = Ty.pd_body
  type pd = Ty.pd

  let parse = Ty.parse
  let print = Ty.print
  let gen_pd = Ty.gen_pd

  module Traverse (Tool:Generic_tool.S) =
  struct
    module ToolRec = Generic_tool.Rec_ver.From_mod (Tool)
    let init = Ty.Traverse.init ToolRec.tool
    let traverse = Ty.Traverse.traverse ToolRec.tool
  end       
  module Untraverse(Untool:Generic_untool.S) =
  struct
    module UntoolRec = Generic_untool.Rec_ver.From_mod(Untool)
    let untraverse = Ty.Untraverse.untraverse UntoolRec.untool
  end
end
