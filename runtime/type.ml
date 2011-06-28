(***********************************************************************
*                                                                      *
*             This software is part of the padsml package              *
*           Copyright (c) 2006-2011 AT&T Knowledge Ventures            *
*                      and is licensed under the                       *
*                        Common Public License                         *
*                      by AT&T Knowledge Ventures                      *
*                                                                      *
*                A copy of the License is available at                 *
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
*            Yitzhak Mandelbaum <yitzhak@research.att.com>             *
*                  Kenny Zhu <kzhu@cs.princeton.edu>                   *
*                                                                      *
***********************************************************************)
module type Types = sig
  type rep
  type pd_body
  type pd = Pads.pd_header * pd_body
end

module type Old_S = sig
  include Types

  val parse : (rep,pd_body) Pads.parser__
  val print : (rep,pd_body) Pads.printer
  val gen_pd : rep -> pd

  module Traverse (Tool : Generic_tool.S) :
  sig
    val init : unit -> Tool.state
    val traverse : rep -> pd -> Tool.state -> Tool.state
  end

  module Produce (Producer : Generic_producer.S) :
  sig
	val produce : Producer.source -> rep * pd
  end  
end

module type Old_Lazy_S = sig
  include Types

  val parse : (rep,pd_body) Pads.parser__
  val print : (rep,pd_body) Pads.printer
  val gen_pd : rep -> pd

  module LazyTraverse (Tool : Generic_lazy_tool.S) :
  sig
    val init : unit -> Tool.in_state
    val traverse : rep -> pd -> Tool.in_state -> Tool.out_state
  end

  module Produce (Producer : Generic_producer.S) :
  sig
	val produce : Producer.source -> rep * pd
  end
end

(** Single-pass traversal tool. *)
module SPTraversal = struct
  type ('rep, 'pd_body, 'state) tool = {
      init : unit -> 'state;
      traverse : 'rep -> 'pd_body Pads.pd -> 'state -> 'state;
    }
end

(** Single-pass traversal tool with different input and output states *)
module SPIOTraversal = struct
  type ('rep, 'pd_body, 'in_state, 'out_state) tool = {
      init : unit -> 'in_state;
      traverse : 'rep -> 'pd_body Pads.pd -> 'in_state -> 'out_state;
    }
end

(** Single-pass producer tool *)
module SPProducer = struct
  type ('source, 'rep, 'pd) producer = {
	produce : 'source -> 'rep * 'pd
  }
end

module type S = sig
  include Types
  val parse : (rep,pd_body) Pads.parser__
  val print : (rep,pd_body) Pads.printer
  val gen_pd : rep -> pd
  val specialize_tool : ('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t 
    -> (rep, pd_body, 'state) SPTraversal.tool
  val specialize_lazy_tool : ('in_state,'out_state,'rps,'dps,'cps,'lps) Generic_lazy_tool.Rec_ver.t 
    -> (rep, pd_body, 'in_state,'out_state) SPIOTraversal.tool
  val specialize_producer : 'source Generic_producer.Rec_ver.t -> ('source, rep, pd) SPProducer.producer
  (** Default type representation that can be safely cast to any other type representation. *)
  val tyrep : (rep,pd_body) Generic.GenFunTys.Make(Generic.UnitClass).type_rep
end

module type ValParam = sig
  include Types

  type val_param_type
      
  val parse : val_param_type -> (rep,pd_body) Pads.parser__
  val print : val_param_type -> (rep,pd_body) Pads.printer
  val gen_pd : val_param_type -> rep -> pd    
  val specialize_tool : ('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t 
    -> (rep, pd_body, 'state) SPTraversal.tool
  val specialize_lazy_tool : ('in_state,'out_state,'rps,'dps,'cps,'lps) Generic_lazy_tool.Rec_ver.t 
    -> (rep, pd_body, 'in_state,'out_state) SPIOTraversal.tool
  val specialize_producer : 
	'source Generic_producer.Rec_ver.t -> 
	('source, rep, pd) SPProducer.producer
  (** Default type representation that can be safely cast to any other type representation. *)
  val tyrep : (rep,pd_body) Generic.GenFunTys.Make(Generic.UnitClass).type_rep
end

module type TypeParam = sig
  type 'a_rep rep
  type 'a_pdb pd_body
  type 'a_pdb pd = Pads.pd_header * 'a_pdb pd_body
      
  val parse : ('a_rep,'a_pdb) Pads.parser__ -> ('a_rep rep, 'a_pdb pd_body) Pads.parser__
  val print : ('a_rep,'a_pdb) Pads.printer -> ('a_rep rep,'a_pdb pd_body) Pads.printer
  val gen_pd : ('a_rep -> 'a_pdb Pads.pd) -> 'a_rep rep -> 'a_pdb pd    
  val specialize_tool : (('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t 
						  -> ('a_rep, 'a_pdb, 'state) SPTraversal.tool)
    -> ('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t 
    -> ('a_rep rep, 'a_pdb pd_body, 'state) SPTraversal.tool
  val specialize_lazy_tool : (('in_state,'out_state,'rps,'dps,'cps,'lps) Generic_lazy_tool.Rec_ver.t 
							   -> ('a_rep, 'a_pdb, 'in_state,'out_state) SPIOTraversal.tool)
    -> ('in_state,'out_state,'rps,'dps,'cps,'lps) Generic_lazy_tool.Rec_ver.t 
    -> ('a_rep rep, 'a_pdb pd_body, 'in_state,'out_state) SPIOTraversal.tool
  val specialize_producer : 
	('source Generic_producer.Rec_ver.t 
	  -> ('source, 'a_rep, 'a_pd Pads.pd) SPProducer.producer)
	-> 'source Generic_producer.Rec_ver.t 
	-> ('source, 'a_rep rep, 'a_pd pd) SPProducer.producer
  (** Default type representation that can be safely cast to any other type representation. *)
  val tyrep : 
	('a_rep,'a_pdb) Generic.GenFunTys.Make(Generic.UnitClass).type_rep
	-> ('a_rep rep,'a_pdb pd_body) Generic.GenFunTys.Make(Generic.UnitClass).type_rep
end

module type TypeAndValParam = sig
  type 'a_rep rep
  type 'a_pdb pd_body
  type 'a_pdb pd = Pads.pd_header * 'a_pdb pd_body

  type ('a_rep,'a_pdb) val_param_type      
      
  val parse : ('a_rep,'a_pdb) Pads.parser__ -> ('a_rep,'a_pdb) val_param_type -> ('a_rep rep, 'a_pdb pd_body) Pads.parser__
  val print : ('a_rep,'a_pdb) Pads.printer -> ('a_rep,'a_pdb) val_param_type -> ('a_rep rep,'a_pdb pd_body) Pads.printer
  val gen_pd : ('a_rep -> 'a_pdb Pads.pd) -> ('a_rep,'a_pdb) val_param_type -> 'a_rep rep -> 'a_pdb pd    
  val specialize_tool : (('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t 
			  -> ('a_rep, 'a_pdb, 'state) SPTraversal.tool)
    -> ('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t 
    -> ('a_rep rep, 'a_pdb pd_body, 'state) SPTraversal.tool
  val specialize_lazy_tool : (('in_state,'out_state,'rps,'dps,'cps,'lps) Generic_lazy_tool.Rec_ver.t 
			  -> ('a_rep, 'a_pdb, 'in_state,'out_state) SPIOTraversal.tool)
    -> ('in_state,'out_state,'rps,'dps,'cps,'lps) Generic_lazy_tool.Rec_ver.t 
    -> ('a_rep rep, 'a_pdb pd_body, 'in_state,'out_state) SPIOTraversal.tool
  val specialize_producer : 
	('source Generic_producer.Rec_ver.t 
	  -> ('source, 'a_rep, 'a_pd Pads.pd) SPProducer.producer)
	-> 'source Generic_producer.Rec_ver.t 
	-> ('source, 'a_rep rep, 'a_pd pd) SPProducer.producer
  (** Default type representation that can be safely cast to any other type representation. *)
  val tyrep : 
	('a_rep,'a_pdb) Generic.GenFunTys.Make(Generic.UnitClass).type_rep
	-> ('a_rep rep,'a_pdb pd_body) Generic.GenFunTys.Make(Generic.UnitClass).type_rep
end

module type TwoTypesAndValParam = sig
  type ('a_rep,'b_rep) rep
  type ('a_pdb,'b_pdb) pd_body
  type ('a_pdb,'b_pdb) pd = Pads.pd_header * ('a_pdb,'b_pdb) pd_body

  type ('a_rep,'a_pdb,'b_rep,'b_pdb) val_param_type      
      
  val parse : ('a_rep,'a_pdb) Pads.parser__ -> ('b_rep,'b_pdb) Pads.parser__ -> ('a_rep,'a_pdb,'b_rep,'b_pdb) val_param_type -> (('a_rep,'b_rep) rep, ('a_pdb,'b_pdb) pd_body) Pads.parser__
  val print : ('a_rep,'a_pdb) Pads.printer -> ('b_rep,'b_pdb) Pads.printer -> ('a_rep,'a_pdb,'b_rep,'b_pdb) val_param_type -> (('a_rep,'b_rep) rep, ('a_pdb,'b_pdb) pd_body) Pads.printer
  val gen_pd : ('a_rep -> 'a_pdb Pads.pd) -> ('b_rep -> 'b_pdb Pads.pd) -> ('a_rep,'a_pdb,'b_rep,'b_pdb) val_param_type -> ('a_rep,'b_rep) rep -> ('a_pdb,'b_pdb) pd
  val specialize_tool : 
    (('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t 
      -> ('a_rep, 'a_pdb, 'state) SPTraversal.tool)
    -> (('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t 
	 -> ('b_rep, 'b_pdb, 'state) SPTraversal.tool)
    -> ('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t 
    -> (('a_rep,'b_rep) rep, ('a_pdb,'b_pdb) pd_body, 'state) SPTraversal.tool
  val specialize_lazy_tool : 
    (('in_state,'out_state,'rps,'dps,'cps,'lps) Generic_lazy_tool.Rec_ver.t 
      -> ('a_rep, 'a_pdb, 'in_state,'out_state) SPIOTraversal.tool)
    -> (('in_state,'out_state,'rps,'dps,'cps,'lps) Generic_lazy_tool.Rec_ver.t 
	 -> ('b_rep, 'b_pdb, 'in_state,'out_state) SPIOTraversal.tool)
    -> ('in_state,'out_state,'rps,'dps,'cps,'lps) Generic_lazy_tool.Rec_ver.t 
    -> (('a_rep,'b_rep) rep, ('a_pdb,'b_pdb) pd_body, 'in_state,'out_state) SPIOTraversal.tool
  val specialize_producer : 
	('source Generic_producer.Rec_ver.t 
	  -> ('source, 'a_rep, 'a_pd Pads.pd) SPProducer.producer)
	-> ('source Generic_producer.Rec_ver.t 
		 -> ('source, 'b_rep, 'b_pd Pads.pd) SPProducer.producer)
	-> 'source Generic_producer.Rec_ver.t 
	-> ('source, ('a_rep, 'b_rep) rep, ('a_pd, 'b_pd) pd) SPProducer.producer
  val tyrep : 
	('a_rep,'a_pdb) Generic.GenFunTys.Make(Generic.UnitClass).type_rep
	-> ('b_rep,'b_pdb) Generic.GenFunTys.Make(Generic.UnitClass).type_rep
	-> (('a_rep, 'b_rep) rep, ('a_pdb, 'b_pdb) pd_body) Generic.GenFunTys.Make(Generic.UnitClass).type_rep
end

module type TableTypeAndValParam = sig
  type ('a_rep,'b_rep) rep
  type ('a_rep, 'a_pdb, 'b_pdb) pd_body
  type ('a_rep, 'a_pdb, 'b_pdb) pd = Pads.pd_header * ('a_rep, 'a_pdb, 'b_pdb) pd_body

  type ('a_rep,'a_pdb,'b_rep,'b_pdb) val_param_type      
      
  val parse : ('b_rep,'b_pdb) Pads.parser__ -> ('a_rep,'a_pdb,'b_rep,'b_pdb) val_param_type -> (('a_rep,'b_rep) rep, ('a_rep, 'a_pdb, 'b_pdb) pd_body) Pads.parser__
  val print : ('b_rep,'b_pdb) Pads.printer -> ('a_rep,'a_pdb,'b_rep,'b_pdb) val_param_type -> (('a_rep,'b_rep) rep, ('a_rep, 'a_pdb, 'b_pdb) pd_body) Pads.printer
  val gen_pd : ('b_rep -> 'b_pdb Pads.pd) -> ('a_rep,'a_pdb,'b_rep,'b_pdb) val_param_type -> ('a_rep,'b_rep) rep -> ('a_rep,'a_pdb, 'b_pdb) pd
  val specialize_tool : 
    (('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t 
      -> ('a_rep, 'a_pdb, 'state) SPTraversal.tool)
    -> (('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t 
	 -> ('b_rep, 'b_pdb, 'state) SPTraversal.tool)
    -> ('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t 
    -> (('a_rep,'b_rep) rep, ('a_rep, 'a_pdb, 'b_pdb) pd_body, 'state) SPTraversal.tool
  val specialize_lazy_tool : 
    (('in_state,'out_state,'rps,'dps,'cps,'lps) Generic_lazy_tool.Rec_ver.t 
      -> ('a_rep, 'a_pdb, 'in_state,'out_state) SPIOTraversal.tool)
    -> (('in_state,'out_state,'rps,'dps,'cps,'lps) Generic_lazy_tool.Rec_ver.t 
	 -> ('b_rep, 'b_pdb, 'in_state,'out_state) SPIOTraversal.tool)
    -> ('in_state,'out_state,'rps,'dps,'cps,'lps) Generic_lazy_tool.Rec_ver.t 
    -> (('a_rep,'b_rep) rep, ('a_rep, 'a_pdb, 'b_pdb) pd_body, 'in_state,'out_state) SPIOTraversal.tool
  val specialize_producer : 
	('source Generic_producer.Rec_ver.t 
	  -> ('source, 'a_rep, 'a_pd Pads.pd) SPProducer.producer)
	-> ('source Generic_producer.Rec_ver.t 
	  -> ('source, 'b_rep, 'b_pd Pads.pd) SPProducer.producer)
	-> 'source Generic_producer.Rec_ver.t 
	-> ('source, ('a_rep, 'b_rep) rep, ('a_rep, 'a_pd, 'b_pd) pd) SPProducer.producer
  val tyrep : 
	('a_rep, 'a_pdb) Generic.GenFunTys.Make(Generic.UnitClass).type_rep
	-> ('b_rep,'b_pdb) Generic.GenFunTys.Make(Generic.UnitClass).type_rep
	-> (('a_rep, 'b_rep) rep, ('a_rep, 'a_pdb, 'b_pdb) pd_body) Generic.GenFunTys.Make(Generic.UnitClass).type_rep
  end

module type Description = sig
  module Source : S

  (**
     Indicates whether the format uses (newline-terminated) records.
  *)
  val __PML__has_records : bool
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
    let tool = Ty.specialize_tool ToolRec.tool
    let init = tool.SPTraversal.init
    let traverse = tool.SPTraversal.traverse
  end

  module Produce (Producer : Generic_producer.S) =
  struct
	module ProducerRec = Generic_producer.Rec_ver.From_mod (Producer)
	let producer = Ty.specialize_producer ProducerRec.producer
	let produce = producer.SPProducer.produce
  end
end

module Convert_lazy_type (Ty: S) =
struct
  type rep = Ty.rep
  type pd_body = Ty.pd_body
  type pd = Ty.pd

  let parse = Ty.parse
  let print = Ty.print
  let gen_pd = Ty.gen_pd

  module LazyTraverse (Tool:Generic_lazy_tool.S) =
  struct
    module ToolRec = Generic_lazy_tool.Rec_ver.From_mod (Tool)
    let tool = Ty.specialize_lazy_tool ToolRec.tool
    let init = tool.SPIOTraversal.init
    let traverse = tool.SPIOTraversal.traverse
  end       

  module Produce (Producer : Generic_producer.S) =
  struct
	module ProducerRec = Generic_producer.Rec_ver.From_mod (Producer)
	let producer = Ty.specialize_producer ProducerRec.producer
	let produce = producer.SPProducer.produce
  end
end
