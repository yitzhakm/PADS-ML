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
module type Plist_specs = sig
  type sep
  type term

    (** A processed version of the separator. *)
  type proc_sep
    (** A processed version of the terminator. *)
  type proc_term

  val pre_process_sep : sep -> Pads.handle -> proc_sep
  val pre_process_term : term -> Pads.handle -> proc_term

  val post_process_sep : proc_sep -> Pads.handle -> unit
  val post_process_term : proc_term -> Pads.handle -> unit

  val absorb_sep : proc_sep -> Pads.pd_header -> Pads.handle -> Pads.pd_header option
  val term_match : Pads.padsc_handle -> proc_term -> int -> Padsc.perror_t

  val print_sep : sep -> Pads.handle -> unit
  end

module type Plist_parse = sig
  type ('a_rep,'a_pdb) val_param_type      
  val parse : ('a_rep,'a_pdb) Pads.parser__ -> ('a_rep,'a_pdb) val_param_type 
    -> ('a_rep list, 'a_pdb Pads.pd list) Pads.parser__
end

module type Plist_print = sig
  type ('a_rep,'a_pdb) val_param_type
  val print_sep : ('a_rep,'a_pdb) val_param_type -> Pads.handle -> unit
end

module Make (Parse: Plist_parse) 
  (Print: Plist_print 
   with type ('a_rep,'a_pdb) val_param_type = ('a_rep,'a_pdb) Parse.val_param_type) =
struct
  type 'a_rep rep = 'a_rep list
  type 'a_pdb pd_body = 'a_pdb Pads.pd list
  type 'a_pdb pd = Pads.pd_header * 'a_pdb pd_body

  type ('a_rep,'a_pdb) val_param_type = ('a_rep,'a_pdb) Parse.val_param_type

  let gen_pd alpha_gen_pd val_param reps =
    let rec gen_pds hdr pds = function
        [] -> (hdr,pds)
      | r::rs ->
		  let (pd, hdr) = Pads.Record.gen_pd hdr (alpha_gen_pd r)
		  in gen_pds hdr (pd::pds) rs
    in
    let hdr = Pads.spanless_pd_hdr in
    let (hdr, pds) = gen_pds hdr [] reps in
      (hdr, List.rev pds)

  let parse = Parse.parse

  let print alpha_print val_param rep (hdr,elt_pds) pads = 
    let rec _print = function
	([],[]) -> ()
      | ([r],[p]) -> alpha_print r p pads
      | (r::rs,p::ps) -> (
	  alpha_print r p pads;
	  Print.print_sep val_param pads;
	  _print (rs, ps))
    in _print (rep,elt_pds)	  

  open Type.SPTraversal
  let specialize_tool alpha_specialize gen_tool = 
    let alpha_tool = alpha_specialize gen_tool in
      {
	init = gen_tool.Generic_tool.Rec_ver.list_t.Generic_tool.Rec_ver.l_init;
	traverse = fun rep (hdr, pds) state ->
	  let rec _traverse rep_list pd_list state p_state =
            match rep_list, pd_list with
				[], [] -> gen_tool.Generic_tool.Rec_ver.list_t.Generic_tool.Rec_ver.l_process_empty p_state
              | [r], [pd] ->
		  let (state, elt_s_opt) = gen_tool.Generic_tool.Rec_ver.list_t.Generic_tool.Rec_ver.l_project_next state in
		  let elt_s = match elt_s_opt with
                      Some s -> s
                    | None -> alpha_tool.init ()
		  in
		  let elt_s = alpha_tool.traverse r pd elt_s in
                    gen_tool.Generic_tool.Rec_ver.list_t.Generic_tool.Rec_ver.process_last p_state elt_s
              | r::rs, pd::pds ->
		  let (state, elt_s_opt) = gen_tool.Generic_tool.Rec_ver.list_t.Generic_tool.Rec_ver.l_project_next state in
		  let elt_s = match elt_s_opt with
                      Some s -> s
                    | None -> alpha_tool.init ()
		  in
		  let elt_s = alpha_tool.traverse r pd elt_s in
		  let p_state =
                    gen_tool.Generic_tool.Rec_ver.list_t.Generic_tool.Rec_ver.process_next p_state elt_s
		  in
		    _traverse rs pds state p_state
	  in
	  let p_state = gen_tool.Generic_tool.Rec_ver.list_t.Generic_tool.Rec_ver.l_start state hdr in
	    _traverse rep pds state p_state;
      }

  open Type.SPIOTraversal
  module GLT = Generic_lazy_tool.Rec_ver
  module GLTStream = Generic_lazy_tool.Stream
  let specialize_lazy_tool alpha_specialize gen_tool = 
    let alpha_tool = alpha_specialize gen_tool in
	let list_t = gen_tool.GLT.list_t in
      {
	init = list_t.GLT.l_init;
	traverse = fun rep (hdr, pds) state ->
	  let p_state = list_t.GLT.l_start state hdr in
	  let rec iterate rep_list pd_list state =
		match rep_list, pd_list with
			[], [] -> GLTStream.End
		  | r::rs, pd::pds ->
			  let (state, elt_s_opt) = list_t.GLT.l_project_next state in
			  let elt_s = match elt_s_opt with
				  Some s -> s
				| None -> alpha_tool.init ()
			  in
			  let elt_s' = alpha_tool.traverse r pd elt_s in
				GLTStream.More (elt_s', lazy (iterate rs pds state))
	  in
		list_t.GLT.process_stream p_state (lazy (iterate rep pds state))
      }

  open Generic_producer
  open Type.SPProducer
  let specialize_producer alpha_specialize producer =
	let alpha_producer = alpha_specialize producer in
	  {
		produce = fun source ->
		  let sources, hdr = producer.Rec_ver.process_list source in
		  let reps, sub_pds = List.split (List.map alpha_producer.produce sources) in
		  let pd = (hdr, sub_pds) in
			reps, pd
	  }


  open Generic
  module UnitGFTys = GenFunTys.Make(UnitClass)
  let tyrep alpha_tyrep = {UnitGFTys.trep = fun tool -> tool.UnitGFTys.list alpha_tyrep} 
end
