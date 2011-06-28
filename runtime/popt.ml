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
type 'a popt = 'a option
type 'a_pdb popt_pd_body = 'a_pdb Pads.pd option
module Popt =
struct
  type 'a_rep rep = 'a_rep popt
  type 'a_pdb pd_body = 'a_pdb popt_pd_body
  type 'a_pdb pd = 'a_pdb pd_body Pads.pd

  let gen_pd alpha_gen_pd =
    function
        Some r ->
          Pads.Datatype.gen_pd (alpha_gen_pd r) (fun pd -> Some pd)
      | None ->
          Pads.Datatype.gen_pd_empty None

  let parse alpha_parse pads =
    match Pads.Datatype.parse_variant alpha_parse pads with
        Some (r, p) ->
	  Pads.Datatype.make_rep (Some r),
	  Pads.Datatype.make_pd (Pads.get_pd_hdr p, Some p)
      | None -> 
	  Pads.Datatype.make_rep None,
	  Pads.Datatype.make_pd (Pads.make_empty_pd_hdr pads, None)

  let print alpha_print =
    function
        Some r ->
	  (function ( _, Some pd) ->
             fun pads -> alpha_print r pd pads)
      | None ->
	  (function (_, None) ->
            fun pads -> ())

  open Type.SPTraversal
  let specialize_tool alpha_specialize gen_tool = 
    let alpha_tool = alpha_specialize gen_tool in
      {
	init = gen_tool.Generic_tool.Rec_ver.datatype_t.Generic_tool.Rec_ver.dt_init;
	traverse = fun r (hdr, pd_body) state ->
	  let p_state = gen_tool.Generic_tool.Rec_ver.datatype_t.Generic_tool.Rec_ver.dt_start state hdr in
            match r, pd_body with
		Some r, Some pd ->
		  let s_opt = gen_tool.Generic_tool.Rec_ver.datatype_t.Generic_tool.Rec_ver.dt_project state "Some" in
		  let s =
                    match s_opt with
			Some s -> s
                      | None -> alpha_tool.init ()
		  in
		  let s' = alpha_tool.traverse r pd s
		  in
                    gen_tool.Generic_tool.Rec_ver.datatype_t.Generic_tool.Rec_ver.process_variant p_state "Some" s'
              | None, None ->
                  gen_tool.Generic_tool.Rec_ver.datatype_t.Generic_tool.Rec_ver.process_empty_variant p_state "None" ;
      }

  open Type.SPIOTraversal
  let specialize_lazy_tool alpha_specialize gen_tool = 
    let alpha_tool = alpha_specialize gen_tool in
      {
	init = gen_tool.Generic_lazy_tool.Rec_ver.datatype_t.Generic_lazy_tool.Rec_ver.dt_init;
	traverse = fun r (hdr, pd_body) state ->
	  let p_state = gen_tool.Generic_lazy_tool.Rec_ver.datatype_t.Generic_lazy_tool.Rec_ver.dt_start state hdr in
      let p_state' = match r, pd_body with
		Some r, Some pd ->
		  let s_opt = gen_tool.Generic_lazy_tool.Rec_ver.datatype_t.Generic_lazy_tool.Rec_ver.dt_project state "Some" in
		  let s =
            match s_opt with
				Some s -> s
              | None -> alpha_tool.init ()
		  in
		  let s' = lazy (alpha_tool.traverse r pd s)
		  in
            gen_tool.Generic_lazy_tool.Rec_ver.datatype_t.Generic_lazy_tool.Rec_ver.process_variant p_state "Some" s'
        | None, None ->
			let s_opt = gen_tool.Generic_lazy_tool.Rec_ver.datatype_t.Generic_lazy_tool.Rec_ver.dt_project state "None" in
			gen_tool.Generic_lazy_tool.Rec_ver.datatype_t.Generic_lazy_tool.Rec_ver.process_empty_variant p_state "None"
(*
			let s =
              match s_opt with
				  Some s -> s
				| None -> gen_tool.Generic_lazy_tool.Rec_ver.datatype_t.Generic_lazy_tool.Rec_ver.dt_init_empty ()
			in
			let s' = gen_tool.Generic_lazy_tool.Rec_ver.datatype_t.Generic_lazy_tool.Rec_ver.dt_process_empty s in
              gen_tool.Generic_lazy_tool.Rec_ver.datatype_t.Generic_lazy_tool.Rec_ver.process_variant p_state "None" s';
*)
	  in
		p_state'
      }

  open Type.SPProducer
  open Generic_producer
  let specialize_producer alpha_specialize producer =
	let alpha_producer = alpha_specialize producer in
	  {
		produce = fun source ->
		  let (rep, hdr) = producer.Rec_ver.process_datatype source in
			match rep with
				("Some", Some source) -> 
				  let rep, sub_pd = alpha_producer.produce source in
					(Some rep, (hdr, Some sub_pd))
			  | ("None", None) -> (None, (hdr, None))
			  | (s, _) -> failwith ("Producer gave the unexpected variant " ^ s ^ " to popt")
	  }

  open Generic
  module UnitGFTys = GenFunTys.Make(UnitClass)
  let tyrep alpha_tyrep = {UnitGFTys.trep = fun tool -> 
	(* rep conversion -- easy as pie *)
	let to_rep = function
		Some v -> GenFunTys.Left v
	  | None -> GenFunTys.Right ()
	in
	let from_rep = function
		GenFunTys.Left v -> Some v
	  | GenFunTys.Right () -> None
	in

	(* pd conversion -- we have to fictionalize a puint pd, though we
	   ignore it in from_pd *)
	let to_pd = function
		Some pd -> GenFunTys.Left pd
	  | None -> GenFunTys.Right (Pads.spanless_pd_hdr, ())
	in
	let from_pd = function
		GenFunTys.Left pd -> Some pd
	  | GenFunTys.Right (unit_hdr, ()) -> None
	in

	(* the actual sum tyrep *)
	let sum_tyrep = {UnitGFTys.trep = fun tool ->  
	  tool.UnitGFTys.sum 
		(GenFunTys.First_last "None") "Some" 
		alpha_tyrep false 
		Punit.tyrep true (* empty *)}
	in
	  tool.UnitGFTys.datatype sum_tyrep (to_rep, from_rep) (to_pd, from_pd)}
	  
end
