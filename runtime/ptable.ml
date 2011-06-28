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
(* FIX: need to check err results in all re alloc functions. *)
(* FIX: Predicate termination is not working for table because it is currently
   sharing the sep_ty and term_ty and specs with plist_dt *)

open Plist_dt

type ('a, 'b) ptable = ('a, 'b) Hashtbl.t 
type ('a, 'a_pdb, 'b_pdb) ptable_pd_body = ('a, ('a_pdb Pads.pd * 'b_pdb Pads.pd)) Hashtbl.t

module Ptable = 
struct
  type ('a_rep, 'b_rep) rep = ('a_rep, 'b_rep) Hashtbl.t
  type ('a_rep, 'a_pdb, 'b_pdb) pd_body = ('a_rep, ('a_pdb Pads.pd * 'b_pdb Pads.pd)) Hashtbl.t
  type ('a_rep, 'a_pdb, 'b_pdb) pd = Pads.pd_header * ('a_rep, 'a_pdb, 'b_pdb) pd_body

  type ('a_rep,'a_pdb, 'b_rep, 'b_pdb) val_param_type = 
		sep_ty * ('b_rep, 'b_pdb) term_ty * ('b_rep->'a_rep) * ('b_pdb Pads.pd -> 'a_pdb Pads.pd)

  (*ignore the first argument*)
  let gen_pd alpha_gen_pd (_, _, _, get_pd) rep_table =
    let f key r (hdr, pdb_table) =
	let (pd, hdr) = Pads.Record.gen_pd hdr (alpha_gen_pd r) in
	let key_pd = get_pd pd in
	(Hashtbl.add pdb_table key (key_pd, pd);
	 (hdr, pdb_table))
    in
    let hdr = Pads.spanless_pd_hdr in
    let pdb_table = Hashtbl.create 100 in
     	Hashtbl.fold f rep_table (hdr, pdb_table)

  let parse alpha_parse (sep, term, get_key, get_pd) pads =
    let sep_specs = get_sep_specs sep term pads in
    let term_specs = get_term_specs term pads in
    (*TODO: need to parameterize the init size of the hash table*)
    let rep_table = Hashtbl.create 100 in
    let pdb_table = Hashtbl.create 100 in
    let is_done () = 
      (Padsc.p_io_at_eof (Pads.get_padsc_handle pads) = 1) 
	|| (term_specs.term_pred (Pads.get_padsc_handle pads) 0 [] [])
    in
    let rec continue prev_pos hdr = 
      let current_pos = Pads.get_current_pos pads in
        if Pads.eq_pos prev_pos current_pos || is_done () then 
          hdr
        else 
          (* Note: see interface for explanation of optional result. *)
          match sep_specs.absorb hdr pads with
              None -> hdr
            | Some hdr ->
                let (r_e, pd_e, hdr) = Pads.Record.parse_next alpha_parse hdr pads in
		let key = get_key r_e in
		let key_pd = get_pd pd_e in
		(
		  (match hdr with 
		   {Pads.error_code = Pads.Good} -> 
    	      	  	(Hashtbl.add rep_table key r_e;
    	      	  	 Hashtbl.add pdb_table key (key_pd, pd_e))
		   | _ -> if  is_done () then () else
    	      	  	(Hashtbl.add rep_table key r_e;
    	      	  	 Hashtbl.add pdb_table key (key_pd, pd_e))
		  );
    	      	  continue current_pos hdr) 
(*
	    | Some hdr ->
		Pads.Log.report_info "Ptable.parse" hdr.Pads.span
			(Pads.Padsc_error_code Padsc.P_ARRAY_SEP_ERR)
			None pads;
			hdr
*)
    in
    let hdr = Pads.make_empty_pd_hdr pads in
    let (res_rep, (res_hdr,res_body)) =
      if is_done() then (rep_table,(hdr, pdb_table))
      else
        let current_pos = Pads.get_current_pos pads in
        let (r_e, pd_e, hdr) = Pads.Record.parse_next alpha_parse hdr pads in
        let key = get_key r_e in
        let key_pd = get_pd pd_e in
        let () = Hashtbl.add rep_table key r_e in
        let () = Hashtbl.add pdb_table key (key_pd, pd_e)  in
        let hdr = 
          continue current_pos hdr
        in
          rep_table, (hdr, pdb_table)
    in
    let _ = sep_specs.post_process_s pads in
    let _ = term_specs.post_process_t pads in
	(res_rep, (Pads.Record.finish_pd_hdr res_hdr pads,res_body))

  let print alpha_print (sep, term, get_key, get_pd) rep_table (hdr, pdb_table) pads =
    let sep_specs = get_sep_specs sep term pads in
    let print_sep = sep_specs.print in
    let f key item (rep_l, pd_l) = 
	let (_, pd) = Hashtbl.find pdb_table key in
	(item::rep_l, pd::pd_l) in
    let (r_rep, r_pd) = (Hashtbl.fold f rep_table ([], [])) in
    let (reps, pds) = (List.rev r_rep, List.rev r_pd) in
    let rec _print = function
        ([],[]) -> ()
      | ([r],[p]) -> alpha_print r p pads
      | (r::rs,p::ps) -> (
          alpha_print r p pads;
          print_sep pads;
          _print (rs, ps))
    in _print (reps, pds)

  open Type.SPTraversal
  let specialize_tool _ alpha_specialize gen_tool = 
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
	  let f key item (rep_l, pd_l) = 
		let (_, pd) = Hashtbl.find pds key in
		(item::rep_l, pd::pd_l) in
	  let (replist, pdlist) = (Hashtbl.fold f rep ([], [])) in
	    _traverse (List.rev replist) (List.rev pdlist) state p_state;
      }

  open Type.SPIOTraversal
  module GLT = Generic_lazy_tool.Rec_ver
  module GLTStream = Generic_lazy_tool.Stream
  let specialize_lazy_tool _ alpha_specialize gen_tool = 
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
	  let f key item (rep_l, pd_l) = 
		let (_, pd) = Hashtbl.find pds key in
		(item::rep_l, pd::pd_l) in
	  let (replist, pdlist) = (Hashtbl.fold f rep ([], [])) in
	    list_t.GLT.process_stream p_state (lazy (iterate (List.rev replist) (List.rev pdlist) state))
      }

  open Generic_producer
  open Type.SPProducer
  let specialize_producer _ alpha_specialize producer =
	let alpha_producer = alpha_specialize producer in
	  {
		produce = fun source -> 
		(Hashtbl.create 1), 
		(Pads.spanless_pd_hdr, (Hashtbl.create 1))
(*
		produce = fun source ->
		  let f key s (rep, pdb) = 
			let (r, p) = alpha_producer.produce s in 
			let () = Hashtbl.add rep key r in
			let () = Hashtbl.add pdb key p in
			(rep, pdb) in
		  let sourcetab, hdr = producer.Rec_ver.process_table source in
		  let rep_tab = Hashtbl.create 100 in
		  let pd_tab = Hashtbl.create 100 in
		  let rep_tab, pd_table = Hashtbl.fold f sourcetab (rep_tab, pd_tab) in
		  let pd = (hdr, pd_table) in
			rep_tab, pd
*)
	  }

  open Generic
  module UnitGFTys = GenFunTys.Make(UnitClass)
  let tyrep alpha_tyrep beta_tyrep = {UnitGFTys.trep = fun tool ->
		  tool.UnitGFTys.table alpha_tyrep beta_tyrep}
end

