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
(*
   Notes: 

   - Given that its not safe to treat a pstream like any other pads
   type -- because it doesn't proactively consume its data, so you
   can't move on past it -- it should really have to be CPS style --
   i.e. take an element type and the remaining type, and not return (or,
   return once all are done.)

   If we provide langauge support for pstreams, then we can probably
   support a more natural style and then convert it to CPS style.
   That said, I see no pressing need for now to freely mix lazy and
   eager. If it turns out to be a real issue, then we can pursue it.
*)

(* FIX: need to check err results in all re alloc functions. *)

(* FIX: why not use option for these datatypes rather than special case? *)
module LazyList = struct
  type 'a node_t =
      | Empty
      | Node of 'a * 'a stream_t
  and 'a stream_t = 'a node_t lazy_t
end

module Iterator = struct
  type 'a element_t =
      | Done
      | More of 'a * 'a t
  and 'a t = unit -> 'a element_t

  let rec to_list it =
	match it () with
		Done -> []
	  | More (x, it) -> x::(to_list it)

  let rec of_list ls =
	(fun () ->
	  match ls with
		  [] -> Done
		| x::xs -> More (x, of_list xs))

  let iter f it =
    let rec _iter it =
      match it () with
	  Done -> ()
	| More (x, it) -> f x; _iter it
    in
      _iter it

  let iter2 f it1 it2 =
    let rec _iter it1 it2 =
    match (it1 (), it2 ()) with
	(Done, Done) -> ()
      | (More (x1, it1), More (x2, it2)) -> f x1 x2; _iter it1 it2
    in
      _iter it1 it2
end

open LazyList
open Iterator

type 'a pstream = 'a Iterator.t
type 'a_pdb pstream_pd_body = 'a_pdb Pads.pd Iterator.t    
 
module Pstream =
struct
  type 'a_rep rep = 'a_rep pstream
  type 'a_pdb pd_body = 'a_pdb Pads.pd pstream
  type 'a_pdb pd = Pads.pd_header * 'a_pdb pd_body

  type ('a_rep,'a_pdb) val_param_type = Plist_dt.sep_ty * ('a_rep,'a_pdb) Plist_dt.term_ty

  let gen_pd alpha_gen_pd val_param rep =
    let rec gen_pd_iterator hdr rep_iterator = 
      fun () ->
	  match rep_iterator () with
	      Done -> Done
	    | More (r,rs) -> 
		let (pd, hdr) = Pads.Record.gen_pd hdr (alpha_gen_pd r)	in
		  More (pd, gen_pd_iterator hdr rs)
    in
    let hdr = {Pads.spanless_pd_hdr
	       with Pads.state = Pads.ps_set_partial Pads.ps_init} 
    in
    let pds = gen_pd_iterator hdr rep in
      (hdr, pds)

  (* Get a unified rep + pd iterator. *)
  let parse_internal alpha_parse (sep,term) pads =
    let sep_specs = Plist_dt.get_sep_specs sep term pads in
    let term_specs = Plist_dt.get_term_specs term pads in

    let end_empty_stream () =
      sep_specs.Plist_dt.post_process_s pads;
      term_specs.Plist_dt.post_process_t pads;
      Done
    in

    (** End a stream with at least one element. *)
    let end_stream (r,p,stream_hdr) =
      sep_specs.Plist_dt.post_process_s pads;
      term_specs.Plist_dt.post_process_t pads;
      let h = Pads.Record.finish_pd_hdr stream_hdr pads in
	More((r,p,{h with Pads.state = Pads.ps_unset_partial h.Pads.state}), fun () -> Done)
    in

    let is_done elt_count reps pds = 
      (Padsc.p_io_at_eof (Pads.get_padsc_handle pads) = 1) 
      || (term_specs.Plist_dt.term_pred (Pads.get_padsc_handle pads) elt_count reps pds)
    in

    (** Parse a non-empty stream. elt_count is the number of elements parsed. 
	Counting starts at 1.
    *)
    let rec parse_nonempty prev_pos elt_count stream_hdr = fun () -> (
	let (r_e, pd_e, stream_hdr) as res = Pads.Record.parse_next alpha_parse stream_hdr pads in
	let current_pos = Pads.get_current_pos pads in
	  if Pads.eq_pos prev_pos current_pos || is_done (elt_count+1) [r_e] [pd_e] then 
	    end_stream res
	  else 
	    (* Note: see interface for explanation of optional result. *)
	    match sep_specs.Plist_dt.absorb stream_hdr pads with
		None -> end_stream res
	      | Some hdr ->
		  More (res, parse_nonempty current_pos (elt_count + 1) stream_hdr)
      )
    in
    let stream_hdr = {(Pads.make_empty_pd_hdr pads)
		    with Pads.state = Pads.ps_set_partial Pads.ps_init} 
    in
      (* Final result of parse_internal. *)

      if is_done 0 [] [] then 
	end_empty_stream
      else	    
	let current_pos = Pads.get_current_pos pads in	    
	  parse_nonempty current_pos 0 stream_hdr

  (* Get separate iterators for the rep and pd streams. Needed for compatability with other pads types*)
  let get_separate_iterators r_pd_iterator pads =
    let rec mk_stream it = 
      lazy (
	  match it () with
	      Done -> Empty
	    | More (res,it) -> Node (res, mk_stream it)
	)
    in

    (* Need to wrap in stream so that rep and pd iterators will stay in synch. *)
    let r_pd_stream = mk_stream r_pd_iterator in

    let rec mk_rep_iterator combo_stream = 
      fun () -> (
	  match Lazy.force combo_stream with
	      Empty -> Done
	    | Node ((rep,_,_),xs) -> More (rep, mk_rep_iterator xs)
	)
    in

    let rec mk_pd_iterator combo_stream = 
      fun () -> (
	  match Lazy.force combo_stream with
	      Empty -> Done
	    | Node ((_,pd,_),xs) -> More (pd, mk_pd_iterator xs)
	)
    in

    let stream_hdr = {(Pads.make_empty_pd_hdr pads)
		    with Pads.state = Pads.ps_set_partial Pads.ps_init} 
    in
      (mk_rep_iterator r_pd_stream, (stream_hdr, mk_pd_iterator r_pd_stream))

  let mk_iterator alpha_parse sep_term pads = parse_internal alpha_parse sep_term pads

  let parse alpha_parse sep_term pads = 
    let s_it = parse_internal alpha_parse sep_term pads in
      get_separate_iterators s_it pads

  let print alpha_print (sep,term) rep (hdr,elt_pds) pads = 
    let sep_specs = Plist_dt.get_sep_specs sep term pads in
    let print_sep = sep_specs.Plist_dt.print in
    let rec print_tail reps pds = 
      match (reps (), pds ()) with
	  (Done, Done) -> ()
	| (More (r,rs), More (p,ps)) -> 
	    begin		    
	      print_sep pads;
	      alpha_print r p pads;
	      print_tail rs ps
	    end
    in  
      match (rep (), elt_pds ()) with
	  (Done, Done) -> ()
	| (More(r,rs), More(p,ps)) -> 
	    begin		    
	      alpha_print r p pads;
	      print_tail rs ps
	    end

  open Type.SPTraversal
  let specialize_tool alpha_specialize gen_tool = 
    let alpha_tool = alpha_specialize gen_tool in
      {
	init = gen_tool.Generic_tool.Rec_ver.list_t.Generic_tool.Rec_ver.l_init;
	traverse = fun rep_stream (hdr, pd_stream) state ->
	  let rec traverse_tail rep_stream pd_stream state p_state prev_elt_s =
            match (rep_stream (), pd_stream ()) with
		(Done, Done) -> 
		  gen_tool.Generic_tool.Rec_ver.list_t.Generic_tool.Rec_ver.process_last p_state prev_elt_s
              | (More(r,rs), More(pd,pds)) ->
		  (* update p_state with state of previous element. *)
		  let p_state = gen_tool.Generic_tool.Rec_ver.list_t.Generic_tool.Rec_ver.process_next p_state prev_elt_s in

		  let (state, elt_s_opt) = gen_tool.Generic_tool.Rec_ver.list_t.Generic_tool.Rec_ver.l_project_next state in
		  let elt_s = match elt_s_opt with
                      Some s -> s
                    | None -> alpha_tool.init ()
		  in
		  let elt_s = alpha_tool.traverse r pd elt_s in
		    traverse_tail rs pds state p_state elt_s
	  in
	  let p_state = gen_tool.Generic_tool.Rec_ver.list_t.Generic_tool.Rec_ver.l_start state hdr in
            match (rep_stream (), pd_stream ()) with
		(Done, Done) -> gen_tool.Generic_tool.Rec_ver.list_t.Generic_tool.Rec_ver.l_process_empty p_state
	      | (More(r,rs), More(pd,pds)) ->
		  let (state, elt_s_opt) = gen_tool.Generic_tool.Rec_ver.list_t.Generic_tool.Rec_ver.l_project_next state in
		  let elt_s = match elt_s_opt with
                      Some s -> s
                    | None -> alpha_tool.init ()
		  in
		  let elt_s = alpha_tool.traverse r pd elt_s in
		    traverse_tail rs pds state p_state elt_s;
      }

  open Type.SPIOTraversal
  module GLT = Generic_lazy_tool.Rec_ver
  module GLTStream = Generic_lazy_tool.Stream
  let specialize_lazy_tool alpha_specialize gen_tool = 
    let alpha_tool = alpha_specialize gen_tool in
    let list_t = gen_tool.GLT.list_t in
      {
	init = list_t.GLT.l_init;
	traverse = fun rep_stream (hdr, pd_stream) state ->
	  let p_state = list_t.GLT.l_start state hdr in
	  let rec iterate rep_stream pd_stream state =
		match (rep_stream (), pd_stream ()) with
			(Done, Done) -> GLTStream.End
		  | (More (rep, rep_stream), More (pd, pd_stream)) ->
			  let (state, elt_s_opt) = list_t.GLT.l_project_next state in			  
			  let elt_s = match elt_s_opt with
				  Some s -> s
				| None -> alpha_tool.init ()
			  in
			  let elt_s = alpha_tool.traverse rep pd elt_s in
				GLTStream.More (elt_s, lazy (iterate rep_stream pd_stream state))
	  in
		list_t.GLT.process_stream p_state (lazy (iterate rep_stream pd_stream state))
      }

  open Type.SPProducer
  open Generic_producer
  let specialize_producer alpha_specialize producer =
	let alpha_producer = alpha_specialize producer in
	  {
		produce = fun source ->
		  let sources, hdr = producer.Rec_ver.process_list source in
		  let reps, sub_pds = List.split (List.map alpha_producer.produce sources) in
		  let pd = (hdr, Iterator.of_list sub_pds) in
			Iterator.of_list reps, pd
	  }

  open Generic
  module UnitGFTys = GenFunTys.Make(UnitClass)
  let tyrep alpha_tyrep = {UnitGFTys.trep = fun tool ->  
	let iso = (Iterator.to_list, Iterator.of_list) in
	let list_tyrep tool = tool.UnitGFTys.list alpha_tyrep in
	  tool.UnitGFTys.datatype {UnitGFTys.trep = list_tyrep} iso iso}
		
end
