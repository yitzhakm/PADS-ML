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
type 'a plist = 'a list
type 'a_pdb plist_pd_body = 'a_pdb Pads.pd list

module Plist (Alpha : Type.S) =
struct
  type rep = Alpha.rep list
  type pd_body = Alpha.pd list
  type pd = pd_body Pads.pd

  type val_param_type = char * char

  let mklist (hd,tl) = hd::tl

  let gen_pd sep_term  reps =
    let rec gen_pds hdr pds = function
        [] -> (hdr,pds)
      | r::rs ->
	  let (pd, hdr) = Pads.Record.gen_pd hdr (Alpha.gen_pd r)
	  in gen_pds hdr (pd::pds) rs
    in
    let hdr = Pads.spanless_pd_hdr in
    let (hdr, pds) = gen_pds hdr [] reps in
      (hdr, List.rev pds)

  let parse (sep,term) pads =
    let is_done () = 
      (Padsc.p_io_at_eof (Pads.get_padsc_handle pads) = 1) ||
	match (Padsc.pchar_lit_match (Pads.get_padsc_handle pads) term 0) with
	    Padsc.P_OK -> true	      
	  | Padsc.P_ERR -> false
    in
    let rec continue prev_pos rev_reps  hdr rev_pds = 
      let current_pos = Pads.get_current_pos pads in
	if Pads.eq_pos prev_pos current_pos || is_done () 
	then (rev_reps, hdr, rev_pds)
	else 
	  let hdr = Pads.Record.absorb_next_char sep hdr pads in
          let (r_e, pd_e, hdr) = Pads.Record.parse_next Alpha.parse hdr pads in
	    continue current_pos (r_e::rev_reps) hdr (pd_e::rev_pds)
    in
    let hdr = Pads.make_empty_pd_hdr pads in
      if is_done() then ([],(Pads.Record.finish_pd_hdr hdr pads,[]))
      else
	let current_pos = Pads.get_current_pos pads in
        let (r_e, pd_e, hdr) = Pads.Record.parse_next Alpha.parse hdr pads in
	let (rev_reps, hdr, rev_pds) = 
	  continue current_pos [r_e] hdr [pd_e]
	in
	  List.rev rev_reps, 
	  (Pads.Record.finish_pd_hdr hdr pads, List.rev rev_pds)

  let print (sep,term) rep (hdr,elt_pds) pads = 
    let rec _print = function
	([],[]) -> ()
      | ([r],[p]) -> Alpha.print r p pads
      | (r::rs,p::ps) -> (
	  Alpha.print r p pads;
	  Pads.print_char_lit sep pads;
	  _print (rs, ps))
    in _print (rep,elt_pds)

  module Traverse (Tool : Generic_tool.S) =
  struct
    module AlphaTrav = Alpha.Traverse (Tool)

    let rec init () = Tool.Datatype.init ()
    let traverse rep (hdr, pds) state =
      let rec _traverse rep_list pd_list state =
	let p_state = Tool.Datatype.start state hdr in
        match rep_list, pd_list with
            [], [] ->
              let s_opt = Tool.Datatype.project state "Nil" in
              let s =
                match s_opt with
                    Some s -> s
                  | None -> Tool.Datatype.Empty.init ()
              in
              let s' = Tool.Datatype.Empty.process s in
                Tool.Datatype.process_variant p_state "Nil" s'
          | r::rs, pd::pds ->
              let s_opt = Tool.Datatype.project state "Cons" in
              let s =
                match s_opt with
                    Some s -> s
                  | None ->
                      Tool.Record.init
                        ["head",AlphaTrav.init ();
                         "tail", init ()]
              in
		(* Use the hdr for the whole list in place of the
		   (non-existent) header for the "record." *)
              let rec_p_state = Tool.Record.start s hdr in
              let head_s = Tool.Record.project s "head" in
              let head_s' = AlphaTrav.traverse r pd head_s
              in
              let rec_p_state =
                Tool.Record.process_field rec_p_state "head" head_s'
              in
              let tail_s = Tool.Record.project s "tail" in
              let tail_s' = _traverse rs pds tail_s in
              let s' =
                Tool.Record.process_last_field rec_p_state "tail" tail_s'
	      in
                Tool.Datatype.process_variant p_state "Cons" s'
      in
	_traverse rep pds state
  end


end
