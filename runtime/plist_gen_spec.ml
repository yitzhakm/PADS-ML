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
open Generic_tool.Rec_ver


(** Datatype used by list termination predicates. *)
type term_action = 
    Terminate_keep
    | Terminate_discard
    | Proceed

(* It is an error to return Terminate_discard when the list is empty *)
type ('a_rep,'a_pdb) term_pred = 'a_rep list -> 'a_pdb Pads.pd list -> term_action

module type Plist_specs = Make_plist.Plist_specs

module Make_plist(Specs: Plist_specs) =
  Make_plist.Make
    (struct
      type ('a_rep,'a_pdb) val_param_type = Specs.sep * Specs.term * ('a_rep,'a_pdb) term_pred

      let parse alpha_parse (sep,term,term_pred) pads =
	(* Perform any necessary pre-processing on the sep and term. *)
	let proc_sep = Specs.pre_process_sep sep pads in
	let proc_term = Specs.pre_process_term term pads in
	let is_done rev_reps rev_pds = 
	  (Padsc.p_io_at_eof (Pads.get_padsc_handle pads) = 1) 
	  || match (Specs.term_match (Pads.get_padsc_handle pads) proc_term 0) with
	      Padsc.P_OK -> true	      
	    | Padsc.P_ERR -> false
	in
	let rec continue prev_pos rev_reps  hdr rev_pds = 
	  let current_pos = Pads.get_current_pos pads in
	    if Pads.eq_pos prev_pos current_pos || is_done rev_reps rev_pds then 
	      (rev_reps, hdr, rev_pds)
	    else 
	      begin
		Pads.IO.checkpoint true pads;
		let spec_level = Pads.IO.get_spec_level pads in
		  (* Note: see interface for explanation of optional result. *)
		  match Specs.absorb_sep proc_sep hdr pads with
		      None -> 
			Pads.IO.restore pads;
			(rev_reps, hdr, rev_pds)
		    | Some hdr ->
			let ended, result = 
			  try 
			    let (r_e, pd_e, new_hdr) = Pads.Record.parse_next alpha_parse hdr pads in
			    let new_reps = r_e::rev_reps in
			    let new_pds = pd_e::rev_pds in
			      false, (new_reps, new_hdr, new_pds)
			  with Pads.Speculation_failure when (Pads.IO.get_spec_level pads = spec_level) ->
			    true, (rev_reps, hdr, rev_pds)
			in
			let (r_reps, r_hdr, r_pds) = result in
			let terminated = term_pred r_reps r_pds in
			if (Pads.IO.get_spec_level pads) = spec_level then
			    if ended then
			      (Pads.IO.restore pads; result)
			    else
			      match terminated with 
				  Terminate_discard -> 
				    Pads.IO.restore pads; 
				    (rev_reps,hdr,rev_pds)
				| Terminate_keep -> 
				    Pads.IO.commit pads; 
				    result
				| Proceed -> 
				    Pads.IO.commit pads;
				    continue current_pos r_reps r_hdr r_pds
			else
			  if ended then 
			    result
			  else
			    match terminated with 
				(* Discarding an element does not make sense if
				   there has been a commit in the element, so we
				   treat it the same as keep. *)
				Terminate_discard | Terminate_keep -> result
			      | Proceed -> continue current_pos r_reps r_hdr r_pds
	      end
	in
	let hdr = Pads.make_empty_pd_hdr pads in
	let (res_rep,(res_hdr,res_body)) =
	  let is_term = match term_pred [] [] with
	      Terminate_discard -> 
		Pads.Log.report_warning "Plist_gen_spec.parse" None Pads.No_info
		  (Some "Attempt by user-supplied predicated to discard element from empty list")
		  pads; true
	    | Terminate_keep -> true
	    | Proceed -> false
	  in
	    if is_term || (is_done [] []) then ([],(hdr,[]))
	    else 
	      let current_pos = Pads.get_current_pos pads in
	      let _ = Pads.IO.checkpoint true pads in
	      let spec_level = Pads.IO.get_spec_level pads in
		try 
		  let (r_e, pd_e, new_hdr) = Pads.Record.parse_next alpha_parse hdr pads in	    
		  let init_reps = [r_e] in
		  let init_pds = [pd_e] in
		  let uncommitted = (Pads.IO.get_spec_level pads) = spec_level in
		    match term_pred init_reps init_pds with 
			Terminate_discard -> 
			  if uncommitted then 
			    (Pads.IO.restore pads; ([],( hdr, [])))
			  else
			    (* Discarding an element does not make sense if
			       there has been a commit in the element, so we
			       treat it the same as keep. *)
			    (init_reps, (new_hdr, init_pds))
		      | Terminate_keep -> 
			  if uncommitted then Pads.IO.commit pads;
			  (init_reps, (new_hdr, init_pds))
		      | Proceed -> 
			  if uncommitted then Pads.IO.commit pads;
			  let (rev_reps, hdr, rev_pds) =
			    continue current_pos init_reps new_hdr init_pds 
			  in
			  List.rev rev_reps, (hdr, List.rev rev_pds)
		with Pads.Speculation_failure when Pads.IO.get_spec_level pads = spec_level ->
		  (Pads.IO.restore pads; ([],( hdr, [])))
	in
	let _ = Specs.post_process_sep proc_sep pads in
	let _ = Specs.post_process_term proc_term pads in
	  (res_rep, (Pads.Record.finish_pd_hdr res_hdr pads,res_body))

    end)
(struct
  type ('a_rep,'a_pdb) val_param_type = Specs.sep * Specs.term * ('a_rep,'a_pdb) term_pred
  let print_sep (sep,_,_) pads = Specs.print_sep sep pads    
end)
    
