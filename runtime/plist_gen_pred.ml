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

type ('a_rep,'a_pdb) term_pred = 'a_rep list -> 'a_pdb Pads.pd list -> bool

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
	  || (term_pred rev_reps rev_pds) 
	  || match (Specs.term_match (Pads.get_padsc_handle pads) proc_term 0) with
	      Padsc.P_OK -> true	      
	    | Padsc.P_ERR -> false
	in
	let rec continue prev_pos rev_reps  hdr rev_pds = 
	  let current_pos = Pads.get_current_pos pads in
	    if Pads.eq_pos prev_pos current_pos || is_done rev_reps rev_pds then 
	      (rev_reps, hdr, rev_pds)
	    else 
	      (* Note: see interface for explanation of optional result. *)
	      match Specs.absorb_sep proc_sep hdr pads with
		  None -> (rev_reps, hdr, rev_pds)
		| Some hdr ->
		    let (r_e, pd_e, hdr) = Pads.Record.parse_next alpha_parse hdr pads in
		      continue current_pos (r_e::rev_reps) hdr (pd_e::rev_pds)
	in
	let hdr = Pads.make_empty_pd_hdr pads in
	let (res_rep,(res_hdr,res_body)) =
	  if is_done [] [] then ([],(hdr,[]))
	  else
	    let current_pos = Pads.get_current_pos pads in
            let (r_e, pd_e, hdr) = Pads.Record.parse_next alpha_parse hdr pads in
	    let (rev_reps, hdr, rev_pds) = 
	      continue current_pos [r_e] hdr [pd_e]
	    in
	      List.rev rev_reps, (hdr, List.rev rev_pds)
	in
	let _ = Specs.post_process_sep proc_sep pads in
	let _ = Specs.post_process_term proc_term pads in
	  (res_rep, (Pads.Record.finish_pd_hdr res_hdr pads,res_body))
    end)
    (struct
      type ('a_rep,'a_pdb) val_param_type = Specs.sep * Specs.term * ('a_rep,'a_pdb) term_pred
      let print_sep (sep,_,_) pads = Specs.print_sep sep pads
    end)
