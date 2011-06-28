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

type ('a_rep,'a_pdb) term_pred = 
    int (* list length *) -> 
      'a_rep list -> 'a_pdb Pads.pd list -> bool
	

type sep_ty = 
    Char_sep of char
    | String_sep of string
    | Regexp_sep of string
    | No_sep

type ('a_rep, 'a_pdb) term_ty = 
    Char_term of char
    | String_term of string
    | Regexp_term of string
    | No_sep_term (** Terminate this list when no separator is found. *)
    | Pred_term of ('a_rep, 'a_pdb) term_pred
    | Length_term of int
    | Longest_term
    | No_term     (** No terminator for this list *)    

type sep_specs = {
    post_process_s : Pads.handle -> unit;
    absorb : Pads.pd_header -> Pads.handle -> Pads.pd_header option;
    print : Pads.handle -> unit
  }

type ('a_rep,'a_pdb) term_specs = {
    post_process_t : Pads.handle -> unit;
    term_pred : Pads.padsc_handle -> int -> 'a_rep list -> 'a_pdb Pads.pd list -> bool;
  }

let no_sep_specs = {
    post_process_s = (fun pads -> ());
    absorb = (fun h pads -> Some h);
    print = (fun pads -> ());    
  }

let char_sep_specs sep = {
    post_process_s = (fun pads -> ());
    absorb = (fun h pads -> Some (Pads.Record.absorb_next_char ~lit:sep h pads));
    print = Pads.print_char_lit sep;
  }

let strict_char_sep_specs sep = {
    post_process_s = (fun pads -> ());
    absorb = 
    (fun hdr pads -> 
      match Padsc.pchar_lit_match (Pads.get_padsc_handle pads) sep 1 with
          Padsc.P_OK -> Some hdr
        | Padsc.P_ERR -> None (* No sep found, so list terminates. *)
    );
    print = Pads.print_char_lit sep;    
  }

let string_sep_specs sep = {
    post_process_s = (fun pads -> ());
    absorb = (fun h pads -> Some (Pads.Record.absorb_next_string ~lit:sep h pads));
    print = Pads.print_str_lit sep;    
  }

let strict_string_sep_specs sep = {
    post_process_s = (fun pads -> ());
    absorb = 
    (fun hdr pads -> 
      match Padsc.pstr_lit_match (Pads.get_padsc_handle pads) sep 1 with
          Padsc.P_OK -> Some hdr
        | Padsc.P_ERR -> None (* No sep found, so list terminates. *)
    );
    print = Pads.print_str_lit sep;    
  }

let regexp_sep_specs sep pads = 
  let (err,cre) = Padsc.pregexp_alloc (Pads.get_padsc_handle pads) in
  let err = Padsc.pregexp_compile_cstr (Pads.get_padsc_handle pads) sep cre in
    {
      post_process_s = (fun pads ->
	let err = Padsc.pregexp_cleanup (Pads.get_padsc_handle pads) cre in
	let err = Padsc.pregexp_free (Pads.get_padsc_handle pads) cre in
          ());
      absorb = (fun h pads -> Some (Pads.Record.absorb_next_regexp ~lit:cre h pads));
      print = Pads.print_re_lit sep;    
    }

let strict_regexp_sep_specs sep pads = 
  let (err,cre) = Padsc.pregexp_alloc (Pads.get_padsc_handle pads) in
  let err = Padsc.pregexp_compile_cstr (Pads.get_padsc_handle pads) sep cre in
    {
      post_process_s = (fun pads ->
	let err = Padsc.pregexp_cleanup (Pads.get_padsc_handle pads) cre in
	let err = Padsc.pregexp_free (Pads.get_padsc_handle pads) cre in
          ());
      absorb =
	(fun hdr pads -> 
	  match Padsc.pre_match (Pads.get_padsc_handle pads) cre 1 with
              Padsc.P_OK -> Some hdr
            | Padsc.P_ERR -> None (* No sep found, so list terminates. *)
	);
      print = Pads.print_re_lit sep;    
    }

let no_term_specs = {
    post_process_t = (fun pads -> ());
    term_pred = (fun pads n rs pds -> false);
  }

let char_term_specs term = {
    post_process_t = (fun pads -> ());
    term_pred = fun pads n rs pds ->
      match Padsc.pchar_lit_match pads term 0 with
          Padsc.P_OK -> true          
        | Padsc.P_ERR -> false
  }

let string_term_specs term = {
    post_process_t = (fun pads -> ());
    term_pred = fun pads n rs pds ->
      match Padsc.pstr_lit_match pads term 0 with
          Padsc.P_OK -> true          
        | Padsc.P_ERR -> false
  }

let regexp_term_specs term pads = 
  let (err,cre) = Padsc.pregexp_alloc (Pads.get_padsc_handle pads) in
  let err = Padsc.pregexp_compile_cstr (Pads.get_padsc_handle pads) term cre in    
    {
      post_process_t = (fun pads -> 
	let err = Padsc.pregexp_cleanup (Pads.get_padsc_handle pads) cre in
	let err = Padsc.pregexp_free (Pads.get_padsc_handle pads) cre in
          ());
      term_pred = fun pads n rs pds ->
	match Padsc.pre_match pads cre 0 with
            Padsc.P_OK -> true          
          | Padsc.P_ERR -> false
    }

let longest_term_specs = {
    post_process_t = (fun pads -> ());
    term_pred = (fun _ _ _ _ -> false);
  }

let mk_pred_term_specs p = {
    post_process_t = (fun pads -> ());
    term_pred = fun pads -> p;
  }

let get_sep_specs sep term pads =
  match term with 
      No_sep_term ->
        (match sep with
            Char_sep c -> strict_char_sep_specs c
          | String_sep s -> strict_string_sep_specs s 
          | Regexp_sep re -> strict_regexp_sep_specs re pads
              (* FIX: Should we warn user about this combo of params? *)
          | No_sep -> no_sep_specs
        )
    | _ ->
        match sep with
            Char_sep c -> char_sep_specs c
          | String_sep s -> string_sep_specs s 
          | Regexp_sep re -> regexp_sep_specs re pads
          | No_sep -> no_sep_specs

let get_term_specs term pads =
      match term with
          Char_term c -> char_term_specs c
        | Regexp_term re -> regexp_term_specs re pads
        | String_term s -> string_term_specs s
        | Pred_term p -> mk_pred_term_specs p
        | Length_term n -> mk_pred_term_specs (fun m _ _ -> m = n)
	| Longest_term -> longest_term_specs
        | No_sep_term | No_term -> no_term_specs

let is_longest = function
    Longest_term -> true | _ -> false

type 'a plist = 'a list
type 'a_pdb plist_pd_body = 'a_pdb Pads.pd list

module Plist =
struct
  type 'a_rep rep = 'a_rep list
  type 'a_pdb pd_body = 'a_pdb Pads.pd list
  type 'a_pdb pd = Pads.pd_header * 'a_pdb pd_body

  type ('a_rep,'a_pdb) val_param_type = sep_ty * ('a_rep, 'a_pdb) term_ty


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

  let parse alpha_parse (sep,term) pads =
    let sep_specs = get_sep_specs sep term pads in
    let term_specs = get_term_specs term pads in
    let is_speculative = is_longest term in
    let is_done elt_count rev_reps rev_pds =
      (Padsc.p_io_at_eof (Pads.get_padsc_handle pads) = 1) 
      || (term_specs.term_pred (Pads.get_padsc_handle pads) elt_count rev_reps rev_pds)
    in
    let rec continue prev_pos elt_count rev_reps  hdr rev_pds = 
      let current_pos = Pads.get_current_pos pads in
        if is_done elt_count rev_reps rev_pds then 
          (rev_reps, hdr, rev_pds)
        else if Pads.eq_pos prev_pos current_pos then 
	  begin
	    Pads.Log.report_info "Plist_dt.parse" (Pads.finish_span hdr.Pads.span pads)
	      (Pads.Padsc_error_code Padsc.P_ARRAY_ELEM_ERR)
	      (Some "Terminating for lack of progress") pads;
            (rev_reps, hdr, rev_pds)
	  end
        else
          (* Note: see interface for explanation of optional result. *)
          match sep_specs.absorb hdr pads with
              None -> (rev_reps, hdr, rev_pds)
            | Some hdr ->
                let (r_e, pd_e, hdr) = Pads.Record.parse_next alpha_parse hdr pads in
		(match hdr with
		     {Pads.error_code = Pads.Good} -> ()
		       | _ -> 
			   Pads.Log.report_warning "Plist_dt.parse" (Pads.finish_span hdr.Pads.span pads)
			     (Pads.Padsc_error_code Padsc.P_ARRAY_ELEM_ERR) None pads);
		continue current_pos (elt_count + 1) (r_e::rev_reps) hdr (pd_e::rev_pds) 
    in


    let rec continue_spec prev_pos elt_count rev_reps  hdr rev_pds = 
      let current_pos = Pads.get_current_pos pads in
        if is_done elt_count rev_reps rev_pds then 
          (rev_reps, hdr, rev_pds)
        else if Pads.eq_pos prev_pos current_pos then 
	  begin
	    Pads.Log.report_info "Plist_dt.parse" (Pads.finish_span hdr.Pads.span pads)
	      (Pads.Padsc_error_code Padsc.P_ARRAY_ELEM_ERR)
	      (Some "Terminating for lack of progress") pads;
            (rev_reps, hdr, rev_pds)
	  end
        else
	  begin	    
	    Pads.IO.checkpoint true pads;
	    (* We check the spec level to know whether a subcomponent has "committed" to the current parse,
	       overriding the speculative nature of the parse.
	    *)
	    let spec_level = Pads.IO.get_spec_level pads in
(* 	    Printf.eprintf "Attempting next element. spec_level = %d. Elements = %d\n" spec_level elt_count; *)
	    
            (* Note: see interface for explanation of optional result. *)
	    let sep_result = try sep_specs.absorb hdr pads with
		Pads.Speculation_failure when (Pads.IO.get_spec_level pads = spec_level) -> None
	    in
            match sep_result with
		None -> 
		  Pads.IO.restore pads;
		  (rev_reps, hdr, rev_pds)
            | Some hdr ->
(* 		Printf.eprintf "Parsed sep. Spec level = %d. Elements = %d\n" spec_level elt_count; *)
		let ended, elt_count, rs, hdr, pds =
		  try
                    let (r_e, pd_e, hdr) = Pads.Record.parse_next alpha_parse hdr pads in
(* 		    Printf.eprintf "Parsed element. Actual spec level = %d. spec_level = %d. Elements = %d\n" (Pads.IO.get_spec_level pads) spec_level elt_count; *)
		    if Pads.pd_is_ok pd_e then
		      begin
			if (Pads.IO.get_spec_level pads) = spec_level then
			  Pads.IO.commit pads;		      
			false, elt_count + 1, r_e::rev_reps, hdr, pd_e::rev_pds
		      end
		    else if Pads.IO.get_spec_level pads <> spec_level then
		      (* A commit has already occured. Therefore,
			 discarding the invalid element does not make
			 sense, so we terminate and keep the
			 element. *)
		      true, elt_count + 1, r_e::rev_reps, hdr, pd_e::rev_pds
		    else
		      begin
			Pads.IO.restore pads;
			true, elt_count, rev_reps, hdr, rev_pds
		      end
		  with 
		      Pads.Speculation_failure when (Pads.IO.get_spec_level pads = spec_level) ->
			Pads.IO.restore pads;
			true, elt_count, rev_reps, hdr, rev_pds
(* 		    | Pads.Speculation_failure ->  *)
(* 			Printf.eprintf "Actual spec level = %d. Local level = %d. Elements = %d\n" (Pads.IO.get_spec_level pads) spec_level elt_count; *)
(* 			raise Pads.Speculation_failure *)
		in
		if ended then
		  (rs, hdr, pds)
		else 
		  continue_spec current_pos elt_count rs hdr pds
	  end
    in     

    let hdr = Pads.make_empty_pd_hdr pads in
    let (res_rep,(res_hdr,res_body)) =
      if is_done 0 [] [] then ([],(hdr,[]))
      else
        let current_pos = Pads.get_current_pos pads in
	if is_speculative then
	  begin
	    Pads.IO.checkpoint true pads;
	    let spec_level = Pads.IO.get_spec_level pads in
	    try 
              let (r_e, pd_e, hdr) = Pads.Record.parse_next alpha_parse hdr pads in
(* 	      Printf.eprintf "Parsed element. Actual spec level = %d. spec_level = %d. Elements = 0\n" (Pads.IO.get_spec_level pads) spec_level; *)
	      if Pads.pd_is_ok pd_e then
		begin
		  if (Pads.IO.get_spec_level pads) = spec_level then
		    Pads.IO.commit pads;
		  let (rev_reps, hdr, rev_pds) = continue_spec current_pos 1 [r_e] hdr [pd_e] in
		  List.rev rev_reps, (hdr, List.rev rev_pds) 
		end
	      else if Pads.IO.get_spec_level pads <> spec_level then
		[r_e], (hdr, [pd_e])
	      else
		(Pads.IO.restore pads; 
		 [],(hdr, []))
	    with 
		Pads.Speculation_failure when Pads.IO.get_spec_level pads = spec_level ->
		  (Pads.IO.restore pads; ([],( hdr, [])))
	      | e -> 
		  if Pads.IO.get_spec_level pads = spec_level then Pads.IO.restore pads;
		  sep_specs.post_process_s pads;
		  term_specs.post_process_t pads;
		  raise e
	  end
	else
	  begin
            let (r_e, pd_e, hdr) = Pads.Record.parse_next alpha_parse hdr pads in
            let (rev_reps, hdr, rev_pds) = 
              continue current_pos 1 [r_e] hdr [pd_e]
            in
            List.rev rev_reps, (hdr, List.rev rev_pds)
	  end
    in
    sep_specs.post_process_s pads;
    term_specs.post_process_t pads;
    (res_rep, (Pads.Record.finish_pd_hdr res_hdr pads,res_body))

  let print alpha_print (sep,term) rep (hdr,elt_pds) pads =
    let sep_specs = get_sep_specs sep term pads in
    let print_sep = sep_specs.print in
    let rec print_tail = function
        ([],[]) -> ()
      | (r::rs,p::ps) -> (
            print_sep pads;
            alpha_print r p pads;
            print_tail (rs, ps))
    in  
      match (rep,elt_pds) with
          ([],[]) -> ()
        | (r::rs,p::ps) -> 
            begin
              alpha_print r p pads;
              print_tail (rs,ps)
            end

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
        init = gen_tool.GLT.list_t.GLT.l_init;
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

  open Type.SPProducer
  open Generic_producer
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
  let tyrep alpha_tyrep = {UnitGFTys.trep = fun tool ->  tool.UnitGFTys.list alpha_tyrep}
end
