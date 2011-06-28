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
module C = Common
module D = Description
module N = Names
module HL = HostLanguage
  
(** Dummy location provided as default of quotations. Quotations that
    want to specify their own location should just locally rebind
    loc. The use of a dummy loc is ok as all of the locs in the ast
    are replaced by camlp4 once the ast is returned at the top
    level.*)
let loc = Ploc.dummy

(*****************************)

(********************)
(* Helper functions *)
(********************)
let abs_type_warning phase_name = 
  PadscError.report_warning loc 
    (phase_name ^ 
       ": Unsupported feature: absorbed types cannot be printed.")

let gen_dt_full f rep_c pd_c =
  let rep_var = PadscId.id2string (PadscId.freshid "r") in
  let pd_var = PadscId.id2string (PadscId.freshid "pd") in
    (<:patt<($uid:rep_c$ $lid:rep_var$, ((_:Pads.pd_header),$uid:pd_c$ $lid:pd_var$))>>, None,
     <:expr<$f$ $lid:rep_var$ $lid:pd_var$ $C.pads_e$>>)

let print_lit lit_expr =
  let e = HL.ocaml_of_host_e lit_expr in
    match HL.infer_lit_tp lit_expr with
	HL.Char_lit   -> <:expr<$C.pads_mod$.$lid:N.print_lit_char$ $e$>>
      | HL.String_lit -> <:expr<$C.pads_mod$.$lid:N.print_lit_string$ $e$>>
      | HL.Int_lit    -> <:expr<$C.pads_mod$.$lid:N.print_lit_int$ $e$>>
      | HL.RegExp_lit -> <:expr<$C.pads_mod$.$lid:N.print_lit_regexp_str$ $e$>>
      | HL.EOL_lit -> <:expr<$C.pads_mod$.$lid:N.print_lit_EOL$>>

let gen_dt_abs_lit e rep_c pd_c =
  (<:patt<($uid:rep_c$, ((_:Pads.pd_header),$uid:pd_c$))>>, None,
   <:expr<$print_lit e$ $C.pads_e$>>)

let gen_dt_abs_type f tp rep_c pd_c =
  (<:patt<($uid:rep_c$, ((_:Pads.pd_header),$uid:pd_c$))>>, None,
   (* Nothing to print. *)
   <:expr<()>>)

let gen_dt_err rep_c pd_c = 
  (<:patt<($uid:rep_c$, ((_:Pads.pd_header),$uid:pd_c$))>>, None,
   (* Nothing to print, as none of the branches succeeded. *)
   <:expr<()>>) 

(* Computed variant/case *)
let gen_dt_comp rep_c pd_c = 
  (<:patt<($uid:rep_c$ _, ((_:Pads.pd_header),$uid:pd_c$ _))>>, None,
   (* Nothing to print. *)
   <:expr<()>>) 

(********************)

let f_name = Names.printer_fun
let phase_name = "PrinterGenerator"

let gen_where_f sub_f id exp =
  let r_s = N.mk_rep_str id in
  let pd_s = N.mk_pd_str id in
  <:expr<fun $lid:r_s$ ((_:Pads.pd_header),$lid:pd_s$) $C.pads_p$ ->
	   $sub_f$ $lid:r_s$ $lid:pd_s$ $C.pads_e$>>

let gen_record_abs_type f tp n =
  abs_type_warning phase_name; []

let gen_record_abs_lit exp n = 
   [[],[<:expr<$print_lit exp$ $C.pads_e$>>]]

let gen_record_full id f n =
  let r_s = N.mk_rep_str id in
  let pd_s = N.mk_pd_str id in
    [[],[<:expr<$f$ $lid:r_s$ $lid:pd_s$ $C.pads_e$>>]]

(* Computed field: nothing to print. *)
let gen_record_comp id tp exp n = []

(* Let field: nothing to print, but still bind value. *)
let gen_record_let id exp n = [[<:patt<$C.id_p id$>>, HL.ocaml_of_host_e exp],[]]

let gen_record_compose is_tuple names processed_fields =
  let lets_l,prints_l = List.split processed_fields in
  let lets = List.flatten lets_l in
  let prints = List.flatten prints_l in
  let print_es = if prints = [] then <:expr<()>> else <:expr<do {$list:prints$}>> in
  let r_p,pd_p =
    (* The first two cases deal with empty and singleton tupels. These
       can arise do to the elimination of literals from tuples and
       records.  We need to treat them specially because camlp4 throws
       an exception on empty or singleton tuple patterns.  
       Singleton records appear to be okay.
    *)
    match names with
	[] -> <:patt<()>>, <:patt<((_:Pads.pd_header),())>>
      | [name] ->
	  if is_tuple then
	    <:patt<$lid:N.mk_rep_str name$>>, <:patt<((_:Pads.pd_header),$lid:N.mk_pd_str name$)>>	  
	  else
	    let r_p = <:patt<$lid:N.mk_rep_str name$>> in 
	    let pd_p = <:patt<$lid:N.mk_pd_str name$>> in
	      <:patt<{$list:[(r_p,r_p)]$}>>, <:patt<((_:Pads.pd_header),{$list:[(pd_p,pd_p)]$})>>	  
      | _ ->
	  if is_tuple then
	    let r_ps = List.map (fun n -> <:patt<$lid:N.mk_rep_str n$>>) names
	    in
	    let pd_ps = List.map (fun n -> <:patt<$lid:N.mk_pd_str n$>>) names
	    in
	      <:patt<($list:r_ps$)>>, <:patt<((_:Pads.pd_header),($list:pd_ps$))>>	  
	  else
	    let r_ps = List.map (fun n -> 
				   let p = <:patt<$lid:N.mk_rep_str n$>> in 
				     p,p) names
	    in
	    let pd_ps = List.map (fun n -> 
				    let p = <:patt<$lid:N.mk_pd_str n$>> in 
				      p,p) names
	    in
	      <:patt<{$list:r_ps$}>>, <:patt<((_:Pads.pd_header),{$list:pd_ps$})>>	  
  in
  let lets_es = List.fold_right (fun pe e -> <:expr<let $list:[pe]$ in $e$>>)  lets print_es in
    <:expr<fun $r_p$ $pd_p$  $C.pads_p$ -> $lets_es$>>
      
let gen_dt_full_var vt_f rep_c pd_c cases =
  (gen_dt_full vt_f rep_c pd_c):: cases

let gen_dt_abs_type_var vt_f tp rep_c pd_c cases =
  abs_type_warning phase_name; 
  (gen_dt_abs_type vt_f tp rep_c pd_c) :: cases

(*   let rep_e = GetDefaultGenerator.gen_f in *)
(*   let pd_e = PadscId.id2string (PadscId.freshid "pd") in *)
(*     (<:patt<$rep_c$>>, None, *)
(*     <:expr<fun (_,$pd_c$) $C.pads_p$ ->  *)
(*       $vt_f$ $str:rep_var$ $str:pd_var$ $C.pads_e$>>)  *)
(*     :: cases *)

let gen_dt_abs_lit_var e rep_c pd_c cases =
  (gen_dt_abs_lit e rep_c pd_c)::cases

(* computed default *)
let gen_dt_comp_def tp e rep_c pd_c = 
  [gen_dt_comp rep_c pd_c]

(* full default *)
let gen_dt_full_def f rep_c pd_c = [gen_dt_full f rep_c pd_c]

(* no default *)
let gen_dt_no_def rep_c pd_c = [gen_dt_err rep_c pd_c]

(* compose the generated code for each variant into a function. *)
let gen_dt_compose cases = 
  let all_cases = cases @ [<:patt<_>>,None,
		   <:expr<Pads.Log.report_error "print" None Pads.Unmatched_constructors 
		             (Some "Invalid data passed to print") $C.pads_e$>>]
  in
    <:expr<fun rep pd $C.pads_p$-> match (rep,pd) with [$list:all_cases$]>>

let gen_dt_full_case pat case_f rep_c pd_c = gen_dt_full case_f rep_c pd_c

let gen_dt_abs_type_case pat case_f tp rep_c pd_c = 
  abs_type_warning phase_name; 
  gen_dt_abs_type case_f tp rep_c pd_c
    
let gen_dt_abs_lit_case pat e rep_c pd_c = gen_dt_abs_lit e rep_c pd_c

let gen_dt_comp_case pat tp e rep_c pd_c = 
  gen_dt_comp rep_c pd_c

let gen_dt_def_case rep_c pd_c = gen_dt_err rep_c pd_c

let gen_dt_match_compose descr cases = gen_dt_compose cases

let f_body_t = <:ctyp<$lid:Names.rep$ -> $lid:Names.pd$ -> $C.handle_t$ -> unit>>

(*********************************)

(* Import the metadata type so as not to have to prefix each field
   name with "Description." *)
type metadata = Description.metadata = {
  name : PadscId.id; (** The (visible) name of the description. *)
  kind : D.kind;
  tyclass : D.tyclass;
}

(* Abbreviations *)
let rid = N.mk_rep_str
let pid = N.mk_pd_str

let r_p id = <:patt<$lid:rid id$>>
let p_p id = <:patt<$lid:pid id$>>

let r_e id = <:expr<$lid:rid id$>>
let p_e id = <:expr<$lid:pid id$>>


let rec gen_record_f dt tc loc fields is_tuple = 
  (** list of field names in record. *)
  let names = 
    C.munge_fields
      (fun ctp -> [])
      (fun (id,_) -> [id])
      (fun (id,_,_) -> [id])
      (fun _ -> [])
      fields
  in

  let processed_fields = 
    C.munge_fields_state
      
      (* Absorb field *)
      (fun ctp n -> 
	 (match ctp with 
	     Ast.Type tp -> gen_record_abs_type (gen_f dt tc loc tp) tp n
	   | Ast.Exp exp -> gen_record_abs_lit exp n),
	 n+1)
      
      (* Normal field *)
      (fun (id,tp) n ->  
	 gen_record_full id (gen_f dt tc loc tp) n,
	 n+1)
      
      (* Compute field *)
      (fun (id,tp,exp) n -> gen_record_comp id tp exp n,n)
      
      (* Let field *)
      (fun (id,exp) n -> gen_record_let id exp n,n)
      
      1 fields
  in
    gen_record_compose is_tuple names processed_fields

and gen_f dt tc loc = function 

    Ast.TupleTp ctps ->
      let fresh_rep = (PadscId.id2string (PadscId.freshid "r")) ^ "_" in
      let fresh_pd = (PadscId.id2string (PadscId.freshid "p")) ^ "_" in
      let rid i = fresh_rep ^ (string_of_int i) in
      let pid i = fresh_pd ^ (string_of_int i) in

      (* Convert tuple elements into record fields *)
      let fields = C.convert_t2r rid ctps in
	gen_record_f dt tc loc fields true	    
	  
  | Ast.TidTp tid -> 
      (match D.lookup_descr dt tid with
	  None -> PadscError.report_error loc ("Type " ^ (PadscId.id2string tid) ^ " not found.")
	| Some d -> C.gen_TidTp loc "PrinterGenerator" tid N.printer_fun d)

  | Ast.ValAppTp (tp_fun,exp) -> 
      let f = gen_f dt tc loc tp_fun in
      let e   = HostLanguage.ocaml_of_host_e exp in
	<:expr<$f$ $e$>>

  | Ast.TpAppTp (tp_args,tp_fun) -> C.gen_TpAppTp tp_fun tp_args (gen_f dt tc loc)

  | Ast.TableTp (id, sep, term, keyt, _) ->
      let pfn = C.gen_TpAppTp (PadscId.makeid "Ptable") 
	[Ast.TidTp id] (gen_f dt tc loc) in
      let sep_e   = HL.ocaml_of_host_e sep in
      let term_e = HL.ocaml_of_host_e term in
	<:expr<$pfn$ ($sep_e$, $term_e$, $lid:N.getkey_fun$, $lid:N.getpd_fun$)>>

	
  | Ast.WhereTp (id,tp,exp) ->	
      let sub_f = gen_f dt tc loc tp in
	gen_where_f sub_f id exp

  | _ -> PadscError.report_error loc "PrinterGenerator: unsupported feature"
	      
let gen_tp_f_body dt tc loc = function    
    (* We single out records here, because records cannot be anonymous
       (nor, therefore, nested) and so are only allowed at top
       level. 

       XXX: doesn't this belong in the type checker? *)
    Ast.RecordTp fields -> 
      gen_record_f dt tc loc fields false
  | b -> gen_f dt tc loc b      
    
(**
   returns: strings rep_c,pd_c
   rep_c : the rep data constructor 
   pd_c : the pd data constructor *)
let mk_cons id = 
  N.mk_rep_c_str id, N.mk_pd_c_str id

let gen_dtp_f_body dt tc loc = function
    Ast.ImplicitDT (vs,def_opt) -> 

      (* mkmatch - generate the match expression for a particular variant.
	 var    : the given variant
	 acc : the accumulator for the fold. *)
      let mk_case var acc =
	match var with
	  Ast.FullVar(id,tp) -> 
	    if C.contains_host_tp tp then (
	      PadscError.report_warning loc "PrinterGenerator: host type used in branch; case will not be included in printer.";
	      acc
	    )
	    else
	    (* vt_f : the function for the variant. *)
	    let vt_f = gen_f dt tc loc tp in
	    let rep_c, pd_c = mk_cons id in
	      gen_dt_full_var vt_f rep_c pd_c acc

	| Ast.AbsorbVar(id,Ast.Type tp) -> 
	    (* vt_f : the function for the variant. *)
	    let vt_f = gen_f dt tc loc tp in
	    let rep_c, pd_c = mk_cons id in
	      gen_dt_abs_type_var vt_f tp rep_c pd_c acc

	| Ast.AbsorbVar(id,Ast.Exp e) -> 
	    let rep_c, pd_c = mk_cons id in
	      (* XXX: Why limited to literals? *)
	      gen_dt_abs_lit_var e rep_c pd_c acc
      in

	(* Make the default constructors from the optional id. *)
      let mk_def_cons id_opt =
	(* Choose the right string for the constructor names. *)
	match id_opt with
	      None -> N.def_vt, N.def_pd_vt
	    | Some id -> N.mk_rep_c_str id, N.mk_pd_c_str id
      in

      let def_vt = match def_opt with
	  Some (Ast.GenDefault(id_opt,tp,e)) -> 
	    let (rep_c,pd_c) = mk_def_cons id_opt in
	      gen_dt_comp_def tp e rep_c pd_c

	| Some (Ast.FullDefault(id_opt,tp)) ->
	    let def_f = gen_f dt tc loc tp in
	    let (rep_c, pd_c) = mk_def_cons id_opt in
	      gen_dt_full_def def_f rep_c pd_c

	| None -> 
	    let rep_c = N.err_vt in
	    let pd_c =  N.err_pd_vt in
	      gen_dt_no_def rep_c pd_c
      in

      let cases = List.fold_right mk_case vs def_vt in
	gen_dt_compose cases
	  
  | Ast.CaseDT (descr,cases) -> 
      let mk_case = function
	  Ast.FullCase(pat,id,tp) -> 
	    let rep_c, pd_c = mk_cons id in
	    (* case_f: the function for the case. *)
	    let case_f = if not (C.contains_host_tp tp) then gen_f dt tc loc tp 
	    else (
	      PadscError.report_warning loc 
		"PrinterGenerator: host type used in branch; will not be printable at run time.";
	      <:expr<raise (Failure "This case cannot be instantiated at parse time.")>>
	    )
	    in gen_dt_full_case pat case_f rep_c pd_c

	| Ast.AbsorbCase(pat,id,Ast.Type tp) -> 
	    (* case_f: the function for the case. *)
	    let case_f = gen_f dt tc loc tp in
	    let rep_c, pd_c = mk_cons id in
	      gen_dt_abs_type_case pat case_f tp rep_c pd_c

	| Ast.AbsorbCase(pat,id,Ast.Exp e) -> 
	    let rep_c, pd_c = mk_cons id in
	      gen_dt_abs_lit_case pat e rep_c pd_c

	| Ast.GenCase(pat,id,tp,e) -> 
	    let rep_c, pd_c = mk_cons id in
	      gen_dt_comp_case pat tp e rep_c pd_c
 
     in
	(* Default error case, in case nothing else matches. *)
      let def_case = 
	let rep_c = N.err_vt in
	let pd_c = N.err_pd_vt in 
	  gen_dt_def_case rep_c pd_c
      in
	(* cs is the list of processed cases. *)
      let cs = (List.map mk_case cases) @ [def_case] in
	gen_dt_match_compose descr cs
      
let gen dt current_descr tc loc decl =
  let (tp_params,name,val_param_opt,_, tp_def) = decl in

  (* extend environment with value params. *)
  let tc_ext = C.process_val_lam tc val_param_opt in

  let f_body =  match tp_def with
      Ast.TpDef tp -> gen_tp_f_body dt tc_ext loc tp
    | Ast.DtDef dtp_body -> gen_dtp_f_body dt tc_ext loc dtp_body
  in


  let mk_printer_ty arg_repty arg_pdbty = <:ctyp<Pads.printer $arg_repty$ $arg_pdbty$>> in

  (* parameterize body with val. param *)
  let (mk_val_t, val_fun) = match val_param_opt with
      None -> (mk_printer_ty, f_body)
    | Some (p_id,p_type) -> 
	((fun repty pdbty -> <:ctyp<$HostLanguage.ctyp_of_tp p_type$ -> 
	    $mk_printer_ty repty pdbty$>>),
	 let p_name = PadscId.id2string p_id in
	   <:expr<fun $lid:p_name$ -> $f_body$>>)
  in

  let poly_t   = C.package_funty mk_val_t tp_params mk_printer_ty in
  let poly_fun = C.package_fun val_fun tp_params N.printer_fun in
    
  let tp_pf_name = Names.printer_fun in
    (* create the final function *)
    ([], 
    ([<:sig_item<value $lid:tp_pf_name$ : $poly_t$>>],
    [<:str_item<value rec $lid:tp_pf_name$  = $poly_fun$ >>]),
    current_descr)
