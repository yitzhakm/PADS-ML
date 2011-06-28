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
  
(* Import the metadata type so as not to have to prefix each field
   name with "Description." *)
type metadata = Description.metadata = {
  name : PadscId.id; (** The (visible) name of the description. *)
  kind : D.kind;
  tyclass : D.tyclass;
}

(** Dummy location provided as default of quotations. Quotations that
    want to specify their own location should just locally rebind
    loc. The use of a dummy loc is ok as all of the locs in the ast
    are replaced by camlp4 once the ast is returned at the top
    level.*)
let loc = Ploc.dummy

(** create expression for parsing a type. 
    @args: parse_fn, is_first, pd_hdr_var name *)
let record_parse parser_fn is_first hdr_var = 
  if is_first then
    <:expr<$C.pads_record_mod$.$lid:N.record_parse_first$ $parser_fn$>>
  else
    <:expr<$C.pads_record_mod$.$lid:N.record_parse_next$ $parser_fn$ $lid:hdr_var$>>

(** create expression for absorbing a type. 
    @args: parse_fn, is_first, pd_hdr_var name *)
let record_absorb parser_fn is_first hdr_var = 
  if is_first then
    <:expr<$C.pads_record_mod$.$lid:N.record_absorb_first$ $parser_fn$>>
  else
    <:expr<$C.pads_record_mod$.$lid:N.record_absorb_next$ $parser_fn$ $lid:hdr_var$>>

(** create expression for absorbing a literal as a record field.
    @args: literal expression, is_first, pd_hdr variable name *)
let record_absorb_lit lit_expr is_first hdr_var =
  let e = HL.ocaml_of_host_e lit_expr in
    if is_first then
      match HL.infer_lit_tp lit_expr with
	  HL.Char_lit   -> <:expr<$C.pads_record_mod$.$lid:N.record_absorb_first_litc$ $e$>>
	| HL.String_lit -> <:expr<$C.pads_record_mod$.$lid:N.record_absorb_first_lits$ $e$>>
	| HL.Int_lit    -> <:expr<$C.pads_record_mod$.$lid:N.record_absorb_first_liti$ $e$>>
	| HL.RegExp_lit -> <:expr<$C.pads_record_mod$.$lid:N.record_absorb_first_litres$ $e$>>
	| HL.EOL_lit ->  
	    <:expr<fun $C.pads_p$ -> 
	      let initial_hdr = $C.pads_mod$.make_valid_pd_hdr ($C.pads_mod$.make_empty_span $C.pads_e$) in
		$C.pads_mod$.find_eor initial_hdr $C.pads_e$>>
    else
      let h = <:expr<$lid:hdr_var$>> in
	match HL.infer_lit_tp lit_expr with
	    HL.Char_lit   -> <:expr<$C.pads_record_mod$.$lid:N.record_absorb_next_litc$ $e$ $h$>>
	  | HL.String_lit -> <:expr<$C.pads_record_mod$.$lid:N.record_absorb_next_lits$ $e$ $h$>>
	  | HL.Int_lit    -> <:expr<$C.pads_record_mod$.$lid:N.record_absorb_next_liti$ $e$ $h$>>
	  | HL.RegExp_lit -> <:expr<$C.pads_record_mod$.$lid:N.record_absorb_next_litres$ $e$ $h$>>
	  | HL.EOL_lit ->  
	    <:expr<$C.pads_mod$.find_eor $h$>>

(** create expression for absorbing a literal as a datatype variant.
    @args: literal expression *)
let datatype_absorb_lit lit_expr =
  let e = HL.ocaml_of_host_e lit_expr in
    match HL.infer_lit_tp lit_expr with
	HL.Char_lit   -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_litc$ $e$>>
      | HL.String_lit -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_lits$ $e$>>
      | HL.Int_lit    -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_liti$ $e$>>
      | HL.RegExp_lit -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_litres$ $e$>>
      | HL.EOL_lit -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_litEOL$>>

(** create expression for absorbing a literal as a datatype case.
    @args: literal expression *)
let datatype_absorb_lit_case lit_expr =
  let e = HL.ocaml_of_host_e lit_expr in
    match HL.infer_lit_tp lit_expr with
	HL.Char_lit   -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_litc_case$ $e$>>
      | HL.String_lit -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_lits_case$ $e$>>
      | HL.Int_lit    -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_liti_case$ $e$>>
      | HL.RegExp_lit -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_litres_case$ $e$>>
      | HL.EOL_lit -> <:expr<$C.pads_dt_mod$.$lid:N.dt_absorb_litEOL_case$>>

(* Abbreviations *)
let rid = N.mk_rep_str
let pid = N.mk_pd_str

let r_p id = <:patt<$lid:rid id$>>
let p_p id = <:patt<$lid:pid id$>>

let r_e id = <:expr<$lid:rid id$>>
let p_e id = <:expr<$lid:pid id$>>

let rec gen_record_parser dt tc loc fields is_tuple = 
  (** list of field names in record. *)
  let names = 
    C.munge_fields
      (fun _ -> [])
      (fun (id,_) -> [id])
      (fun (id,_,_) -> [id])
      (fun _ -> [])
      fields
  in

  (* Assemble pattern-expression pairs used to build let expressions below. *)
  let patt_expr_list = 
    let hdr_name = N.pd_hdr_var in
      C.munge_fields_state

	(* Absorb field *)
	(fun ctp n -> 
	   let e = match ctp with 
	       Ast.Type tp -> record_absorb (gen_parser dt tc loc tp) (n=1) hdr_name
	     | Ast.Exp exp -> record_absorb_lit exp (n=1) hdr_name
	   in
	     [(<:patt<$lid:hdr_name$>>, 
	       <:expr<$e$ $C.pads_e$>>)],
	     n+1)

	(* Normal field *)
	(fun (id,tp) n ->  
	   let f = n = 1 in
	     [(<:patt<($C.id_p id$,$p_p id$,$lid:hdr_name$)>>, 
	       <:expr<$record_parse (gen_parser dt tc loc tp) f hdr_name$ $C.pads_e$>>)],
	     n+1)
	
	(* Compute field *)
	(fun (id,tp,exp) n ->  
	   let genpd_fn = GenPDGenerator.gen_genpd dt tc loc tp in
	   let e = HostLanguage.ocaml_of_host_e exp in
	     [(<:patt<($C.id_p id$,$p_p id$)>>, 
	       <:expr<$C.pads_mod$.Compute.generate_parser $e$ $genpd_fn$ $C.pads_e$>>)],
	     n)

	(* Let field *)
	(fun (id,exp) n -> [(<:patt<$C.id_p id$>>, HL.ocaml_of_host_e exp)], n)

	1 fields
  in

  (* Make the patt-expr pair for the record rep. Note that the camlp4
     documentation is wrong and a patt-expr list is required, not an
     expr-expr list. 
  *)
  let mk_recrep_e = List.map (fun n -> r_p n, r_e n) in
    
  (* Make the patt-expr pair for the record pd *)
  let mk_recpd_e = List.map (fun n -> p_p n,p_e n) in
    
  (* Make the expression for the tuple rep *)
  let mk_tuprep_e = List.map (fun n -> r_e n) in
    
  (* Make the expression for the tuple pd *)
  let mk_tuppd_e = List.map (fun n -> p_e n) in
    

  (* Make a list of identifier expressions *)
  let mk_elist mk = List.map
    (fun name -> <:expr<$lid:mk name$>>) in

  (* Make a list of identifier patterns *)
  let mk_plist mk = List.map
    (fun name -> <:patt<$lid:mk name$>>) in

  (* Generates a list expression from a function and a list of ids *)
  let mk_list_e mk_e ids = 
    List.fold_right (fun id le -> <:expr<[$mk_e id$::$le$]>>) ids <:expr<[]>>
  in

  let mklets = List.fold_right (fun pe e -> <:expr<let $list:[pe]$ in $e$>>) in
    
    (* The first two cases deal with empty and singleton tuples. These
       can arise do to the elimination of literals from tuples and
       records.  We need to treat them specially because camlp4 throws
       an exception on empty or singleton tuple expressions and patterns. 
       Singleton records appear to be okay.
    *)
    match names with
	[] -> 
	  let final_e = <:expr<	    
	    let $lid:N.pd_hdr_var$ = 
	      $C.pads_record_mod$.$lid:N.record_finish_pd$ 
		$lid:N.pd_hdr_var$ $C.pads_e$ 
	    in
	      ((), ($lid:N.pd_hdr_var$, ())) >> 
	  in
	    (<:expr<fun $C.pads_p$ -> $mklets patt_expr_list final_e$>>)
      | [name] ->
	  let rep_e = <:expr<$lid:PadscId.id2string name$>> in
	  let pd_e  = <:expr<$lid:pid name$>> in
	  let hdr_e = <:expr<[$C.get_pd_hdr$ $lid:pid name$]>> in

	  let rep_p = <:patt<$lid:rid name$>> in
	  let pd_p  = <:patt<$lid:pid name$>> in

	  (* The bodies of the make_rep and make_pd functions *)
	  let mr_body = if is_tuple then r_e name
	    (* records can have one field, unlike tuples. *)
          else <:expr<{$list:mk_recrep_e names$}>> in
	  let mp_body = if is_tuple then p_e name
	    (* records can have one field, unlike tuples. *)
          else <:expr<{$list:mk_recpd_e names$}>> in

	  let final_e = <:expr<
	    let rep = $rep_e$ in
	    let pd  = $pd_e$ in
	    let $lid:N.pd_hdr_var$ = 
	      $C.pads_record_mod$.$lid:N.record_finish_pd$ 
		$lid:N.pd_hdr_var$ $C.pads_e$ 
	    in
	      ($lid:N.make_rep$ rep, $lid:N.make_pd$ $lid:N.pd_hdr_var$ pd)
	      >> 
	  in
	    <:expr< 
	      fun $C.pads_p$ -> 
		let $lid:N.make_rep$ $rep_p$ = $mr_body$ in
		let $lid:N.make_pd$ hdr $pd_p$ = (hdr, $mp_body$) 
		in $mklets patt_expr_list final_e$
		   >>
      | _ ->
	  let reps_e = mk_elist PadscId.id2string names in
	  let pds_e  = mk_elist pid names in
	  let hdrs_e = mk_list_e (fun n -> <:expr<$C.get_pd_hdr$ $lid:pid n$>>) names in

	  let reps_p = mk_plist rid names in
	  let pds_p  = mk_plist pid names in

	  (* The bodies of the make_rep and make_pd functions *)
	  let mr_body = if is_tuple then <:expr<($list:mk_tuprep_e names$)>>
          else <:expr<{$list:mk_recrep_e names$}>> in
	  let mp_body = if is_tuple then <:expr<($list:mk_tuppd_e names$)>>
          else <:expr<{$list:mk_recpd_e names$}>> in

	  let final_e = <:expr<
	    let reps = ($list:reps_e$) in
	    let pds  = ($list:pds_e$) in
	    let $lid:N.pd_hdr_var$ = 
	      $C.pads_record_mod$.$lid:N.record_finish_pd$ 
		$lid:N.pd_hdr_var$ $C.pads_e$ 
	    in
	      ($lid:N.make_rep$ reps, $lid:N.make_pd$ $lid:N.pd_hdr_var$ pds)
	      >> 
	  in
	    <:expr< 
	      fun $C.pads_p$ -> 
		let $lid:N.make_rep$ ($list:reps_p$) = $mr_body$ in
		let $lid:N.make_pd$ hdr ($list:pds_p$) = (hdr, $mp_body$) in
		  $mklets patt_expr_list final_e$
		   >>

and gen_parser dt tc loc = function
    Ast.TupleTp ctps ->
      let fresh_rep = (PadscId.id2string (PadscId.freshid "r")) ^ "_" in
      let fresh_pd = (PadscId.id2string (PadscId.freshid "p")) ^ "_" in
      let rid i = fresh_rep ^ (string_of_int i) in
      let pid i = fresh_pd ^ (string_of_int i) in
      let fields = C.convert_t2r rid ctps in
	gen_record_parser dt tc loc fields true	    
	  
  | Ast.TidTp tid -> 
      (match D.lookup_descr dt tid with
	  None -> PadscError.report_error loc ("Type " ^ (PadscId.id2string tid) ^ " not found.")
	| Some d -> C.gen_TidTp loc "ParserGenerator" tid N.parser_fun d)

  | Ast.ValAppTp (tp_fun,exp) -> 
      let pfn = gen_parser dt tc loc tp_fun in
      let e   = HL.ocaml_of_host_e exp in
	<:expr<$pfn$ $e$>>

  | Ast.TpAppTp (tp_args,tp_fun) -> C.gen_TpAppTp tp_fun tp_args (gen_parser dt tc loc)

  | Ast.TableTp (id, sep, term, keytype, _) ->
      let pfn = C.gen_TpAppTp (PadscId.makeid "Ptable") [Ast.TidTp id] (gen_parser dt tc loc) in
      let sep_e   = HL.ocaml_of_host_e sep in
      let term_e = HL.ocaml_of_host_e term in
	<:expr<$pfn$ ($sep_e$, $term_e$, $lid:N.getkey_fun$, $lid:N.getpd_fun$)>>
(*

*)
  | Ast.WhereTp (id,tp,exp) ->	
      let where_fn = <:expr<$C.pads_where_mod$.$lid:N.where_parse_underlying$>> in
      let parse_fn = gen_parser dt tc loc tp in
      let pred = <:expr<fun $C.id_p id$ -> $HL.ocaml_of_host_e exp$>> in
	<:expr< fun $C.pads_p$ -> $where_fn$ $parse_fn$ $pred$ $C.pads_e$>>

  | _ -> PadscError.report_error loc "ParserGenerator: unsupported feature"
	      
let gen_tp_parser_body dt tc loc = function    
    (* We single out records here, because records cannot be anonymous
       (nor, therefore, nested) and so are only allowed at top
       level. 

       XXX: doesn't this belong in the type checker? *)
    Ast.RecordTp fields -> 
      gen_record_parser dt tc loc fields false
  | b -> gen_parser dt tc loc b      

     
(* let rec parse pads =  *)
(*   let make_rep r = r in *)
(*   let make_pd h p =  *)
(*     Pads.Datatype.make_pd_hdr h, p in *)
(*   let make_err_pd pads = ... *)
(*   match Pads.Datatype.parse_variant VT1.parse pads with *)
(*       Some (r,p) -> make_rep V1 r, make_pd (Pads.get_pd_hdr p), V1_pd p *)
(*     | None -> *)
(* 	match Pads.Datatype.parse_variant VT2.parse pads with *)
(* 	    Some (r,p) -> make_rep V2 r, make_pd ((Pads.get_pd_hdr p), V2_pd p) *)
(* 	  | None ->  *)
(* (\*  ... *\) *)
(* 	      match Pads.Datatype.parse_variant VTn.parse pads with *)
(* 		  Some (r,p) -> make_rep Vn r, make_pd ((Pads.get_pd_hdr p), Vn_pd p) *)
(* 		| None -> Verr, Pads.Datatype.make_err_pd pads Verr_pd *)

let gen_dtp_parser_body dt tc loc = function
    Ast.ImplicitDT (vs,def_opt) -> 

      (* mkmatch - generate the match expression for a particular variant.
	 var    : the given variant
	 none_e : the expression to call in case this variant fails to parse. *)
      let mkmatch var none_e =
	match var with
	  Ast.FullVar(id,tp) -> 
	    if C.contains_host_tp tp then none_e
	    else
	    (* dt_fn : the error-handling function from the datatype module.
	       parser_fn: the parsing function for the variant.
	       c_rep : the rep constructor for the variant
	       c_pd : the pd constructor for the variant *)
	    let dt_fn = <:expr<$lid:N.dt_parse_vt$>> in
	    let parser_fn = gen_parser dt tc loc tp in
	    let c_rep = <:expr<$uid:N.mk_rep_str id$>> in
	    let c_pd = <:expr<$uid:N.mk_pd_str id$>>  in
	    let descr_e = <:expr<$C.pads_dt_mod$.$dt_fn$ $parser_fn$ $C.pads_e$>> in
	      (* XXX: r and p are not guaranteed fresh, is that ok? *)
	      <:expr< match $descr_e$ with 
                  [ Some (r,p) -> ($C.pads_dt_mod$.make_rep ($c_rep$ r),
				   $C.pads_dt_mod$.make_pd ($C.get_pd_hdr$ p, $c_pd$ p))
	          | None -> $none_e$ ] >>
	| Ast.AbsorbVar(id,Ast.Type tp) -> 
	    let dt_fn = <:expr<$lid:N.dt_absorb_vt$>> in
	    let parser_fn = gen_parser dt tc loc tp in
	    let c_rep = <:expr<$uid:N.mk_rep_str id$>> in
	    let c_pd  = <:expr<$uid:N.mk_pd_str id$>> in
	    let descr_e = <:expr<$C.pads_dt_mod$.$dt_fn$ $parser_fn$ $C.pads_e$>> in
	      (* XXX: sp is not guaranteed fresh, is that ok? *)
	      <:expr< match $descr_e$ with
		        [ Some sp -> ($C.pads_dt_mod$.$lid:N.dt_make_rep$ $c_rep$, 
			              $C.pads_dt_mod$.$lid:N.dt_make_absorb_pd$ sp $c_pd$)
	                | None -> $none_e$ ] >>
	| Ast.AbsorbVar(id,Ast.Exp e) -> 
	    (* XXX: Why limited to literals? *)
	    let dt_fn = datatype_absorb_lit e in
	    let c_rep = <:expr<$uid:N.mk_rep_str id$>> in
	    let c_pd  = <:expr<$uid:N.mk_pd_str id$>> in
	    let descr_e = <:expr<$dt_fn$ $C.pads_e$>> in
	      (* XXX: sp is not guaranteed fresh, is that ok? *)
	      <:expr< match $descr_e$ with
		        [ Some sp -> ($C.pads_dt_mod$.$lid:N.dt_make_rep$ $c_rep$, 
			              $C.pads_dt_mod$.$lid:N.dt_make_absorb_pd$ sp $c_pd$)
	                | None -> $none_e$ ] >>
      in
      let mkmatches = List.fold_right mkmatch in
      let def_e = match def_opt with
	  (* For both Some branches, use one of the case functions, as the default branch acts
	     like a case rather than a variant. That is because it
	     can contain errors (like cases), while variants must
	     succeed to be chosen. *)		
	  Some (Ast.GenDefault(id_opt,tp,e)) -> 
	    let dt_fn = <:expr<$lid:N.dt_gen_case$>> in
	    let gen_val = HL.ocaml_of_host_e e in
	    let genpd_fn = GenPDGenerator.gen_genpd dt tc loc tp in	      
	    let (c_rep,c_pd) = 
	      match id_opt with
		  None -> <:expr<fun r -> $uid:N.def_vt$ r>>,
		     <:expr<fun p -> $uid:N.def_pd_vt$ p>>
    	        | Some id -> <:expr<fun r -> $uid:N.mk_rep_str id$ r>>,
		     <:expr<fun p -> $uid:N.mk_pd_str id$ p>>
	    in
	      <:expr<$C.pads_dt_mod$.$dt_fn$ $gen_val$ $genpd_fn$ $c_rep$ $c_pd$ $C.pads_e$>>	    

	| Some (Ast.FullDefault(id_opt,tp)) ->
	    let dt_fn = <:expr<$lid:N.dt_parse_case$>> in
	    let parser_fn = gen_parser dt tc loc tp in
	    let (c_rep,c_pd) = 
	      match id_opt with
		  None -> <:expr<fun r -> $uid:N.def_vt$ r>>,
		     <:expr<fun p -> $uid:N.def_pd_vt$ p>>
	        | Some id -> <:expr<fun r -> $uid:N.mk_rep_str id$ r>>,
		     <:expr<fun p -> $uid:N.mk_pd_str id$ p>>
	    in
	      <:expr<$C.pads_dt_mod$.$dt_fn$ $parser_fn$ $c_rep$ $c_pd$ $C.pads_e$>>	    

	| None -> <:expr< ($uid:N.err_vt$, $C.pads_dt_mod$.handle_error_variant
			     $C.pads_e$ $uid:N.err_pd_vt$) >> 
      in
	<:expr<fun $C.pads_p$ -> $mkmatches vs def_e$ >>
		
  | Ast.CaseDT (descr,cases) -> 
      (*
	fun parse pads = 
	match e with
	p1 -> Pads.Datatype.parse_case parse1 (fun r -> C1 r) (fun p -> C1_pd p) pads
	| p2 -> Pads.Datatype.absorb_case parse2 (fun r -> C2 r) (fun p -> C2_pd p) pads
	| p3 -> Pads.Datatype.absorb_char_case c (fun r -> C3 r) (fun p -> C3_pd p) pads
	| p4 -> Pads.Datatype.gen_case e (fun r -> C4 r) (fun p -> C4_pd p) pads
      *)
      let mkcase = function
	  Ast.FullCase(pat,id,tp) -> 
	    if C.contains_host_tp tp then (
	      PadscError.report_warning loc "ParserGenerator: host type used in branch; will not be instantiatable at parse time.";
	      HL.ocaml_of_host_p pat, None, <:expr<raise (Failure "This case cannot be instantiated at parse time.")>>
	    )
	    else
	    (* dt_fn : the error-handling function from the datatype module.
	       parser_fn: the parsing function for the variant.
	       c_rep : the rep constructor for the variant
	       c_pd : the pd constructor for the variant *)
	    let dt_fn = <:expr<$lid:N.dt_parse_case$>> in
	    let parser_fn = gen_parser dt tc loc tp in
	    let c_rep = <:expr<fun r -> $uid:N.mk_rep_str id$ r>> in
	    let c_pd = <:expr<fun p -> $uid:N.mk_pd_str id$ p>> in
	    HL.ocaml_of_host_p pat, None, <:expr<$C.pads_dt_mod$.$dt_fn$ $parser_fn$ $c_rep$ $c_pd$ $C.pads_e$>>
	| Ast.AbsorbCase(pat,id,Ast.Type tp) -> 
	    let dt_fn = <:expr<$lid:N.dt_absorb_case$>> in
	    let parser_fn = gen_parser dt tc loc tp in
	    let c_rep = <:expr<$uid:N.mk_rep_str id$>> in
	    let c_pd  = <:expr<$uid:N.mk_pd_str id$>> in
	    HL.ocaml_of_host_p pat, None, <:expr<$C.pads_dt_mod$.$dt_fn$ $parser_fn$ $c_rep$ $c_pd$ $C.pads_e$>>
	| Ast.AbsorbCase(pat,id,Ast.Exp e) -> 
	    (* The expression "fn" is fully qualified, that is - it's not relative to the datatype module. 
	       Also, it is already applied to the expression "e"*)
	    let fn = datatype_absorb_lit_case e in
	    let c_rep = <:expr<$uid:N.mk_rep_str id$>> in
	    let c_pd  = <:expr<$uid:N.mk_pd_str id$>> in
	    HL.ocaml_of_host_p pat, None, <:expr<$fn$ $c_rep$ $c_pd$ $C.pads_e$>>
	| Ast.GenCase(pat,id,tp,e) -> 
	    let dt_fn = <:expr<$lid:N.dt_gen_case$>>  in
	    let genpd_fn = GenPDGenerator.gen_genpd dt tc loc tp in
	    let c_rep = <:expr<fun r -> $uid:N.mk_rep_str id$ r>> in
	    let c_pd  = <:expr<fun p -> $uid:N.mk_pd_str id$ p>> in
	    HL.ocaml_of_host_p pat, None, 
	    <:expr<$C.pads_dt_mod$.$dt_fn$ $HL.ocaml_of_host_e e$ $genpd_fn$ $c_rep$ $c_pd$ $C.pads_e$>> 
      in
      let mkcases = List.map mkcase in
      (* Default error case, in case nothing else matches. *)
      let def_case = <:patt<_>>,None,
        <:expr< ($uid:N.err_vt$, $C.pads_dt_mod$.make_err_pd $C.pads_e$ $uid:N.err_pd_vt$) >>
      in
      <:expr<fun $C.pads_p$ -> match $HL.ocaml_of_host_e descr$ with [$list:(mkcases cases)@[def_case]$]>>
      
let gen dt current_descr tc loc decl = 
  let (tp_params,name,val_param_opt,_, tp_def) = decl in

  (* extend environment with value params. *)
  let tc_ext = C.process_val_lam tc val_param_opt in

  let body_parser_fun = match tp_def with
      Ast.TpDef tp -> gen_tp_parser_body dt tc_ext loc tp
    | Ast.DtDef dtp_body -> gen_dtp_parser_body dt tc_ext loc dtp_body
  in

  let mk_parser_ty repty pdbty = <:ctyp<Pads.parser__ $repty$ $pdbty$>> in

  (* parameterize body with val. param *)
  let (mk_val_t, val_fun) = match val_param_opt with
      None -> (mk_parser_ty, body_parser_fun)
    | Some (p_id,p_type) -> 
	((fun repty pdbty -> <:ctyp<$HostLanguage.ctyp_of_tp p_type$ -> 
	    $mk_parser_ty repty pdbty$>>),
	 let p_name = PadscId.id2string p_id in
	   <:expr<fun $lid:p_name$ -> $body_parser_fun$>>)
  in

  let poly_t   = C.package_funty mk_val_t tp_params mk_parser_ty in
  let poly_fun = C.package_fun val_fun tp_params N.parser_fun in
    
  let tp_pf_name = Names.parser_fun in
    (* create the final parser function *)
    ([], 
    ([<:sig_item<value $lid:tp_pf_name$ : $poly_t$>>],
    [<:str_item<value rec $lid:tp_pf_name$  = $poly_fun$ >>]),
    current_descr)
    
