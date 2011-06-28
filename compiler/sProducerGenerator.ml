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

let source = <:expr<$lid:N.producer_src$>>
let source_pat =  <:patt<$lid:N.producer_src$>>

let record_pre = <:expr<$lid:N.producer_rec$.Rec_ver.process_record>>
let dt_pre = <:expr<$lid:N.producer_rec$.Rec_ver.process_datatype>>

let tuple_to_fields tys =
  let rid i = "elt"^(string_of_int i) in
  (* Convert a tuple-element AST node to a record-field AST node. *)
  let convert (i,fds) ty = i+1,(PadscId.makeid (rid i),ty)::fds in
  let _,fields_rev = List.fold_left convert (1,[]) tys in
    List.rev fields_rev

let gen_nonempty_record_producer gp fields is_tuple =
  (* what we want to generate for a record { f1 : t1; f2 : t2; f3 : t3; ... } is:

	 let ([f1; f2; f3; ...], hdr) = producer.Rec_ver.process_record source ["f1"; "f2"; "f3"; ...] in
	 let (f1', f1_pd) = (t1.specialize_producer producer).produce f1 in
	 let (f2', f2_pd) = (t2.specialize_producer producer).produce f2 in
	 let (f3', f3_pd) = (t3.specialize_producer producer).produce f3 in
	 ...
	 { f1 = f1'; f2 = f2'; f3 = f3'; ... }, (hdr, { f1_pd = f1_pd; f2_pd = f2_pd; f3_pd = f3_pd; ... }

	 or, in the case of a tuple,

	 (f1', f2', f3', ...), (hdr, (f1_pd, f2_pd, f3_pd, ...))
  *)

  (* fold over the list of fields (pairs of names and simple types) to get:
	 - pattern of all names
	 - expression of all names as strings
	 - assignments to the record/tuple rep
	 - assignments to the record/tuple pd
	 - function to construct the body
  *)
  let names_p, names_e, rep_assignments, pd_assignments, process_children = 
	List.fold_left
	  (fun (acc_ps,acc_es,acc_as,acc_pd_as,e_fn) (ni, tp) -> 
		(* the type name, in regular and primed form *)
		let ni_str = PadscId.id2string ni in 
		let ni_str' = ni_str ^ "'" in
		let ni_pd_str = ni_str ^ "_pd" in
 
		(* references to the (primed) type name as a pattern variable or an expression *)
		let ni_p = <:patt<$lid:ni_str$>> in 
		let ni_e = <:expr<$lid:ni_str$>> in 
		let ni_pd_p = <:patt<$lid:ni_pd_str$>> in
		let ni_pd_e = <:expr<$lid:ni_pd_str$>> in

		(* type name as a string *)
		let ni_s = <:expr<$str:ni_str$>> in 

		(* names as a pattern: [f1; f2; f3; ...] *)
		let acc_ps' = <:patt<[$ni_p$::$acc_ps$]>> in 
		  
		(* names as strings: ["f1"; "f2"; "f3"; ...] *)
		let acc_es' = <:expr<[$ni_s$::$acc_es$]>> in 
		  
		(* rep assignment: { f1 = f1'; ... } or (f1', ...) *)
		let acc_as' = (ni_p, ni_e)::acc_as in 

		(* pd assignment: { f1_pd = f1_pd; ... } or (f1_pd, ...) *)
		let acc_pd_as' = (ni_pd_p, ni_pd_e)::acc_pd_as in
		  
		(* builds up the recursion:
		   
		   let (f1, f1_pd) = (t1.specialize_producer producer).produce f1 in
		   let (f2, f2_pd) = (t2.specialize_producer producer).produce f2 in
		   let (f3, f3_pd) = (t3.specialize_producer producer).produce f3 in
		   ...
		*)
		let e_fn' = (fun e -> <:expr<let ($ni_p$, $ni_pd_p$) = $gp tp$ $ni_e$ in $e_fn e$>>) in 
          (acc_ps', acc_es', acc_as', acc_pd_as', e_fn'))
      (<:patt<[]>>, <:expr<[]>>, [], [], (fun e -> e))
      (List.rev fields)
  in
  
  (* let ([f1; f2; f3; ...], hdr) = producer.Rec_ver.process_record source ["f1"; "f2"; "f3"; ...] in ... *)
  let deconstruct body = <:expr<let ($names_p$, hdr) = $record_pre$ $source$ $names_e$ in $body$>> in

  (* reconstruct an assignment into a PADS rep or pd:
	 
	 it takes a list of pairs: (field_name_as_patt,
	 field_name_as_expr).  this is the format that records take for
	 assignments.  a tuple assignment only needs the second part,
	 which is why 'snd' shows up 
  *)
  let reconstruct assgns = 
	match is_tuple, assgns with
		(* a tuple with one element, degenerate case *)
		(true, [e]) -> snd e 		  
		  (* a normal tuple -- we convert the list of pairs to just a list of variable references *)
	  | (true, assgns) -> <:expr<($list:List.map snd assgns$)>> 
		  (* a normal record -- create the assignment *)
	  | _ -> <:expr<{$list:assgns$}>>
  in
	
  (* reconstruct the rep:
	 { f1 = f1; f2 = f2; f3 = f3; ... }
	 or, in the case of a tuple,
	 (f1, f2, f3, ...)
  *)
  let rep = reconstruct rep_assignments in

  (* reconstruct the pd:
	 { f1_pd = f1_pd; f2_pd = f2_pd; f3_pd = f3_pd; ... }
	 or, in the case of a tuple,
	 (f1_pd, f2_pd, f3_pd, ...)
  *)
  let pd = reconstruct pd_assignments in

  (* the full reconstruction: (rep, (hdr, pd)) *)
  let reconstruction = <:expr<($rep$, (hdr, $pd$))>> in

  (* build up the body of the function in the right order: 
	 
	 deconstruct the result of process_record
	 process each of the children
	 reconstruct the PADS rep
  *)
  let body = deconstruct (process_children reconstruction) in
	<:expr<fun $source_pat$ -> $body$>> 

(* a wrapper around the record producer to convert empty tuples and records to punit *)
let gen_record_producer gp fields is_tuple =
  (* if there are no fields, just make a punit *)
  if fields = [] 
  then gp (SimpleAst.TyId (PadscId.makeid "Punit"))
  else gen_nonempty_record_producer gp fields is_tuple
	
let gen_datatype_producer gp variants =
  (* builds a pattern-matching clause for a given variant and an
	 optional type if the type is present, then it is a 'full'
     variant; otherwise, it is an 'empty' variant 

	 for the full variant "Foo x", we generate the following clause:

	 ("Foo", source) -> let foo = (... subtype producer calls ...) in Foo foo

	 for the empty variant "Bar", we make the following:

	 ("Bar", source) -> Bar
  *)
  let mk_variant (v, tp) =
	let v_s = PadscId.id2string v in
	let v_pd_s = v_s ^ "_pd" in

	(* ("Foo", source) -> *)
	let v_p = 
	  match tp with
		  Some tp -> <:patt<($str:v_s$, Some source)>>
		| None -> <:patt<($str:v_s$, None)>>
	in
	
	(* handled differently for full and empty variants... *)
	let v_e =       
	    match tp with
		(* full: let (v, pd) = (... ...) source in (Foo v, (hdr, Foo_pd pd)) *)
		Some tp -> 
		  if C.contains_s_host_tp tp then (
		    PadscError.report_warning loc 
		      "SProducerGenerator: host type used in branch; will not be producable at run time.";
		    <:expr<raise (Failure "This case cannot be produced.")>>
		  )
		  else	    
		    <:expr<let (v, pd) = $gp tp$ source in
		    ($uid:v_s$ v, (hdr, $uid:v_pd_s$ pd))>>
		  (* empty: (Foo, (hdr, Foo_pd))  *)
	      | None -> <:expr<($uid:v_s$, (hdr, $uid:v_pd_s$))>>
	in
	  (v_p, None, v_e)
  in
	(* build the outer matching clause:
		 
	   match Generic_producer.process_datatype source with
	   ("Foo", source) -> ...
	   | ("Bar", source) -> ...
	*)
  let cases = List.map mk_variant variants in
	<:expr<fun source -> 
	  let (rep, hdr) = $dt_pre$ source in
		match rep with [$list:cases$]>>

let rec gen_producer dt tc loc = function
	(* type application and type identifiers just need to have the
	   appropriate specialized producer built; then we can use the
	   derived 'produce' function *)
  | (SimpleAst.TyId _) 	  
  | (SimpleAst.TyApp _) as tp -> 
	  (* gen_specialize builds the appropriate specialize_producer
		 function; we then just pass in the producer *)
	  let spec_tool = <:expr<$gen_specialize dt tc loc tp$ $lid:N.producer_rec$>> in
		(* now that we have a specialized producer, pull out its produce function *)
		<:expr<$spec_tool$.$lid:N.producer_fun$>>
  | (SimpleAst.Table _) as tp -> 
	  let spec_tool = <:expr<$gen_specialize dt tc loc tp$ $lid:N.producer_rec$>> in
		<:expr<$spec_tool$.$lid:N.producer_fun$>>
  | SimpleAst.ValApp (tp, _) -> gen_producer dt tc loc tp

  (* both tuples and records can be treated the same way, once tuples have had their names munged *)
  | SimpleAst.Tuple tps -> gen_record_producer (gen_producer dt tc loc) (tuple_to_fields tps) true	  
  | SimpleAst.Record fields -> gen_record_producer (gen_producer dt tc loc) fields false
	  
  | SimpleAst.Datatype variants -> gen_datatype_producer (gen_producer dt tc loc) variants
	  
  | SimpleAst.Constraint (tp, id, _) -> 
	  let id_s = PadscId.id2string id in
		<:expr<fun $lid:id_s$ -> 
				 let ($lid:id_s$, sub_pd) = $gen_producer dt tc loc tp$ $lid:id_s$ in
				 let pd = $uid:N.pads_mod$.Where.$lid:N.genpd_fun$ sub_pd True (* hack, since constraints that reference value parameters will generate compile errors *) in
				   ($lid:id_s$, pd)>>
  | SimpleAst.HostTp _ -> PadscError.report_error loc "SLazyTraversalGenerator: arbitrary use of host types unsupported"

(** generates the specialized producer and returns its 'produce function *)
and gen_specialize dt tc loc = function
	(* we just need to look up the type, specialize it, and return the produce function *)
  | SimpleAst.TyId tid ->
	  (match D.lookup_descr dt tid with
		  None -> PadscError.report_error loc ("SProducerGenerator: Type " ^ (PadscId.id2string tid) ^ " not found.")
		| Some d -> C.gen_TidTp loc "SProducerGenerator" tid N.specialize_producer_fun d)
  
  (* apply the type application recursively; eventually we'll
	 bottom out in one of the other cases and return the produce
	 function *)
  | SimpleAst.TyApp (tp_fun, tp_args) -> 
      let apply_fun f arg = <:expr<$f$ $gen_specialize dt tc loc arg$>> in
      let f = gen_specialize dt tc loc (SimpleAst.TyId tp_fun) in
		List.fold_left apply_fun f tp_args 

  | SimpleAst.Table (id, _, _, keyt, _) -> 
      let apply_fun f arg = <:expr<$f$ $gen_specialize dt tc loc arg$>> in
      let f = gen_specialize dt tc loc (SimpleAst.TyId (PadscId.makeid "Ptable")) in
	List.fold_left apply_fun f [SimpleAst.TyId keyt; SimpleAst.TyId id]
		  
  (* type reference -- just generate and use its producer *)	  
  | tp ->
	  <:expr<fun $lid:N.producer_rec$ -> { $lid:N.producer_fun$ = $gen_producer dt tc loc tp$ }>>

let gen dt current_descr tc loc decl = 
  let (tp_params,name,val_param_opt,_, tp_def) = decl in
	
  let ty = SimpleAstTx.trans dt current_descr tc loc tp_def in
	
  let specialize_producer_body = 
	(* fun producer -> { producer = ... } *)
	<:expr<fun $lid:N.producer_rec$ ->
			 { $lid:N.producer_fun$ = $gen_producer dt tc loc ty$ }>>
  in

  let mk_specialize_producer_ty arg_repty arg_pdbty =
	<:ctyp<Generic_producer.Rec_ver.t 'source -> Type.SPProducer.producer 'source $arg_repty$>>
  in

  let poly_t   = C.package_funty mk_specialize_producer_ty tp_params mk_specialize_producer_ty in
  let poly_fun = C.package_fun specialize_producer_body tp_params N.specialize_producer_fun in
	
  let tp_pf_name = N.specialize_producer_fun in
	([],
	([<:sig_item<value $lid:tp_pf_name$ : $poly_t$>>], 
	 [<:str_item<open Type.SPProducer>>;
	  <:str_item<open Generic_producer>>;
	  <:str_item<value rec $lid:tp_pf_name$ = $poly_fun$>>]),
	current_descr)
