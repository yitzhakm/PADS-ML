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
  
(* Import the metadata type so as not to have to prefix each field
   name with "Description." *)
type metadata = D.metadata = {
  name : PadscId.id; (** The (visible) name of the description. *)
  kind : D.kind;
  tyclass: D.tyclass
}

(* We provide an empty context as we are type checking closed expressions. *)
(* XXX: It seems that this function is never used. *)
let type_of expr = HostLanguage.ctyp_of_tp
  (HostLanguage.infer_type HostLanguage.empty_ctxt expr)

(* Generate types for Pads types that can be nested. *)
let rec gen_types dt tc loc tp = 
  (* Call gen_types and wrap the pd_body type with a pd header. *)
  let gen_types_wrap tp = 
    let (rep_t,pd_body_t) = gen_types dt tc loc tp in
      (rep_t,C.mk_pd_t pd_body_t)
  in
  let gen_Tid tid = 
    match D.lookup_descr dt tid with
	None -> PadscError.report_error loc ("Type " ^ (PadscId.id2string tid) ^ " not found.")
      | Some {tyclass=D.Current} -> 
	  (<:ctyp<$lid:N.mk_top_level_rep_str tid$>>, 
	  <:ctyp<$lid:N.mk_top_level_pd_body_str tid$>>)
      | Some {tyclass=D.Ty_var} -> 
	  (<:ctyp<'$N.mk_rep_tyvar_str tid$>>, 
	  <:ctyp<'$N.mk_pd_body_tyvar_str tid$>>)
      | Some {tyclass=D.Defined} -> 
	  (C.rep_t tid, C.pd_body_t tid)
  in
  let rec gen_TpApp (tp_args,tp_fun) =
(*     let gen_tys = function *)
(* 	Ast.TidTp tid -> gen_Tid tid *)
(*       | Ast.TpAppTp (tp_args,tp_fun) -> gen_TpApp (tp_args,tp_fun) *)
(*       | _ -> PadscError.report_error loc "expected nested type application." *)
(*     in *)
    let gen_app_tys (r,pdb) arg_tp = 
      let rep_t,pd_body_t = gen_types dt tc loc arg_tp in
	<:ctyp<$r$ $rep_t$>>, <:ctyp<$pdb$ $pd_body_t$>> 
    in
    let tp_fun_tys = (<:ctyp<$lid:Names.mk_top_level_rep_str tp_fun$>>,
		     <:ctyp<$lid:Names.mk_top_level_pd_body_str tp_fun$>>)
    in
      match D.lookup_descr dt tp_fun with
	  None -> PadscError.report_error loc ("Type " ^ (PadscId.id2string tp_fun) ^ " not found.")
	| Some {kind=D.Fun(params_md);tyclass=D.Current} -> 
	    let rec check_args params_md args = 
	      match params_md, args with
		  [],[] -> ()
		| [],_ -> PadscError.report_error loc "Use does not match kind: too many args"
		| _,[] -> PadscError.report_error loc "Use does not match kind: too few args"
		| {name=n}::mds, (Ast.TidTp tid)::args when (PadscId.eqid n tid) -> check_args mds args
		| {name=n}::mds, (Ast.TidTp tid)::args -> 
		    PadscError.report_error loc ("Attempt to use non-uniform recursion. Expected " ^ (PadscId.id2string n)
					     ^ ", but found " ^ (PadscId.id2string tid) ^ ".")
		| {name=n}::mds, _ -> 
		    PadscError.report_error loc ("Attempt to use non-uniform recursion: expected type identifier " 
					     ^ (PadscId.id2string n) ^ ".")		       
	    in	       
	      (check_args params_md tp_args;
	       List.fold_left gen_app_tys tp_fun_tys tp_args)
	| Some {kind=D.Fun _;tyclass=D.Defined} ->	      
	    List.fold_left gen_app_tys tp_fun_tys tp_args 	     
	| Some {D.kind=D.Fun _; D.tyclass=D.Ty_var} -> 
	    (* XXX: This check can be removed once a kind checker is implemented. *)
	    PadscError.report_error loc ("TypeGenerator: Type variable " ^ (PadscId.id2string tp_fun) ^ " cannot be applied as all type variables must have base kind.")
	| Some _ ->	      
	    PadscError.report_error loc "Use does not match kind."
      
  in
    match tp with
	Ast.TupleTp ctps -> 
	  let do_ctp = function
	      Ast.Type tp -> [gen_types_wrap tp]
		(* XXX: The following is an optimization. It should
		   probably be done elsewhere. Here, we could treat the literals
		   like absorbed types and give them appropriate types. *)
	    | Ast.Exp e -> []
	  in
	    (match List.split (List.flatten (List.map do_ctp ctps)) with
		([],[]) -> 
		  (<:ctyp<unit>>, <:ctyp<unit>>)
	      | ([rep],[pd]) -> (rep,pd)
	      | (reps,pds) ->
		  (<:ctyp<($list:reps$)>>, <:ctyp<($list:pds$)>>))

      | Ast.TidTp tid -> gen_Tid tid
	  
      | Ast.ValAppTp (tp_fun,_) -> gen_types dt tc loc tp_fun
	  
      | Ast.TpAppTp (tp_args,tp_fun) -> gen_TpApp (tp_args,tp_fun)
	    
      | Ast.WhereTp (id,tp,exp) -> gen_types_wrap tp

      | _ -> PadscError.report_error loc "unsupported feature"
	    
(* Generate types for top-level types. *)
let gen_tp_types dt tc loc tp_body = 
  let gen_RecordTp_types fields =
    (* lsbt = loc * string * boolean * ctyp.  A list of these is
       needed to create a record type.  make_lsbts takes an id, rep,
       and pd types and generates the corresponding tuple of
       lsbts.*)
    (* As ocaml doesn't allow different records to
       share field names and can't nest anonymous records, 
       we define the pd type as follows: 
       
       type pd_body = {...}
       type pd = Pads.pd_hdr * pd_body
    *)
    let make_lsbts id (r,p) = 
      (loc,N.mk_rep_str  id,false,r),
      (loc,N.mk_pd_str   id,false,p)
    in
    let munge_fields = C.munge_fields 
      (fun _ -> []) 
      (fun (id,tp) -> let (rep_t,pd_body_t) = gen_types dt tc loc tp in	   
	 [make_lsbts id (rep_t, C.mk_pd_t pd_body_t)])
      (fun (id,tp,_) -> let (rep_t,pd_body_t) = gen_types dt tc loc tp in
	 [make_lsbts id (rep_t, C.mk_pd_t pd_body_t)])
      (fun _ -> []) 
    in
      match List.split (munge_fields fields) with
	  ([],[]) -> 
	    (<:ctyp<unit>>, <:ctyp<unit>>)
	| (reps,pds) ->
	    (<:ctyp<{$list:reps$}>>,
	     <:ctyp<{$list:pds$}>>) (* pd body *)
  in
    match tp_body with
      (* Records cannot be anonymous (nor, therefore, nested) and so
	 are only allowed at top level. *)
	Ast.RecordTp fields -> gen_RecordTp_types fields 
      | _ -> gen_types dt tc loc tp_body
	      
let gen_dtp_types dt tc loc = function
    Ast.ImplicitDT (variants, def_opt) -> 
      let gen_var = function
	  Ast.FullVar (id, tp) -> 
	    let (rep_t,pd_body_t) = gen_types dt tc loc tp in
	      ((loc,N.mk_rep_str id,[rep_t]),
	       (loc,N.mk_pd_str  id,[C.mk_pd_t pd_body_t]))
	| Ast.AbsorbVar (id,_) ->
	    ((loc,N.mk_rep_str id,[]),
	     (loc,N.mk_pd_str  id,[]))
      in 
      let (rep_vars,pd_vars) = List.split (List.map gen_var variants) in
      let (def_vt,def_pd_vt) = 
	match def_opt with
	    Some (Ast.GenDefault(id_opt,tp,e)) ->
	      let (rep_name,pd_name) = 
		match id_opt with
		    None -> N.def_vt,N.def_pd_vt
		  | Some id -> N.mk_rep_str id, N.mk_pd_str id
	      in
	      let (rep_t,pd_body_t) = gen_types dt tc loc tp in
		((loc,rep_name,[rep_t]),
		 (loc,pd_name,[C.mk_pd_t pd_body_t]))
	  | Some (Ast.FullDefault(id_opt,tp)) ->
	      let (rep_name,pd_name) = 
		match id_opt with
		    None -> N.def_vt,N.def_pd_vt
		  | Some id -> N.mk_rep_str id, N.mk_pd_str id
	      in
	      let (rep_t,pd_body_t) = gen_types dt tc loc tp in
		((loc,rep_name,[rep_t]),
		 (loc,pd_name,[C.mk_pd_t pd_body_t]))
	  | None -> (loc,N.err_vt,[]),(loc,N.err_pd_vt,[]) 
      in	
      let rep_t = <:ctyp< [$list:def_vt::rep_vars$] >> in
      let pd_body_t = <:ctyp< [$list:def_pd_vt::pd_vars$] >> in
	(rep_t,pd_body_t)
	
  | Ast.CaseDT (e,cases) -> 
      let gen_case  = function
	  Ast.FullCase (_,id,tp) ->  
	    let (rep_t,pd_body_t) = gen_types dt tc loc tp in
	      ((loc,N.mk_rep_str id,[rep_t]),
	       (loc,N.mk_pd_str  id,[C.mk_pd_t pd_body_t]))
	| Ast.AbsorbCase (_,id,_) ->  
	    ((loc,N.mk_rep_str id,[]),
	     (loc,N.mk_pd_str  id,[]))
	| Ast.GenCase (_,id,tp,e) ->
	    (* We only keep the rep type, as the value is not parsed but computed. *)
	    let (rep_t,pd_body_t) = gen_types dt tc loc tp in
	      ((loc,N.mk_rep_str id,[rep_t]),
	       (loc,N.mk_pd_str  id,[C.mk_pd_t pd_body_t]))
      in 
      let (rep_cases,pd_cases) = List.split (List.map gen_case cases) in
      let err_vt = (loc,N.err_vt,[]) in
      let err_pd_vt = (loc,N.err_pd_vt,[]) in	
      let rep_t = <:ctyp< [$list:err_vt::rep_cases$] >> in
      let pd_body_t = <:ctyp< [$list:err_pd_vt::pd_cases$] >> in
	(rep_t,pd_body_t)

let gen dt current_descr tc loc decl =
  let (tp_params,name,val_param_opt,tp_eq_opt, tp_def) = decl in

    (* extend environment with value params. *)
  let new_ctxt = C.process_val_lam tc val_param_opt in
  let (rep_t,pd_body_t) = 
    match tp_def with 
	Ast.TpDef tp_body -> gen_tp_types dt new_ctxt loc tp_body
      |	Ast.DtDef dtp_body -> gen_dtp_types dt new_ctxt loc dtp_body 
  in

  (* pair of booleans determines variance of type
     variable. false,false = none, true,true = none, true,false =
     - and false,true = positive.*)
  let id2rep_tyvar id = (Names.mk_rep_tyvar_str id,(false,false)) in
  let id2pdb_tyvar id = (Names.mk_pd_body_tyvar_str id,(false,false)) in
  let rep_name = Names.mk_top_level_rep_str name in
  let pd_body_name = Names.mk_top_level_pd_body_str name in
  let rep_tyvars = List.map id2rep_tyvar tp_params in
  let pdb_tyvars = List.map id2pdb_tyvar tp_params in

  let mk_rep_t r id = <:ctyp<$r$ '$N.mk_rep_tyvar_str id$>> in
  let rep_type = List.fold_left mk_rep_t <:ctyp<$lid:rep_name$>> tp_params in
  let mk_pd_body_t pb id = <:ctyp<$pb$ '$N.mk_pd_body_tyvar_str id$>> in
  let pd_body_type = List.fold_left mk_pd_body_t <:ctyp<$lid:pd_body_name$>> tp_params in
    (* internal pd_body type. used only within module. *)
  let pb_t = List.fold_left mk_pd_body_t <:ctyp<$lid:N.pd_body$>> tp_params in

  let sig_items = 
    [<:sig_item<type $lid:Names.rep$ $list:rep_tyvars$ = $rep_type$>>;
      <:sig_item<type $lid:Names.pd_body$ $list:pdb_tyvars$ = $pd_body_type$>>;
      <:sig_item<type $lid:Names.pd$ $list:pdb_tyvars$ = $C.mk_pd_t pb_t$>>] in
  let str_items =
    [<:str_item<type $lid:Names.rep$ $list:rep_tyvars$ = $rep_type$>>;
      <:str_item<type $lid:Names.pd_body$ $list:pdb_tyvars$ = $pd_body_type$>>;
      <:str_item<type $lid:Names.pd$ $list:pdb_tyvars$ = $C.mk_pd_t pb_t$>>] 
  in
  let tl_str_items = match tp_eq_opt with
      None ->
	[<:str_item<type $rep_name$ $list:rep_tyvars$  = $rep_t$>>;
	  <:str_item<type $pd_body_name$ $list:pdb_tyvars$ = $pd_body_t$>>]	      
    | Some tp_eq ->
	let rep_eq, pdb_eq = match tp_eq with
	    (None, t_id) -> 
	      <:ctyp<$lid:Names.mk_top_level_rep_str t_id$>>, 
	      <:ctyp<$lid:Names.mk_top_level_pd_body_str t_id$>>
	  | (Some mod_id, t_id) -> 
	      <:ctyp<$uid:PadscId.id2string mod_id$.$lid:Names.mk_top_level_rep_str t_id$>>, 
	      <:ctyp<$uid:PadscId.id2string mod_id$.$lid:Names.mk_top_level_pd_body_str t_id$>>
	in
	let rep_eq_t = List.fold_left mk_rep_t rep_eq tp_params in
	let pdb_eq_t = List.fold_left mk_pd_body_t pdb_eq tp_params in	  
	  [<:str_item<type $rep_name$ $list:rep_tyvars$  = $rep_eq_t$ == $rep_t$>>;
	    <:str_item<type $pd_body_name$ $list:pdb_tyvars$ = $pdb_eq_t$ == $pd_body_t$>>]	      

  in
    tl_str_items,(sig_items, str_items), current_descr
