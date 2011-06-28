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
type metadata = D.metadata = {
  name : PadscId.id; (** The (visible) name of the description. *)
  kind : D.kind;
  tyclass: D.tyclass
}

(* Generate types for Pads types that can be nested. *)
let rec gen_types dt tc loc tp = 
  (* Call gen_types and wrap the pd_body type with a pd header. *)
  let gen_types_wrap tp = 
    let (rep_t,pd_body_t) = gen_types dt tc loc tp in
      (rep_t,C.mk_pd_t pd_body_t)
  in
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
    let trans_field (id,tp) =
      let (rep_t,pd_body_t) = gen_types dt tc loc tp in	   
	make_lsbts id (rep_t, C.mk_pd_t pd_body_t)
    in
      match List.split (List.map trans_field fields) with
	  ([],[]) -> 
	    (<:ctyp<unit>>, <:ctyp<unit>>)
	| (reps,pds) ->
	    (<:ctyp<{$list:reps$}>>,
	     <:ctyp<{$list:pds$}>>) (* pd body *)
  in
  let gen_TpTable (tp_key, tp_item) =
	let tp_fun = PadscId.makeid "Ptable" in
  	let tp_fun_rep = <:ctyp<$lid:Names.mk_top_level_rep_str tp_fun$>> in
	let tp_fun_pdb = <:ctyp<$lid:Names.mk_top_level_pd_body_str tp_fun$>> in 
	let key_rep_t, key_pd_body_t = gen_types dt tc loc tp_key in
	let item_rep_t, item_pd_body_t = gen_types dt tc loc tp_item in
    	let gen_app_tys (r,pdb) (rep_t, pd_body_t) = 
		<:ctyp<$r$ $rep_t$>>, <:ctyp<$pdb$ $pd_body_t$>> in
	let (myrep_t, mypd_t) = List.fold_left gen_app_tys (tp_fun_rep, tp_fun_pdb)
		[(key_rep_t, key_rep_t); (item_rep_t, key_pd_body_t)] in
	let mypd_t = <:ctyp<$mypd_t$ $item_pd_body_t$>> in
	(myrep_t, mypd_t)
  in
  let rec gen_TpApp (tp_fun, tp_args) =
    let gen_app_tys (r,pdb) arg_tp = 
      let rep_t,pd_body_t = gen_types dt tc loc arg_tp in
	<:ctyp<$r$ $rep_t$>>, <:ctyp<$pdb$ $pd_body_t$>> 
    in
    let tp_fun_tys = (<:ctyp<$lid:Names.mk_top_level_rep_str tp_fun$>>,
		     <:ctyp<$lid:Names.mk_top_level_pd_body_str tp_fun$>>) 
    in
    (*let _ = PadscError.report_error loc ("TypeApp: " ^ (PadscId.id2string tp_fun) )  
    in *)
      match D.lookup_descr dt tp_fun with
	  None -> PadscError.report_error loc ("Type " ^ (PadscId.id2string tp_fun) ^ " not found.")
	| Some {kind=D.Fun(params_md);tyclass=D.Current} -> 
	    let rec check_args params_md args = 
	      match params_md, args with
		  [],[] -> ()
		| [],_ -> PadscError.report_error loc "Use does not match kind: too many args"
		| _,[] -> PadscError.report_error loc "Use does not match kind: too few args"
		| {name=n}::mds, (SimpleAst.TyId tid)::args when (PadscId.eqid n tid) -> check_args mds args
		| {name=n}::mds, (SimpleAst.TyId tid)::args -> 
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
	    PadscError.report_error loc ("Type " ^  (PadscId.id2string tp_fun) ^ " does not match kind.")
      
  in
    match tp with
	SimpleAst.Tuple tps -> 
	  (match List.split (List.map gen_types_wrap tps) with
	      ([],[])      -> (<:ctyp<unit>>, <:ctyp<unit>>)
	    | ([rep],[pd]) -> (rep,pd)
	    | (reps,pds)   -> (<:ctyp<($list:reps$)>>, <:ctyp<($list:pds$)>>))

      | SimpleAst.TyId tid -> 
	  (match D.lookup_descr dt tid with
	      None -> PadscError.report_error loc ("Type " ^ (PadscId.id2string tid) ^ " not found.")
	    | Some {tyclass=D.Current} -> 
		(<:ctyp<$lid:N.mk_top_level_rep_str tid$>>, 
		<:ctyp<$lid:N.mk_top_level_pd_body_str tid$>>)
	    | Some {tyclass=D.Ty_var} -> 
		(<:ctyp<'$N.mk_rep_tyvar_str tid$>>, 
		<:ctyp<'$N.mk_pd_body_tyvar_str tid$>>)
	    | Some {tyclass=D.Defined} -> 
		(C.rep_t tid, C.pd_body_t tid))
	  
      | SimpleAst.TyApp (tp_fun, tp_args) -> gen_TpApp (tp_fun, tp_args)
	    
      | SimpleAst.ValApp (tp, _) -> gen_types dt tc loc tp

      | SimpleAst.Record fields -> gen_RecordTp_types fields 

      | SimpleAst.Constraint (tp, _, _) -> gen_types_wrap tp

      | SimpleAst.Datatype variants -> 
	  let gen_variant (id,tp_opt) = match tp_opt with
	      Some tp -> 
		let (rep_t,pd_body_t) = gen_types dt tc loc tp in
		  ((loc,N.mk_rep_str id,[rep_t]),
		  (loc,N.mk_pd_str  id,[C.mk_pd_t pd_body_t]))
	    | None ->
		((loc,N.mk_rep_str id,[]),
		(loc,N.mk_pd_str  id,[]))
	  in 
	  let (rep_vars,pd_vars) = List.split (List.map gen_variant variants) in
	  let rep_t = <:ctyp< [$list:rep_vars$] >> in
	  let pd_body_t = <:ctyp< [$list:pd_vars$] >> in
	    (rep_t,pd_body_t)
      | SimpleAst.Table (item_tp, _, _, tyid, _) -> gen_TpTable (SimpleAst.TyId tyid, SimpleAst.TyId item_tp)
	  
      | SimpleAst.HostTp t -> (HL.ctyp_of_tp t, <:ctyp<unit>>)

      | _ -> PadscError.report_error loc "STypeGenerator: unsupported feature"

let check_table ty =
  match ty with	    
      SimpleAst.Table (item_tp, _, _, tyid, path) -> 
	(Some tyid, Some item_tp, Some path)
     | _ -> (None, None, None)

let gen dt current_descr tc loc decl =
  let (tp_params,name,val_param_opt,tp_eq_opt, tp_def) = decl in

  let ty = SimpleAstTx.trans dt current_descr tc loc tp_def in
  (* check if this ty is a table and if so get the key type, item type and key path *)
  let (keytype_op, itemtype_op, path_op) = check_table ty in

    (* extend environment with value params. *)
  let new_ctxt = C.process_val_lam tc val_param_opt in
  let (rep_t,pd_body_t) = gen_types dt new_ctxt loc ty in

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
    match (keytype_op, itemtype_op, path_op) with 
    (Some keytype, Some id, Some path) -> 
    let path_str = N.mk_rep_str path 
    and path_pd_str = N.mk_pd_str path in
    [<:str_item<type $lid:Names.rep$ $list:rep_tyvars$ = $rep_type$>>;
      <:str_item<type $lid:Names.pd_body$ $list:pdb_tyvars$ = $pd_body_type$>>;
      <:str_item<type $lid:Names.pd$ $list:pdb_tyvars$ = $C.mk_pd_t pb_t$>>;
      <:str_item<value $lid:N.getkey_fun$ (x : $C.rep_t id$) : $C.rep_t keytype$ = x.$lid:path_str$ >>;
      <:str_item<value $lid:N.getpd_fun$ ((_, x) : $C.pd_t id$) : $C.pd_t keytype$ = x.$lid:path_pd_str$ >>
    ] 
    | _ ->
    [<:str_item<type $lid:Names.rep$ $list:rep_tyvars$ = $rep_type$>>;
      <:str_item<type $lid:Names.pd_body$ $list:pdb_tyvars$ = $pd_body_type$>>;
      <:str_item<type $lid:Names.pd$ $list:pdb_tyvars$ = $C.mk_pd_t pb_t$>>;
    ] 
  in
  let tl_str_items = match tp_eq_opt with
      None ->
      (
	  [<:str_item<type $rep_name$ $list:rep_tyvars$  = $rep_t$>>;
	  <:str_item<type $pd_body_name$ $list:pdb_tyvars$ = $pd_body_t$>>;
	  ]
      )
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
