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

(* Helper functions *)
let rid = N.mk_rep_str
let pid = N.mk_pd_str
let r_e id = <:expr<$lid:rid id$>>
let p_e id = <:expr<$lid:pid id$>>
let r_p id = <:patt<$lid:rid id$>>
let p_p id = <:patt<$lid:pid id$>>
let make_fresh s = PadscId.id2string (PadscId.freshid s)

(**************** Traversal FUNCTOR and Tool MODULE Version **********
 *****  We might eventually allow a compiler option to enable this 
 *****  compilation method.
let record_init = "init"
let record_start = "start"
let record_project = "project"
let record_process_field = "process_field"
let record_process_last_field = "process_last_field"
let record_pre = <:expr<$uid:N.tool_mod$.Record>>

let dt_init = "init"
let dt_start = "start"
let dt_project = "project"
let dt_process_variant = "process_variant"
let dt_init_empty = "init"
let dt_process_empty = "process"
let dt_pre = <:expr<$uid:N.tool_mod$.Datatype>>
let dt_empty_pre = <:expr<$uid:N.tool_mod$.Datatype.Empty>>

let con_init = "init"
let con_start = "start"
let con_project = "project"
let con_process = "process"
let con_pre = <:expr<$uid:N.tool_mod$.Constraint>>

(* Apply the tp_fun to its args, project out the traversal functor, 
   apply it to the tool and project from the result the specified value. *)
let proj_val_from_trav_func tp_fun tp_args proj_val =
  let get_trav_mod tp_mod_name =
    <:module_expr<$uid:tp_mod_name$.$uid:N.traversal_functor$ $uid:N.tool_mod$>> 
  in    
  let tp_mod = C.ty2mod_app tp_fun tp_args in
    C.bind_local_project tp_mod (fun tp_mod_name -> 
    C.bind_local_project (get_trav_mod tp_mod_name) (fun trav_mod_name ->
      <:expr<$uid:trav_mod_name$.$lid:proj_val$>>))

let init_from_tid tid =
  let mod_exp = 
    <:module_expr<$uid:PadscId.id2string tid$.$uid:N.traversal_functor$ 
      $uid:N.tool_mod$>> in
  let mod_id = PadscId.freshid "M" in
  let mod_name = PadscId.id2string mod_id in
    <:expr<let module $uid:mod_name$ = $mod_exp$ in 
	     $uid:mod_name$.$lid:N.init_fun$>>

let init_from_tpapp tp_fun tp_args = proj_val_from_trav_func tp_fun tp_args N.init_fun

let traverse_from_tid tid =
  let mod_exp = 
    <:module_expr<$uid:PadscId.id2string tid$.$uid:N.traversal_functor$ 
      $uid:N.tool_mod$>> in
  let mod_id = PadscId.freshid "M" in
  let mod_name = PadscId.id2string mod_id in
    <:expr<let module $uid:mod_name$ = $mod_exp$ in 
	     $uid:mod_name$.$lid:N.traversal_fun$>>

let traverse_from_tpapp tp_fun tp_args = proj_val_from_trav_func tp_fun tp_args N.traversal_fun

***********************************************************************)

let record_init = "r_init"
let record_start = "r_start"
let record_project = "r_project"
let record_process_field = "process_field"
let record_process_last_field = "process_last_field"
let record_pre = <:expr<$lid:N.tool_rec$.record_t>>

let dt_init = "dt_init"
let dt_start = "dt_start"
let dt_project = "dt_project"
let dt_process_variant = "process_variant"
let dt_process_empty_variant = "process_empty_variant"
let dt_pre = <:expr<$lid:N.tool_rec$.datatype_t>>
let dt_empty_pre = dt_pre

let con_init = "c_init"
let con_start = "c_start"
let con_project = "c_project"
let con_process = "c_process"
let con_pre = <:expr<$lid:N.tool_rec$.constraint_t>>

let spec_and_project spec_fun val_name =
  (* specialize the tool to the type. *)
  let spec_tool = <:expr<$spec_fun$ $lid:N.tool_rec$>> in
    (* project the value from the specialized tool record. *)
    <:expr<$spec_tool$.$lid:val_name$>>

let from_tid f_name loc tid d = 
  let specialize_fun = C.gen_TidTp loc "TraversalGenerator" tid N.specialize_fun d in
    spec_and_project specialize_fun f_name

let from_tpapp f_name tp_fun tp_args gen_f = 
  let applied_specialize_fun = C.gen_TpAppTp tp_fun tp_args gen_f in
    spec_and_project applied_specialize_fun f_name

(********************* init gen code ****************************)

let gen_record_init gi fields is_tuple = 

  (* Make the init function given a list of initialized sub-states. *)
  let fun_e fd_states = 
    <:expr<fun () -> $record_pre$.$lid:record_init$ $fd_states$>> in
 
  let process_field (id,tp) inits =
    let id_string = PadscId.id2string id in
    let fd_name = <:expr<$str:id_string$>> in
    let init_fn = gi tp in
      <:expr< [($fd_name$,$init_fn$ ()):: $inits$] >>
  in
  (* Make the list of fields inits. *)
    if fields = [] 
	then (* Its a unit. *)
      gi (SimpleAst.TyId (PadscId.makeid "Punit")) 
    else
	  let fields_inits = List.fold_right process_field fields <:expr< [] >> in
		fun_e fields_inits

let gen_record_traversal gt fields is_tuple = 

  (* list of field names in record. *)
  let names = List.map fst fields in

  (* The rep and pd parameter patterns of the traversal function *)
  let (reps_p, pds_p) = 
    (* The first two cases deal with empty and singleton tupels. These
       can arise do to the elimination of literals from tuples and
       records.  We need to treat them specially because camlp4 throws
       an exception on empty or singleton tuple patterns.  
       Singleton records appear to be okay.
    *)
    match names with
	[] -> <:patt<()>>, <:patt<()>>
      | [name] ->
	  if is_tuple then 
	    (<:patt<$r_p name$>>, <:patt<$p_p name$>>)
	  else 
	    let recrep_p = r_p name in
	    let recpd_p = p_p name in
	      (<:patt<{$list:[(recrep_p,recrep_p)]$}>>, 
	       <:patt<{$list:[(recpd_p,recpd_p)]$}>>)
      | _ ->
	  if is_tuple then 
	    (<:patt<($list:List.map r_p names$)>>, <:patt<($list:List.map p_p names$)>>)
	  else 
	    let recrep_p = List.map (fun n -> r_p n, r_p n) names in    
	    let recpd_p = List.map (fun n -> p_p n,p_p n) names in
	      (<:patt<{$list:recrep_p$}>>, <:patt<{$list:recpd_p$}>>)
  in

  let state_var = make_fresh "state" in
  let p_state_var = make_fresh "p_state" in

  (* Make the traversal function given a body. *)
  let fun_e body = <:expr<fun $reps_p$ (hdr,$pds_p$) $lid:state_var$ -> $body$>> in
 
  (* Make the start let expression given the remainder of the lets. *)
  let start_e the_rest = 
    <:expr<let $lid:p_state_var$ = $record_pre$.$lid:record_start$ $lid:state_var$ hdr in $the_rest$>> in

  let process_field (id,tp) (last,e) =
    let id_string = PadscId.id2string id in
    let s_p = <:patt<$lid:id_string ^ "_s"$>> in
    let s'_p = <:patt<$lid:id_string ^ "_s'"$>> in
    let s_e = <:expr<$lid:id_string ^ "_s"$>> in
    let s'_e = <:expr<$lid:id_string ^ "_s'"$>> in
    let fd_name = <:expr<$str:id_string$>> in
    let traversal_fn = gt tp in
    let process_fn_name = if last then record_process_last_field else record_process_field in
      (false,
      <:expr<
        let $s_p$ = $record_pre$.$lid:record_project$ $lid:state_var$ $fd_name$ in	  
	let $s'_p$ = $traversal_fn$ $r_e id$ $p_e id$ $s_e$ in
	let $lid:p_state_var$ = $record_pre$.$lid:process_fn_name$ $lid:p_state_var$ $fd_name$ $s'_e$ in
	  $e$>>)
  in
    if names = [] 
	then 
      gt (SimpleAst.TyId (PadscId.makeid "Punit")) 
	else
      (* Make the lets to process the fields, given the final expression. *)
      let (_,process_fields) = List.fold_right process_field fields (true,<:expr<$lid:p_state_var$>>) in
		fun_e (start_e process_fields)

let tuple_to_fields tys =
  let rid i = "elt"^(string_of_int i) in
  (* Convert a tuple-element AST node to a record-field AST node. *)
  let convert (i,fds) ty = i+1,(PadscId.makeid (rid i),ty)::fds in
  let _,fields_rev = List.fold_left convert (1,[]) tys in
    List.rev fields_rev

let rec gen_init dt tc loc = function 

  | (SimpleAst.TyId tid) as tp-> 
      let specialize_fun = gen_specialize dt tc loc tp in
	spec_and_project specialize_fun N.init_fun

  | (SimpleAst.TyApp _) as tp-> 
      let applied_specialize_fun = gen_specialize dt tc loc tp in
	spec_and_project applied_specialize_fun N.init_fun
	
  | SimpleAst.ValApp (tp, _) -> gen_init dt tc loc tp

  | SimpleAst.Tuple tps ->
      gen_record_init (gen_init dt tc loc) (tuple_to_fields tps) true	    
	  
  | SimpleAst.Record fields -> 
      gen_record_init (gen_init dt tc loc) fields false

  | SimpleAst.Datatype variants -> <:expr<fun () -> $dt_pre$.$lid:dt_init$ () >> 

  | SimpleAst.Constraint (tp, _, _) ->	
      let init_fn = gen_init dt tc loc tp in
	<:expr<fun () -> $con_pre$.$lid:con_init$ ($init_fn$ ())>>

and gen_traversal dt tc loc = function 

  | (SimpleAst.TyId _) as tp -> 
      let specialize_fun = gen_specialize dt tc loc tp in
	spec_and_project specialize_fun N.traversal_fun

  | (SimpleAst.TyApp _) as tp-> 
      let applied_specialize_fun = gen_specialize dt tc loc tp in
	spec_and_project applied_specialize_fun N.traversal_fun

  | SimpleAst.ValApp (tp, _) -> gen_traversal dt tc loc tp
		  
  | SimpleAst.Tuple tps ->
      gen_record_traversal (gen_traversal dt tc loc) (tuple_to_fields tps) true
	  
  | SimpleAst.Record fields -> 
      gen_record_traversal (gen_traversal dt tc loc) fields false

  | SimpleAst.Datatype variants -> 
      (* Build the traversal expression for a variant with a subcomponent. *)
      let mk_full_expr rep_con pd_con sub_tp = 
	let vpatt = <:patt<($uid:rep_con$ r,$uid:pd_con$ pd)>> in
	let vexpr = <:expr<
	  let s_opt = $dt_pre$.$lid:dt_project$ state $str:rep_con$ in
	  let s = match s_opt with 
	      [ Some s -> s 
	      | None -> $gen_init dt tc loc sub_tp$ ()]
	  in
	  let s' = $gen_traversal dt tc loc sub_tp$ r pd s in
	    $dt_pre$.$lid:dt_process_variant$ p_state $str:rep_con$ s'
	    >>
	in
	  (vpatt,None,vexpr)
      in
	
      (* Build the traversal expression for a variant with no subcomponent *)
      let mk_absorb_expr rep_con pd_con = 
	let vpatt = <:patt<($uid:rep_con$,$uid:pd_con$)>> in
	let vexpr = <:expr<
	  $dt_pre$.$lid:dt_process_empty_variant$ p_state $str:rep_con$
	  >>
	in
	  (vpatt,None,vexpr)
      in
	
      let gen_variant (id,tp_opt) = match tp_opt with
	  None -> mk_absorb_expr (N.mk_rep_str id) (N.mk_pd_str id) 
	| Some tp -> mk_full_expr (N.mk_rep_str id) (N.mk_pd_str id) tp 
      in
      let all_cases = List.map gen_variant variants in
	<:expr<fun r (hdr,pd_body) state -> 
	  let p_state = $dt_pre$.$lid:dt_start$ state hdr in	  	    
	    match (r,pd_body) with [$list:all_cases$]>>

  | SimpleAst.Constraint (tp, _, _) ->	
      let traversal_fn = gen_traversal dt tc loc tp in
	<:expr<fun r (hdr,pd) state ->
	  let p_state = $con_pre$.$lid:con_start$ state hdr in	
	  let s = $con_pre$.$lid:con_project$ state in	  
	  let s' = $traversal_fn$ r pd s in
	    $con_pre$.$lid:con_process$ p_state s'>>
	    
and gen_specialize dt tc loc = function 

  | SimpleAst.TyId tid ->
      (match D.lookup_descr dt tid with
	   None -> PadscError.report_error loc ("TraversalGenerator: Type " ^ (PadscId.id2string tid) ^ " not found.")
	 | Some d -> C.gen_TidTp loc "TraversalGenerator" tid N.specialize_fun d)

  | SimpleAst.TyApp (tp_fun, tp_args) -> 
      let apply_fun f arg = <:expr<$f$ $gen_specialize dt tc loc arg$>> in
      let f = gen_specialize dt tc loc (SimpleAst.TyId tp_fun) in
	  List.fold_left apply_fun f tp_args 
	
  | tp -> 
      let init_fun = gen_init dt tc loc tp in
      let traversal_fun = gen_traversal dt tc loc tp in
      let tool_rec_e = <:expr< 
	let rec $lid:N.init_fun$ = $init_fun$ 
	and $lid:N.traversal_fun$ = $traversal_fun$ in
	  {$lid:N.init_fun$ = $lid:N.init_fun$;
	   $lid:N.traversal_fun$ = $lid:N.traversal_fun$}>>
      in
	<:expr<fun $lid:N.tool_rec$ -> $tool_rec_e$>>
	    
(********************* traversal gen code ****************************)


let gen dt current_descr tc loc decl = 
  let (tp_params,name,val_param_opt,_, tp_def) = decl in

  let ty = SimpleAstTx.trans dt current_descr tc loc tp_def in

  (* Generate init and traverse functions *)
  let init_fun  = gen_init dt tc loc ty in
  let traversal_fun = gen_traversal dt tc loc ty in
 
  let specialize_tool_body = 
    let tool_rec_e = <:expr< 
      let rec $lid:N.init_fun$ = $init_fun$ 
      and $lid:N.traversal_fun$ = $traversal_fun$ in
	{$lid:N.init_fun$ = $lid:N.init_fun$;
	 $lid:N.traversal_fun$ = $lid:N.traversal_fun$}>>
    in
      <:expr<fun $lid:N.tool_rec$ -> $tool_rec_e$>>
  in

  let mk_specialize_tool_ty arg_repty arg_pdbty = 
    <:ctyp<Generic_tool.Rec_ver.t 'state 'rps 'dps 'cps 'lps
    ->  Type.SPTraversal.tool $arg_repty$ $arg_pdbty$ 'state>>
  in

  let poly_t   = C.package_funty mk_specialize_tool_ty tp_params mk_specialize_tool_ty in
  let poly_fun = C.package_fun specialize_tool_body tp_params N.specialize_fun in
    
  let tp_pf_name = N.specialize_fun in
    (* create the final function *)
    ([], 
    ([<:sig_item<value $lid:tp_pf_name$ : $poly_t$>>],
    [<:str_item<open Generic_tool.Rec_ver>>;
     <:str_item<open Type.SPTraversal>>;
     <:str_item<value rec $lid:tp_pf_name$  = $poly_fun$ >>]),
    current_descr)
