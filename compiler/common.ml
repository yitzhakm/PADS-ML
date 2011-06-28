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
module D = Description
module N = Names

exception Unsupported_feature
exception Internal_error

(* filename (basename) of the .pml file that's being compiled.

   should be set only by set_global_source_file, which scrubs the
   filename of possibly bad characters
*)
let global_source_file = ref ""

let set_global_source_file filename =
  let scrub_filename f =
	(* remove any preceeding directory information *)
	let f = Filename.basename f in
	  
	let illegal_chars = Str.regexp "[^a-zA-Z0-9._-]" in
	  Str.global_replace illegal_chars "_" f
  in
	global_source_file := scrub_filename filename

(** Dummy location provided as default of quotations. Quotations that
    want to specify their own location should just locally rebind
    loc. The use of a dummy loc is ok as all of the locs in the ast
    are replaced by camlp4 once the ast is returned at the top
    level.*)
let loc = Ploc.dummy

let global_loc = ref loc

let rep_t  id = <:ctyp<$uid:PadscId.id2string id$.$lid:Names.rep$>>
let pd_body_t   id = <:ctyp<$uid:PadscId.id2string id$.$lid:Names.pd_body$>>
let pd_t   id = <:ctyp<$uid:PadscId.id2string id$.$lid:Names.pd$>>

(* MLast expressions *)

(** a pads function expression, parameterized by type id and function name. *)
let fun_e tid f_name = <:expr<$uid:PadscId.id2string tid$.$lid:f_name$>>

(** parse function expression, parameterized by type id. *)
let parser_e tid = fun_e tid Names.parser_fun

(** pads handle *)
let pads_e = <:expr<$lid:Names.pads_handle$>>

(** make an expression from an id *)
let id_e id  = <:expr<$lid:PadscId.id2string id$>>

(* MLast patterns *)

(** pads handle *)
let pads_p = <:patt<$lid:Names.pads_handle$>>

(** make a pattern from an id *)
let id_p id = <:patt<$lid:PadscId.id2string id$>>

(* Pads library content *)

(** pads module expression *)
let pads_mod = <:expr<$uid:N.pads_mod$>>

(** pads where module expression *)
let pads_where_mod = <:expr<$uid:N.pads_mod$.Where>>

(** pads record module expression *)
let pads_record_mod = <:expr<$uid:N.pads_mod$.Record>>

(** pads datatype module expression *)
let pads_dt_mod = <:expr<$uid:N.pads_mod$.Datatype>>

(* types *)
let pd_header_t = <:ctyp<$uid:N.pads_mod$.$lid:N.pd_header_t$>>
let base_pd_t   = <:ctyp<$uid:N.pads_mod$.$lid:N.base_pd_t$>>
let handle_t    = <:ctyp<$uid:N.pads_mod$.$lid:N.handle_t$>>

(* functions *)
let get_pd_hdr = <:expr<$pads_mod$.$lid:N.get_pd_hdr$>>

(* Utility functions *)

(** wrap a pd body type with a header *)
let mk_pd_t body_t = <:ctyp<$uid:N.pads_mod$.pd $body_t$>>

let rev_flatten l = 
  let rec rf new_l = function 
      [] -> new_l
    | (l::ls) -> rf (l @ new_l) ls
  in rf [] l

let split3 ts = 
  let rec do_split (xs,ys,zs) = function
      [] -> (List.rev xs, List.rev ys, List.rev zs)
    | ((x,y,z)::ts) -> do_split (x::xs,y::ys,z::zs) ts
  in
    do_split ([],[],[]) ts

let rev_split3 ts = 
  let rec do_split (xs,ys,zs) = function
      [] -> (xs,ys,zs)
    | ((x,y,z)::ts) -> do_split (x::xs,y::ys,z::zs) ts
  in
    do_split ([],[],[]) ts

let munge_fields a f g l =
  let munge_field = function
      Ast.AbsorbField af -> a af
    | Ast.FullField ff -> f ff
    | Ast.GenField gf -> g gf
    | Ast.LetField lf -> l lf
  in
  let rec mfs = function
      [] -> []
    | fd::fds -> let m = munge_field fd in (m @ (mfs fds))
  in 
    mfs

(** init is the initial state *)
let munge_fields_state a f g l init =
  let munge_field = function
      Ast.AbsorbField af -> a af
    | Ast.FullField ff -> f ff
    | Ast.GenField gf -> g gf
    | Ast.LetField lf -> l lf
  in
  let rec mfs s = function
      [] -> []
    | fd::fds -> let m,s' = munge_field fd s in (m @ (mfs s' fds))
  in 
    mfs init

(** Like List.rev_map, its a tail recursive version of munge_fields,
which is equivalent to List.rev o munge_fields*)
let rev_munge_fields a f g l =
  let munge_field = function
      Ast.AbsorbField af -> a af
    | Ast.FullField ff -> f ff
    | Ast.GenField gf -> g gf
    | Ast.LetField lf -> l lf
  in
  let rec mfs munged_fields = function
      [] -> munged_fields
    | fd::fds -> mfs (munge_field fd @ munged_fields) fds
  in 
    mfs []

let gen_TidTp loc ~calling_ctxt_name tid ~f_name = function
    {D.tyclass=D.Current} -> <:expr<$lid:f_name$>>
  | {D.tyclass=D.Ty_var} -> <:expr<$lid:N.mk_fun_tyvar_str tid f_name$>>
  | {D.tyclass=D.Defined} -> fun_e tid f_name

(* convert a type application to a function application. *)
let ty2fun_app tp_fun tp_args gen_f = 
  let apply_fun f arg = <:expr<$f$ $gen_f arg$>> in
  let f = gen_f (Ast.TidTp tp_fun) in
    List.fold_left apply_fun f tp_args 

let gen_TpAppTp = ty2fun_app
 
let ty2mod tp = <:module_expr<$uid:PadscId.id2string tp$>>

(* convert a type application to a module application. *)
let rec gen_tyapp_exp = function
    Ast.TidTp tid -> <:module_expr<$uid:PadscId.id2string tid$>>
  | Ast.TpAppTp (tp_args,tp_fun) -> ty2mod_app tp_fun tp_args
  | _ -> PadscError.report_error loc "expected nested type application."
and apply_fun f arg = <:module_expr<$f$ $gen_tyapp_exp arg$>> 
and ty2mod_app tp_fun tp_args = List.fold_left apply_fun 
  <:module_expr<$uid:PadscId.id2string tp_fun$>> tp_args 

let bind_local_project mod_expr proj_fn =
  let mod_id = PadscId.freshid "M" in
  let mod_name = PadscId.id2string mod_id in
    <:expr<let module $uid:mod_name$ = $mod_expr$ in $proj_fn mod_name$>>

let apply_project tp_fun tp_args proj_str = 
  bind_local_project (ty2mod_app tp_fun tp_args) (fun m -> <:expr<$uid:m$.$lid:proj_str$>>)

let process_tp_lam dt ids = dt
let process_val_lam tp_ctxt vp_opt = tp_ctxt

let package_funty mk_body_ty tp_params mk_arg_ty =
  let id2funty id body_t =
    let arg_repty = <:ctyp<'$N.mk_rep_tyvar_str id$>> in
    let arg_pdbty = <:ctyp<'$N.mk_pd_body_tyvar_str id$>> in
    let arg_ty = mk_arg_ty arg_repty arg_pdbty in
      <:ctyp<$arg_ty$ -> $body_t$>>
  in 

  let mk_rep_t rep_t id = <:ctyp<$rep_t$ '$N.mk_rep_tyvar_str id$>> in
  let rep_t = List.fold_left mk_rep_t <:ctyp<$lid:N.rep$>> tp_params in

  let mk_pdb_t pdb_t id = <:ctyp<$pdb_t$ '$N.mk_pd_body_tyvar_str id$>> in
  let pdb_t = List.fold_left mk_pdb_t <:ctyp<$lid:N.pd_body$>> tp_params in

  let fun_body_t = mk_body_ty rep_t pdb_t in
    List.fold_right id2funty tp_params fun_body_t

let package_fun fun_body tp_params fun_name =
  let id2fun id body = <:expr<fun $lid:N.mk_fun_tyvar_str id fun_name$ -> $body$>> in
    List.fold_right id2fun tp_params fun_body 

let rec contains_host_tp = function
    Ast.TupleTp ctps -> List.exists 
      (function (Ast.Type tp) -> contains_host_tp tp | _ -> false)
      ctps      
  | Ast.HostTp _ -> true
  | tp -> false

let rec contains_host_tp = function
    Ast.TupleTp ctps -> List.exists 
      (function (Ast.Type tp) -> contains_host_tp tp | _ -> false)
      ctps      
  | Ast.HostTp _ -> true
  | tp -> false

let rec contains_s_host_tp = function
    SimpleAst.Tuple tps -> List.exists contains_s_host_tp tps      
  | SimpleAst.HostTp _ -> true
  | tp -> false

let convert_t2r rid ctps =
  let convert (i,fds) = function
(*       Ast.Type (Ast.HostTp t) -> i,fds (\* ignore the field *\) *)
    | Ast.Type tp -> i+1,Ast.FullField(PadscId.makeid (rid i),tp)::fds
    | (Ast.Exp exp) as a -> i,(Ast.AbsorbField a)::fds
  in
  List.rev (snd (List.fold_left convert (1,[]) ctps))

(* needs commments about metadata. *)
type core_phase = 
    Description.table -> Description.metadata
  -> HostLanguage.tp_ctxt
  -> MLast.loc
  -> Ast.tp_def 
  -> (MLast.sig_item list * MLast.str_item list * Description.metadata)
  
module type COMP_PHASE = 
sig
  val gen : core_phase
end

type core_phase_v2 = 
    Description.table -> Description.metadata
  -> HostLanguage.tp_ctxt
  -> MLast.loc
  -> Ast.decl 
  (* Top-level declarations, followed by the module-level items. *)
  -> MLast.str_item list * (MLast.sig_item list * MLast.str_item list) * Description.metadata
  
module type COMP_PHASE_v2 = 
sig
  val gen : core_phase_v2
end

let cp_upgrade_1to2 cp1 = 
  fun dt current_descr tc loc (_,_,_,_, def) -> 
    let s,st,m =  cp1 dt current_descr tc loc def in
      [],(s,st),m

(*       produce signatures? -> camlp4 loc -> whole ast -> Ocaml top level items *)
type whole_ast_phase = bool -> MLast.loc -> Ast.pml_ast -> MLast.str_item list

module type COMP_PHASE_WHOLE_AST_PRE =
sig
  val whole_ast_pre_gen : whole_ast_phase
end

module type COMP_PHASE_WHOLE_AST_POST =
sig
  val whole_ast_post_gen : whole_ast_phase
end
  
