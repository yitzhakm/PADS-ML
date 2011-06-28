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
exception Unsupported_feature
exception Internal_error

val global_source_file : string ref
val set_global_source_file : string -> unit

val global_loc : MLast.loc ref

val rep_t  : PadscId.id  -> MLast.ctyp
val pd_body_t : PadscId.id  -> MLast.ctyp
val pd_t   : PadscId.id  -> MLast.ctyp

(** wrap a pd body type with a header *)
val mk_pd_t : MLast.ctyp -> MLast.ctyp

(* MLast expressions *)

(** parse function expression, parameterized by type id. *)
val parser_e : PadscId.id -> MLast.expr

(** pads handle *)
val pads_e : MLast.expr

(** make an expression from an id. id must start with lowercase letter. *)
val id_e : PadscId.id -> MLast.expr

(* MLast patterns *)

(** pads handle *)
val pads_p : MLast.patt

(** make a pattern from an id *)
val id_p : PadscId.id -> MLast.patt

(* Pads library content *)

(** pads module expression *)
val pads_mod : MLast.expr

(** pads record module expression *)
val pads_record_mod : MLast.expr

(** pads where module expression *)
val pads_where_mod : MLast.expr

(** pads datatype module expression *)
val pads_dt_mod : MLast.expr

(* types *)
val pd_header_t   : MLast.ctyp
val base_pd_t     : MLast.ctyp
val handle_t      : MLast.ctyp

(* functions *)
val get_pd_hdr : MLast.expr

(* Utility functions *)

val rev_flatten : 'a list list -> 'a list
val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val rev_split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list

(* munge fields from beginning to end of list. Not tail recursive.*)
val munge_fields :
  (Ast.comp_tp -> 'a list) ->
  (Ast.id * Ast.tp -> 'a list) ->
  (Ast.id * Ast.tp * Ast.exp -> 'a list) -> 
  (Ast.id  * Ast.exp -> 'a list) ->
  Ast.field list -> 'a list

(* munge fields from beginning to end of list. Not tail recursive.
   Thread abstract state from beginning to end.
*)
val munge_fields_state :
  (Ast.comp_tp -> 's -> 'a list * 's) ->
  (Ast.id * Ast.tp -> 's -> 'a list * 's) ->
  (Ast.id * Ast.tp * Ast.exp -> 's -> 'a list *'s) -> 
  (Ast.id  * Ast.exp -> 's -> 'a list * 's) ->
  's ->
  Ast.field list -> 'a list

(** Like List.rev_map, its a tail recursive version of munge_fields,
    which is equivalent to List.rev o munge_fields*)
val rev_munge_fields :
  (Ast.comp_tp -> 'a list) ->
  (Ast.id * Ast.tp -> 'a list) ->
  (Ast.id * Ast.tp * Ast.exp -> 'a list) -> 
  (Ast.id  * Ast.exp -> 'a list) ->
  Ast.field list -> 'a list

(** Generate code corresponding to a type identifier. *)
val gen_TidTp : MLast.loc -> calling_ctxt_name:string -> 
  PadscId.id -> f_name:string -> Description.metadata -> MLast.expr

(** Generate code corresponding to a type application. 
    @arguments  
    The first argument generates expressions for nested types. 
    The second argument is the identifier of the type function.
    The third argument is the list of type arguments.
*)
val gen_TpAppTp :  PadscId.id -> Ast.tp list -> (Ast.tp -> MLast.expr) -> MLast.expr

(** Convert a type application to a function application, using the first argument
    to generate expressions for nested types. 
    The second argument is the identifier of the type function.
    The third argument is the list of type arguments.
*)
val ty2fun_app : Ast.id -> Ast.tp list -> (Ast.tp -> MLast.expr) -> MLast.expr
(** Convert a type name to a module expression. *)
val ty2mod : PadscId.id -> MLast.module_expr

(** Convert a type application to a module application. *)
val ty2mod_app : PadscId.id -> Ast.tp list -> MLast.module_expr

(** Bind the module expression to a fresh module variable and pass the
    name of the fresh variable to the supplied function to create an expr. *)
val bind_local_project : MLast.module_expr -> (string -> MLast.expr) -> MLast.expr

(** Apply a functor to its arguments (potentially containing nested
    applications), and then project out the specified value. 
    Assumes that third argument is a lower-case identifier. 
    
    This function is a composition of ty2mod_app and bind_local_project.
*)
val apply_project : Ast.id -> Ast.tp list -> string -> MLast.expr


val process_tp_lam: Description.table -> Ast.id list 
  -> Description.table
val process_val_lam: HostLanguage.tp_ctxt -> Ast.val_param option 
  -> HostLanguage.tp_ctxt

(** Add (pads-type) parameter types to the type of PADS function.

    The first parameter is a function to construct the type of the
    function from rep and pd-body type names.

    The second parameter is a list of parameter names.

    The third parameter is a function to construct the type of any
    parameter from its rep and pd-body type names.

    For monomorphic types, this function returns the first
    argument. Otherwise, it returns a function type prefixed by
    arguments whose types are given by the third parameter.
*)
val package_funty : 
  (MLast.ctyp -> MLast.ctyp -> MLast.ctyp)
  -> Ast.id list
  -> (MLast.ctyp -> MLast.ctyp -> MLast.ctyp)
  -> MLast.ctyp 

(** Add (pads-type) parameters to the PADS function.

    The first parameter is the function body.

    The second parameter is a list of parameter names.

    The third parameter is the name of the PADS function.

    For monomorphic types, this function returns the first
    argument. Otherwise, it returns a function parameterized by
    arguments whose name is determined by the third parameter.    
*)
val package_fun : MLast.expr -> Ast.id list -> string -> MLast.expr 

(** Predicate which checks for HostTp -- directly, or nested w/i tuples. *)
val contains_host_tp : Ast.tp -> bool

(** Predicate which checks for HostTp in simple types -- 
    directly, or nested w/i tuples. *)
val contains_s_host_tp : SimpleAst.simple_type -> bool

(** convert tuple element to record field, given function for
    generating record field name from element position.
    Currently filters our HostTp-typed elements.
*)
val convert_t2r : (int -> string) -> Ast.comp_tp list -> Ast.field list

(* needs commments about metadata. *)
(** @deprecated *)
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

val cp_upgrade_1to2: core_phase -> core_phase_v2 

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
    
