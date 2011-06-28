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
(* !!! make sure to include pa_extend.cmo in camlp4 command line when
   doing syntax extensions. e.g.
ocamlc -pp "camlp4 pa_o.cmo pa_extend.cmo pr_o.cmo q_MLast.cmo" -c foo.ml
*)

open Pcaml
open Ast
module HL = HostLanguage

let descr_table = ref BaseTypeDescriptions.base_type_table
let val_ctxt = ref HostLanguage.empty_ctxt

let hoc_e = HL.host_of_ocaml_e
let hoc_p = HL.host_of_ocaml_p

let make_type_id id_s = PadscId.makeid (String.capitalize id_s)

let tuple_flatten = function
    TupleTp([(Exp _) as e]) -> e
  | tp -> Type tp

let parse_arg_list args =
  	let f (sep,term, key, path) = function
          TableSep e ->  (e, term, key, path)
        | TableTerm e -> (sep, e, key, path)
        | TableKeyPath (k, p) -> (sep, term, k, p)
  	in
	List.fold_left f (HL.EOL, HL.EOL, PadscId.makeid(""), PadscId.makeid("")) args 

EXTEND
GLOBAL: str_item;
  exprlit :
    [ [ i = INT -> <:expr<$int:i$>>
      | s = STRING -> <:expr<$str:s$>>
      | c = CHAR -> <:expr<$chr:c$>>]]
  ;

  hlexpr :
    [[
	(* Need to limit to use of literals as full expressions
	   confuse the parser.  The problem is that the parser for
	   expressions tries to eat any ';' that are intened as part
	   of the description (field separators). For actual
	   expressions, then, need to surround by ':'. *)
	lit_e = exprlit -> hoc_e lit_e
    | "<"; e = expr; ":"; t = ctyp; ">" -> HL.host_of_annotated_e e t
    |"pre"; id_e = LIDENT -> HL.host_of_regexp <:expr<$lid:id_e$>>
    |"pre"; str_e = STRING -> HL.host_of_regexp <:expr<$str:str_e$>>
    |"peol" -> HL.host_eol]]
  ;

  pfield :
    [[tp = ptype -> 
      (match tp with
	     TupleTp([Exp e]) -> AbsorbField (Exp e)
	   | _              -> AbsorbField (Type tp))
     | id = LIDENT; ":"; tp = ptype; "="; e = expr LEVEL "expr1" -> 
	 GenField (PadscId.makeid id, tp, hoc_e e)
     | id = LIDENT; ":"; tp = ptype -> FullField (PadscId.makeid id,tp)
	 (* Must be last to give it priority over LIDENT in the other rules. *)
     | "let"; id = LIDENT; "="; e = expr LEVEL "expr1" -> 
	 LetField (PadscId.makeid id, hoc_e e)
     ]]
  ;


  (* Note on camlp4 parsing: it would seem that SELF serves two
     functions - one, to differentiate between precedence levels, and
     two, to manage left recursion. When managing left recursion, it
     seems that SELF (even as the first token of the rule) can refer
     back the to the first precedence level from any precedence level
     in the entry.  When managing precedence, though, SELF only seems
     to refer to higher levels of precedence. I would guess that there
     is some sort of flag (or stack of flags) in the camlp4 parser
     that tracks what mode the parser is in.
  *)

  field_list : 
    [[ f = pfield; ";"; fs = SELF -> f::fs
    |  f = pfield; ";" -> [f]
    |  f = pfield -> [f]
     ]]
  ;

  ptable_arg:
    [[ "sep"; "="; e = expr LEVEL "expr1" -> TableSep (hoc_e e)
     | "term"; "="; e = expr LEVEL "expr1" -> TableTerm (hoc_e e)
     | "key"; "="; path = LIDENT; ":"; id = LIDENT -> 
	TableKeyPath(make_type_id id, PadscId.makeid path) ]]
  ;
(*
  ptype_path:
    [[ id = LIDENT; "."; ids = LIST1 LIDENT SEP "." -> (make_type_id id)::(List.map make_type_id ids) ]]
  ;
*)

  ptable_arg_list:
    [[ f = ptable_arg; ";"; fs = SELF -> f::fs
     | f = ptable_arg; ";" -> [f]
     | f = ptable_arg -> [f]
    ]]
  ;

  ptype : 
    [ "star" 
	[ tp = SELF;  "*"; tps = LIST1 (ptype LEVEL "ptype1") SEP "*"-> TupleTp (List.map tuple_flatten (tp::tps))]
    | "ptype1" 
	[ arg_tp = SELF; id = LIDENT -> TpAppTp ([arg_tp],make_type_id id) ]
    | "ptype2"
	[ 
(* 	  tp = SELF; "Parray"; "["; args = LIST0 parray_args SEP ","; "]" -> ArrayTp (tp,args) | *)
	  "("; tp = SELF; ")" -> tp
	| "{"; fields = field_list; "}" -> RecordTp fields
	| "["; id = LIDENT; ":"; tp = ptype; "|"; e = expr; "]" ->
            WhereTp (PadscId.makeid id, tp, hoc_e e)
	| tp = SELF; arg = ptype_arg -> ValAppTp (tp,arg)
	| "("; arg_tp1 = SELF; ","; arg_tps = LIST1 SELF SEP ","; ")"; id = LIDENT -> TpAppTp (arg_tp1::arg_tps,make_type_id id)
	| "table"; item_tid = LIDENT; "of"; table_args = ptable_arg_list; "end" 
		-> 
		let (sep, term, keytype, path) = parse_arg_list table_args
		in TableTp(make_type_id item_tid, sep, term, keytype, path)
(* 	| e = hlexpr -> SingletonTp e *)
	| e = hlexpr -> TupleTp([Exp e])
        | id = LIDENT -> TidTp (make_type_id id)] 
    ]
  ;

(*   parray_args: *)
(*     [[ "Psep"; e = hlexpr -> ArrSep e *)
(*      | "Pterm"; t = parray_term -> ArrTerm t  *)
(*      | "Ppred"; e = hlexpr -> ArrPred e ]] *)
(*   ; *)

(*   parray_term: *)
(*     [[ "Pnosep" -> ArrNosep *)
(*      | e = hlexpr -> ArrTermExp e]] *)
(*   ;	  *)

  ptype_vparam:
    [[ "("; id = LIDENT; ":"; t = ctyp; ")" -> (PadscId.makeid id, HL.tp_of_ctyp t)]]
  ; 
  
  ptype_tparam:
    [[ -> [] (*empty*) 
     | "("; ids = LIST1 LIDENT SEP ","; ")" -> List.map make_type_id ids] ]
    ;

  ptype_arg:
    [[ "("; e = expr; ")" -> hoc_e e]]
  ; 
  
  pvariant:
    [ [ id = UIDENT; "of"; v_tp = ptype -> 
	  (match v_tp with
	      TupleTp([Exp e]) -> AbsorbVar(PadscId.makeid id, Exp e)
	    | _              -> FullVar(PadscId.makeid id, v_tp))
      | id = UIDENT; "of"; "omit"; v_tp = ptype -> AbsorbVar(PadscId.makeid id, Type v_tp) 
      | id = UIDENT -> AbsorbVar(PadscId.makeid id, Type (TidTp (make_type_id "punit"))) ]]
  ;

  pdefault_var:
    [[ "with"; "pdefault"; id_opt = OPT UIDENT; "of"; d_tp = ptype; "="; e = expr -> 
	 let io = match id_opt with None -> None | Some id -> Some(PadscId.makeid id) in
	   GenDefault(io, d_tp,hoc_e e)
     | "with"; "pdefault"; id_opt = OPT UIDENT; "of"; d_tp = ptype -> 
	 let io = match id_opt with None -> None | Some id -> Some(PadscId.makeid id) in
	   FullDefault (io, d_tp)]]
  ;

  pcase:
    [ [ p = patt; "->"; id = UIDENT; "of"; v_tp = ptype -> 
	  (match v_tp with
	       TupleTp([Exp e]) -> AbsorbCase(hoc_p p, PadscId.makeid id, Exp e)
	     | _              -> FullCase(hoc_p p, PadscId.makeid id, v_tp))
    (* XXX: decide on "absorb" syntax. "of omit" is place holder. *)
      | p = patt; "->"; id = UIDENT; "of"; "omit"; o_tp = ptype -> AbsorbCase(hoc_p p, PadscId.makeid id, Type o_tp)
      | p = patt; "->"; id = UIDENT -> 
	  AbsorbCase(hoc_p p, PadscId.makeid id, Type (TidTp (make_type_id "punit")))
      | p = patt; "->"; id = UIDENT; "of"; c_tp = ptype; "="; e = expr -> GenCase(hoc_p p, PadscId.makeid id,c_tp,hoc_e e)] ]
  ;

  ptype_module_path : 
    [[i = UIDENT; "." -> i]]
  ;

  ptype_equation : 
    [[ "=="; mod_s_opt = OPT ptype_module_path; t_id = LIDENT -> 
      let mod_id_opt = match mod_s_opt with None -> None | Some id -> Some (PadscId.makeid id) in
	(mod_id_opt, PadscId.makeid t_id)]] 
  ;

  ptype_def : 
    [[ tp = ptype -> TpDef tp
     | OPT "|"; vdl = LIST1 pvariant SEP "|"; def_opt = OPT pdefault_var ->
	 DtDef (ImplicitDT (vdl,def_opt)) 
     | "pmatch"; e = expr; "with"; OPT "|"; cl = LIST1 pcase SEP "|" ->
	 DtDef(CaseDT (hoc_e e, cl))]]
  ;

  ptype_binding :
    [[ tparams = ptype_tparam; id = LIDENT; param_opt = OPT ptype_vparam; tp_eq_opt = OPT ptype_equation; "="; def_body = ptype_def 
	-> (tparams, make_type_id id, param_opt, tp_eq_opt, def_body)]]
  ;
  
  pextern :
    [[ tparams = ptype_tparam; id = LIDENT;  param_opt = OPT ptype_vparam -> (tparams, make_type_id id, param_opt)]]
  ;
  
  str_item : 
    [ [ "ptype"; tp_decls = LIST1 ptype_binding SEP "and" ->
	  let _ = Common.global_loc := loc in
	  let (si,dt) = match tp_decls with 
	      [decl] -> Compiler.process_decl false !descr_table !val_ctxt loc decl
	    | tp_decls -> Compiler.process_rec_decls !descr_table !val_ctxt loc tp_decls 
	  in
	  let _ = descr_table := dt in si

    | "pextern"; ext_tp = pextern -> 
	  let _ = Common.global_loc := loc in
	  let dt = Compiler.process_ext !descr_table loc ext_tp in
	    descr_table := dt;
	    <:str_item<declare end>>
      ]] 
  
  ;
END;;
