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
open Pads
module E = Perlre.Escaped_regexp
module S = Perlre.Simple_regexp
module C = Perlre.Char_class_regexp
module RQ = Perlre.Regexp_qualifier
module SU = Perlre.Simple_unqualified_regexp
module Record = Perlre.Record

module Source = Type.Convert_type(Perlre.Source)
module TDebug = Source.Traverse(Debug_tool)

(*  Controls whether to distinguish special children for deciding to start parsing an element.
    If not, then all "redo" members are simply ignored. 
    Given that some containers have *no* members, this should always be set to true.
*)
let split_start_and_members = true

let use_lit_strings = true

exception Failure of string

let handle_res = function
    Pads.Ok p -> p 
  | Pads.Error -> raise (Failure "error result")

(* type regexp = *)
(*     Char_class of C.rep *)
(*     | Simple     of S.rep *)
(*     | Qualified of qualified_regexp *)
(*     | Concat of regexp list *)
(*     | Alt    of regexp list *)

type unqualified_regexp =
    Grouped of perl_regexp
    | Char_class of C.rep
    | Simple     of S.rep

and qualified_regexp = {
  base : unqualified_regexp;
  qual : RQ.rep option
}

and concat_regexp = qualified_regexp list
and perl_regexp = concat_regexp list

(************ INJECTION FUNCTIONS ****************)

let inject_simple_unqualified = function
    Perlre.SChar_class cc_re -> Char_class cc_re
  | Perlre.SSimple      s_re -> Simple s_re

let inject_unqualified inject_nested = function
    Perlre.Grouped ((),nested_re) -> Grouped (inject_nested nested_re)
  | Perlre.Char_class cc_re -> Char_class cc_re
  | Perlre.Simple      s_re -> Simple s_re

let inject_qualified inject_uq q =
  {base = inject_uq q.Perlre.base;
   qual = q.Perlre.qual}

let inject_concat inject_uq concat_re = List.map (inject_qualified inject_uq) concat_re
let inject_alt inject_uq alt_re = List.map (inject_concat inject_uq) alt_re

let inject_nested2 = inject_alt inject_simple_unqualified
let inject_nested1 = inject_alt (inject_unqualified inject_nested2)
let inject_nested0 = inject_alt (inject_unqualified inject_nested1)

(************ CONVERSION FUNCTIONS ****************)

(** escape a string to be a valid reg.exp. *)
let re_escape s = Str.quote s

(** escape a string to be a valid ocaml identifier. For now, just replace '-' and ' ' with '_'.*)
let ocamlid_escape s = Str.global_replace (Str.regexp "[- ]") "_" s

type ptypedef = {tyname: string option; ty :string}

type ptype = 
      Lit_string of string
    | Lit_re of string
    | Type of string 
    | Named_lit_string of string * string
    | Named_lit_re of string * string
    | Named_type of string * string

let fresh_field_count = ref 0
let fresh_variant_count = ref 0
let fresh_type_count = ref 0
let unique_tag_count = ref 0

let arg_count = ref 0

let reset_arg_count () = arg_count := 0
let get_arg_count () = !arg_count
let incr_arg_count () = incr arg_count; !arg_count

let fresh_suf = "__00"

let freshid s fc = 
  let i = !fc in 
    fc := i + 1; 
    s ^ fresh_suf ^ string_of_int i

let fresh_field_name () = freshid "field" fresh_field_count
let fresh_type_name ()  = freshid "type" fresh_type_count
let fresh_variant_name () = freshid "Variant" fresh_variant_count

let keywords = 
  ["type";
   "in";
   "of";
   "with";
   "begin";
   "end";
   "let"]

let bound_names_tbl = Hashtbl.create 400
(* add keywords to hashtable *)
let _ = List.fold_left (fun () s -> Hashtbl.add bound_names_tbl s ()) () keywords

let is_bound n = Hashtbl.mem bound_names_tbl n
let bind_name n = Hashtbl.add bound_names_tbl n ()

let mk_type_name s = "t_" ^ s

(* Given an XML-tag as a string, returns a unique version to avoid overlap of field/constructor names. *)
let mk_unique_tag s =
  let ucs = String.uncapitalize s in
    if is_bound ucs then
      freshid ("f__" ^ s) unique_tag_count
    else
      (bind_name ucs;
       ucs)

(* source: Named_lit, Named_type *)
let mk_field_name s = String.uncapitalize s
let mk_variant_name s = String.capitalize s

let string_of_typedef {tyname=n;ty=t} = 
  let tn = match n with
      None -> fresh_type_name () 
    | Some tn -> tn
  in String.concat " " ["ptype";tn;"=";t]

let type_of_lit_string lit_s = "\"" ^ lit_s ^ "\""
let type_of_lit_re lit_s = "pre \"/" ^ lit_s ^ "/\""
let pstring_of_lit_string lit_s = "pstring_ME(\"/" ^ re_escape lit_s ^"/\")"
let pstring_of_lit_re lit_s = "pstring_ME(\"/" ^ lit_s ^"/\")"

let tuple_element_of_ptype = function
    Lit_string lit_s -> type_of_lit_string lit_s
  | Lit_re lit_s -> type_of_lit_re lit_s
  | Type ty_s   -> ty_s
  | Named_type _ -> raise (Failure "Tuple elements must be unnamed.")
  | Named_lit_string _ -> raise (Failure "Tuple elements must be unnamed.")
  | Named_lit_re _ -> raise (Failure "Tuple elements must be unnamed.")

let field_of_ptype = function
    Lit_string lit_s -> type_of_lit_string lit_s
  | Named_lit_string(name, lit_s) -> mk_field_name name ^ " : " ^ pstring_of_lit_re lit_s
  | Lit_re lit_s -> type_of_lit_re lit_s
  | Named_lit_re(name, lit_s) -> mk_field_name name ^ " : " ^ pstring_of_lit_re lit_s
  | Type ty_s   -> fresh_field_name() ^ " : " ^ ty_s
  | Named_type(name, ty_s)   -> mk_field_name name ^ " : " ^ ty_s

let mk_string_variant name lit_s = 
  mk_variant_name name ^ " of " ^ type_of_lit_string lit_s

let variant_of_ptype = function
    (* Name the variant based on the string literal. *)
    Lit_string lit_s -> mk_string_variant (mk_unique_tag (ocamlid_escape lit_s)) lit_s
  | Named_lit_string(name,lit_s) -> 
      (* Unlike reg.expr., we don't create a pstring, but a literal,
	 as the exactly literal is known statically. *)
      mk_string_variant name lit_s
  | Lit_re lit_s -> fresh_variant_name() ^ " of " ^ type_of_lit_re lit_s
  | Named_lit_re(name,lit_s) -> mk_variant_name name ^ " of " ^ pstring_of_lit_re lit_s
  | Type ty_s   -> fresh_variant_name() ^ " of " ^ ty_s
  | Named_type(name, ty_s) -> mk_variant_name name ^ " of " ^ ty_s

let type_of_ptype = function
    Lit_string lit_s -> type_of_lit_string lit_s
  | Named_lit_string(name,lit_s) -> mk_variant_name name ^ " of " ^ pstring_of_lit_string lit_s
  | Lit_re lit_s -> type_of_lit_re lit_s
  | Named_lit_re(name,lit_s) -> mk_variant_name name ^ " of " ^ pstring_of_lit_re lit_s
  | Type ty_s -> ty_s
  | Named_type(name,ty_s) -> mk_variant_name name ^ " of " ^ ty_s

let omit_type_of_ptype = function
    Lit_string lit_s -> type_of_lit_string lit_s
  | Named_lit_string(name,lit_s) -> mk_variant_name name ^ " of omit " ^ pstring_of_lit_string lit_s
  | Lit_re lit_s -> type_of_lit_re lit_s
  | Named_lit_re(name,lit_s) -> mk_variant_name name ^ " of omit " ^ pstring_of_lit_re lit_s
  | Type ty_s -> fresh_variant_name() ^ " of omit " ^ ty_s
  | Named_type(name,ty_s) -> mk_variant_name name ^ " of omit " ^ ty_s

let mk_typedef ty = {tyname=None; ty=ty}
let mk_named_typedef n ty = {tyname=Some n; ty=ty}
let match_typedef n = function
    {tyname= None} -> false
  | {tyname= Some tn} -> tn = n

let mk_datatype name variants_info =
  let mk_v (name,ty) = mk_variant_name name ^ " of " ^ ty in
  let variants = List.map mk_v variants_info in
  let type_name = fresh_type_name() in
  let type_def = mk_named_typedef type_name (String.concat "|" variants) in
    type_def, type_name

let mk_try_ty bol_anchored t = 
  let suf = if bol_anchored then " ptry" else " cisco_try" in
    match t with
	Type ty_s -> Type ("(" ^ ty_s ^ ")"  ^ suf)
      | Named_type(name,ty_s) -> Named_type(name,"(" ^ ty_s ^ ")"  ^ suf)
      | Lit_re lit_s -> Type (type_of_lit_re lit_s ^ suf)
      | Lit_string lit_s -> Type (type_of_lit_string lit_s ^ suf)
      | _ -> raise (Failure "mk_try_ty: received non-Type ptype.")

let mk_record_ty bol_anchored t = 
  let suf = if bol_anchored then " cisco_bol_record" else " cisco_record" in
    match t with
	Type ty_s -> Type ("(" ^ ty_s ^ ")" ^ suf)
      | Named_type(name,ty_s) -> Named_type(name,"(" ^ ty_s ^ ")"  ^ suf)
      | Lit_re lit_s -> Type (type_of_lit_re lit_s ^ suf)
      | Lit_string lit_s -> Type (type_of_lit_string lit_s ^ suf)
      | _ -> raise (Failure "mk_record_ty: received named ptype.")

(** Force a type to refer to a type id rather than a nested type.
    Returns a the list of typedefs, extended with a new typedef if
    the ty argument was a nested type.  

    This function is somewhat broken. It uses a regular expression to
    determine whether the type is type identifier. Ideally, the ADT
    would have a constructor to indicate this directly, and then we
    would not need this reg.expr check.
*)

let ocamlid_re = Str.regexp "[a-z_][A-Za-z0-9_']*$"
let is_tyid s = Str.string_match ocamlid_re s 0

let tyid_of_ptype ty = None,ty
(*   match ty with *)
(*       Lit_re lit_s ->  *)
(* 	let ty_s = type_of_lit_re lit_s in *)
(* 	let tyname = fresh_type_name () in *)
(* 	let tydef = mk_named_typedef tyname ty_s in *)
(* 	  Some tydef, Type tyname *)
(*     | Lit_string lit_s ->  *)
(* 	let ty_s = type_of_lit_string lit_s in *)
(* 	let tyname = fresh_type_name () in *)
(* 	let tydef = mk_named_typedef tyname ty_s in *)
(* 	  Some tydef, Type tyname *)
(*     | Type ty_s -> *)
(* 	(\* check if ty_s is the name of defined ptype. *\) *)
(* 	if is_tyid ty_s then *)
(* 	  None,ty (\* ty is already a type id. *\) *)
(* 	else *)
(* 	  let tyname = fresh_type_name () in *)
(* 	  let tydef = mk_named_typedef tyname ty_s in *)
(* 	    Some tydef, Type tyname *)
(*     | Named_lit_string (name,lit_s) ->  *)
(* 	let ty_s = type_of_lit_string lit_s in *)
(* 	let tyname = fresh_type_name () in *)
(* 	let tydef = mk_named_typedef tyname ty_s in *)
(* 	  Some tydef, Named_type (name,tyname) *)
(*     | Named_lit_re (name,lit_s) ->  *)
(* 	let ty_s = type_of_lit_re lit_s in *)
(* 	let tyname = fresh_type_name () in *)
(* 	let tydef = mk_named_typedef tyname ty_s in *)
(* 	  Some tydef, Named_type (name,tyname) *)
(*     | Named_type (name,ty_s) ->  *)
(* 	(\* check if ty_s is the name of defined ptype. *\) *)
(* 	if is_tyid ty_s then *)
(* 	  None,ty (\* ty is already a type id. *\) *)
(* 	else *)
(* 	  let tyname = fresh_type_name () in *)
(* 	  let tydef = mk_named_typedef tyname ty_s in *)
(* 	    Some tydef, Named_type (name,tyname) *)

(*********************************************************************)

type patt_info = int * Arg_map.Map.rep

module ArgMap = struct
  open Arg_map

  let get_arg (patt_id,cont_name,arg_map) arg = 
    let match_arg a = a.pattern_id = patt_id && a.pattern_arg = arg
    in
    try Some (List.find match_arg arg_map) with 
	Not_found -> None

  let get_tag (patt_id,cont_name,arg_map) arg = 
    let match_arg a = a.pattern_id = patt_id && a.pattern_arg = arg
    in
    try 
      match List.find match_arg arg_map with 
	  {xml_name=xname; container=""}  -> Some (mk_unique_tag xname)
	| {xml_name=xname; container=cname} when not (cname=cont_name) ->
	    Some ("c__" ^ cname ^ "__" ^ xname)	
	| {xml_name=xname} -> Some (mk_unique_tag xname)
    with 
	Not_found -> None
end

let convert_escaped = function
    Perlre.Word_char -> Lit_re "[[:word:]]"
  | Perlre.Non_word -> Lit_re "[^[:word:]]"
  | Perlre.Digit    -> Lit_re "[[:digit:]]"
  | Perlre.Non_digit   -> Lit_re "[^[:digit:]]"
  | Perlre.Ws -> Lit_re "\\\\s"
  | Perlre.Non_ws -> Lit_re "\\\\S"
  | Perlre.Other c -> 
      if use_lit_strings then
	if c = '.' then Lit_string "." else Lit_re ("\\\\" ^ (String.make 1 c))
      else
	Lit_re ("\\\\" ^ (String.make 1 c))
  | _ -> raise (Failure "unsupported escaped regexp")

let convert_simple = function
    Perlre.Begin_line -> Lit_re "^"
  | Perlre.End_line   -> Lit_re "$"
  | Perlre.Any        -> Type "pchar"
  | Perlre.Bang       -> Lit_re "[!]"
  | Perlre.Literal_char c -> 
      if use_lit_strings then
	Lit_string (String.make 1 c)
      else
	Lit_re (String.make 1 c)
  | Perlre.Escaped e_re -> convert_escaped e_re

let convert_char_class ((),s) = Lit_re ("[" ^ s)

let string_of_qualifier = function
    Perlre.G_kleene -> "*"
  | Perlre.G_opt    -> "?"
  | Perlre.G_plus   -> "+"
  | _ -> raise (Failure "unsupported numeric qualifier")

let convert_regexp_qualifier = function
    Perlre.G_kleene -> "plist_longest"
  | Perlre.G_opt    -> "popt"
  | Perlre.G_plus   -> "nonempty_list"
(*   | Perlre.N_qual nq -> "plist_pred("^ convert_numeric_qualifier nq ^")" *)
  | _ -> raise (Failure "unsupported qualifier")

let rec convert_unqualified patt_info = function
    Grouped re -> 
      let current_arg = incr_arg_count () in
      let tydefs, ty = convert_perlre patt_info re in
	(match ArgMap.get_tag patt_info current_arg with
	    None -> tydefs, ty
	  | Some xname ->
	      match ty with
		  Lit_string lit_s -> tydefs, Named_lit_string (xname, lit_s)
		| Lit_re lit_s -> tydefs, Named_lit_re (xname, lit_s)
		| Type   ty_s -> tydefs, Named_type (xname, ty_s)
		| Named_lit_string (curr_name, lit_s) ->
		    let tydef, new_ty = mk_datatype curr_name [(curr_name,type_of_lit_string lit_s)] in
		      (tydefs@[tydef]), Named_type(xname, new_ty)
		| Named_lit_re (curr_name, lit_s) ->
		    let tydef, new_ty = mk_datatype curr_name [(curr_name,type_of_lit_re lit_s)] in
		      (tydefs@[tydef]), Named_type(xname, new_ty)
		| Named_type (curr_name, ty_s) ->
		    let tydef, new_ty = mk_datatype curr_name [(curr_name,ty_s)] in
		      (tydefs@[tydef]), Named_type(xname, new_ty))
  | Char_class cc_re -> [],convert_char_class cc_re
  | Simple      s_re -> [],convert_simple s_re

and convert_qualified patt_info q_re =
  (* Check for special cases. *)
  match q_re.base, q_re.qual with
      (Simple Perlre.Escaped Perlre.Digit, Some Perlre.G_plus) -> [],Type "pint32"
    | (Simple Perlre.Any, Some Perlre.G_kleene) -> [],Type "pstring_SE(peor)"
    | _ ->
	let tydefs, baseTy = convert_unqualified patt_info q_re.base in
	  match (q_re.qual, baseTy) with
	      (None,_) -> tydefs, baseTy
	    | (Some qual, Lit_re lit_s) -> tydefs, Lit_re ("(" ^ lit_s ^ ")" ^ string_of_qualifier qual)
	    | (Some qual, Named_lit_re(name,lit_s)) -> tydefs, Named_lit_re (name,"(" ^ lit_s ^ ")" ^ string_of_qualifier qual)
	    | (Some qual, Lit_string lit_s) -> tydefs, Lit_re ("(" ^ re_escape lit_s ^ ")" ^ string_of_qualifier qual)
	    | (Some qual, Named_lit_string(name,lit_s)) -> tydefs, Named_lit_re (name,"(" ^ re_escape lit_s ^ ")" ^ string_of_qualifier qual)
	    | (Some qual, ty) -> 
		let new_tydef, ty_id = tyid_of_ptype ty in
		let new_tydefs = match new_tydef with None -> tydefs | Some tydef -> tydefs@[tydef] in
		let new_ty = match ty_id with
		    Type ty_s -> Type (ty_s ^ " " ^ convert_regexp_qualifier qual)
		  | Named_type(name, ty_s) -> Named_type(name, ty_s ^ " " ^ convert_regexp_qualifier qual)
		  | _ -> raise (Failure "Function tyid_of_ptype should only return types, not literals.")
		in
		  new_tydefs, new_ty




and convert_concat patt_info = function
    [] -> raise (Failure "concat re should not be empty")
  | [q_re] -> convert_qualified patt_info q_re
      (* Check for special case of empty comment line. *)
  | [{base=Simple Perlre.Bang; qual=None};
     {base=Simple Perlre.End_line; qual=None}] ->
      [], Type "empty_comment"
  | concat_re ->
      let type_defs,types = List.split (List.map (convert_qualified patt_info) concat_re) in 
      let optimize_re_lits ty (lits,types) = 
	match ty,lits with
	    (Lit_re s,_) -> s::lits,types
	  | (t,[]) -> [],t::types
	  | (t,_)  -> 
	      let merged_lit = Lit_re (String.concat "" lits) in
		[],t::merged_lit::types
      in
      let optimize_string_lits ty (lits,types) = 
	match ty,lits with
	    (Lit_string s,_) -> s::lits,types
	  | (t,[]) -> [],t::types
	  | (t,_)  -> 
	      let merged_lit = Lit_string (String.concat "" lits) in
		[],t::merged_lit::types
      in
      let string_lits,_merged_types = List.fold_right optimize_string_lits types ([],[]) in
      let merged_types = 
	if string_lits = [] then _merged_types 
	else (Lit_string (String.concat "" string_lits))::_merged_types 
      in
      let re_lits,_merged_types = List.fold_right optimize_re_lits merged_types ([],[]) in
      let merged_types = 
	if re_lits = [] then _merged_types 
	else (Lit_re (String.concat "" re_lits))::_merged_types 
      in
	match merged_types with 
	    (* This branch should be unreachable *)
	    [] -> raise (Failure ("Internal error."))
	  | [t] -> (List.flatten type_defs), t
	  | _ ->
	      (* Check whether any of the fields are named. If not, can use tuple. *)
	      if List.exists (function Named_type _ -> true | Named_lit_string _ -> true | Named_lit_re _ -> true | _ -> false) merged_types then
		let fields = List.map field_of_ptype merged_types in
		let type_name = fresh_type_name() in
		let type_def = mk_named_typedef type_name (String.concat "\n   " ["{";(String.concat ";\n   " fields);"}"]) in
		  (List.flatten type_defs)@[type_def],Type type_name
	      else
		let tup_types = List.map tuple_element_of_ptype merged_types in
		let tup_type = String.concat " * " tup_types in
		  (List.flatten type_defs),Type tup_type

and convert_alt patt_info = function
    [] -> raise (Failure "concat re should not be empty")
  | [c_re] -> convert_concat patt_info c_re 
  | alt_re ->
      let type_defs,types = List.split (List.map (convert_concat patt_info) alt_re) in 
      let optimize_lits ty (lits,types) = 
	match ty,lits with
	    (Lit_re s,_) -> s::lits,types
	  | (t,[]) -> [],t::types
	  | (t,[l])  -> [],t::(Lit_re l)::types
	  | (t,lits)  -> 
	      let merged_lit = Lit_re ("((" ^ (String.concat ")|(" lits) ^ "))") in
		[],t::merged_lit::types
      in
      let lits,_merged_types = List.fold_right optimize_lits types ([],[]) in
      let merged_types = match lits with
	  [] -> _merged_types 
	| [l] -> (Lit_re l)::_merged_types 
	| _ -> (Lit_re ("((" ^ (String.concat ")|(" lits) ^ "))"))::_merged_types 
      in
	match merged_types with 
	    [t] -> (List.flatten type_defs), t
	  | _ ->
	      let variants = List.map variant_of_ptype types in
	      let type_name = fresh_type_name() in
	      let type_def = mk_named_typedef type_name (String.concat "|" variants) in
		(List.flatten type_defs) @ [type_def], Type type_name

and convert_perlre patt_info re = convert_alt patt_info re

(* Convert a top-level perl regexp. *)
let convert_tl_perlre patt_info re = 
  reset_arg_count(); convert_alt patt_info re

(* let convert_tl_perlre patt_info re =  *)
(*   reset_arg_count();  *)
(*   let tydefs, ty = convert_alt patt_info re in *)
(*   let new_ty =  *)
(*     match ty with  *)
(* 	Lit_re lit_s -> Lit_re (lit_s ^ "\\n") *)
(*       | Type ty_s -> Type (ty_s ^ " * '\\n'") *)
(*       | Named_lit_re (n,lit_s) -> Named_lit_re (n, lit_s ^ "\\n") *)
(*       | Named_type (n,ty_s) -> Named_type (n, ty_s ^ " * '\\n'") *)

(*   in *)
(*     tydefs, new_ty *)

(********* NODE TABLE *******)
module Node = struct
  type kind = Member | Container of string * bool | Pop of string
  type start = 
      int (** pattern identifier *)
      * ptype * ptypedef list 
      * bool (** redo status. *)
      * bool (** is it anchored at the beginning of line? *)

  type child = 
      int * ptype * ptypedef list * kind
      * bool (** is it anchored at the beginning of line? *)

  type parent = {
      name : string;
      mutable start_type : bool option; (* true = redo (speculative), false = non-redo. *)
      mutable start_children : start list; 
      mutable children : child list; 
    }      

  type table = parent list

  (*** String utility functions. Belong elsewhere. *)
  let is_sub sub s start length =
    let rec check sub_i s_i =
      if sub_i = length then 
	true
      else if not (String.get sub sub_i = String.get s s_i) then
	false
      else
	check (sub_i + 1) (s_i + 1)
    in
      check 0 start

  let is_suffix s suf = 
    let s_len = String.length s in
    let suf_len = String.length suf in
      if suf_len > s_len then false
      else
	is_sub suf s (s_len - suf_len) suf_len

  let is_prefix s pre = 
    let s_len = String.length s in
    let pre_len = String.length pre in
      if pre_len > s_len then false
      else
	is_sub pre s 0 pre_len
	  
  (***********************************)

  let of_record r re = 
    let redo = match r.Perlre.parent_op with
	"redo" -> true
      | "next" -> false
      | "end"  -> false
      | _ -> raise (Failure ("Invalid parent_op in pattern " ^ string_of_int r.Perlre.pattern_id ^ ": " ^ r.Perlre.parent_op))
    in
    let kind = match (r.Perlre.container, r.Perlre.post_pop) with
	(Some c, _) when r.Perlre.parent = c -> raise (Failure ("Parent should not equal container: pattern " ^ string_of_int r.Perlre.pattern_id ^ ", container " ^ c ))
      | (Some c, _) when is_prefix c "popto_" -> Pop (String.sub c 6 (String.length c - 6))
      | (None, _) when r.Perlre.parent_op = "redo" -> raise (Failure ("Non-container pattern should not be marked as redo: pattern " ^ string_of_int r.Perlre.pattern_id))
(*       | (Some c, Some p) when not (p = r.Perlre.parent || p = c) -> raise (Failure ("Post pop not equal to parent or container: " ^ p ^ " != " ^ r.Perlre.parent ^ "," ^ c)) *)
      | (Some c, _) -> Container (c,redo)
      | (None,   Some p) -> Pop p
      | (None,   None) -> Member
    in
    let bol_anchored = match r.Perlre.bol_anchor_opt with Some _ -> true | None -> false in
      r.Perlre.pattern_id, re, kind, bol_anchored

  let create_parent n sc c tc = 
    {name = n;
     start_type = None;
     start_children = sc;
     children = c;}

  let empty = []

    (* Add child to tbl and associate it with specified parent. 
       Creates parent if it does not already exist.
    *)
  let associate tbl arg_map child parent_name =
    let (c_id, c_re, c_kind, c_bol_anchored) = child in
    let parent, tbl = 
      try
	let parent = List.find (fun {name=name} -> name = parent_name) tbl in
	  parent,tbl
      with Not_found -> 
	let new_parent = create_parent parent_name [] [] [] in
	  new_parent, new_parent::tbl
    in
      match c_kind with
	  Container (c,redo) ->
	    (* Need to ensure that child appears earlier than parent in list so that 
	       child's type will be bound before it appears in parent. So, we use 
	       partition to remove the child from the list, if it is already present.
	    *)
	    let c_tydefs,c_ty = convert_tl_perlre (c_id, c, arg_map) c_re in
	    let self_opt,rest = List.partition (fun {name=name} -> name = c) tbl in
	    let self, tbl = 
	      match self_opt with
		  [] -> let self = create_parent c [] [] [] in
			  self, self::tbl			    
		| [self] -> self, self::rest (* Place child at front of list to ensure that it appears before parent. *)
		| _ -> raise (Failure ("Multiple entries for child " ^ c ^ " found."))
	    in
	      begin
		if split_start_and_members then
		  begin
		    (match self.start_type with 
			None -> self.start_type <- Some redo 
		      | Some b -> if not b = redo then
			    prerr_endline ("Warning: container " ^ c ^ " mixes start types."));
		    self.start_children <- (c_id,c_ty,c_tydefs,redo,c_bol_anchored)::self.start_children
		  end
		else
		  if not redo then (* Treat as normal child. *)
		  self.children <- (c_id,c_ty,c_tydefs,Member,c_bol_anchored)::self.children
		    (* Otherwise, simply ignore. *)
	      end;
	      (* Add container c to parent's child list if its not already there.*)
	      if not (List.exists (function (_,_,_,(Container (c',_)),_) -> c' = c | _ -> false) parent.children) then
		parent.children <- (c_id, c_ty, c_tydefs, c_kind, c_bol_anchored)::parent.children
	      else ();
	      tbl
	| _ -> 
	    let c_tydefs, c_ty = convert_tl_perlre (c_id,parent_name,arg_map) c_re in
	      parent.children <- (c_id, c_ty, c_tydefs, c_kind, c_bol_anchored)::parent.children;
	      tbl

  let print_parent p =

    let tydefs_of_member_child (_,_,tydefs,kind,_) = 
      match kind with
	  Container _ -> []
	| _ -> tydefs
    in

    let process_single_start_child (id,ty,tydefs,redo,bol_anchored) = 
      let new_tydef, ty_id = tyid_of_ptype ty in
      let new_tydefs = match new_tydef with None -> tydefs | Some tydef -> tydefs@[tydef] in
      let id_s = string_of_int id in
      let ty_s = 
	if redo then 
	  omit_type_of_ptype (mk_try_ty bol_anchored ty_id) 
	else 
	  type_of_ptype (mk_record_ty bol_anchored ty_id)
      in
	new_tydefs, ty_s ^ " (* start: " ^ id_s ^ "*)"
    in
    let process_start_child (id,ty,tydefs,redo, bol_anchored) = 
      let new_tydef, ty_id = tyid_of_ptype ty in
      let new_tydefs = match new_tydef with None -> tydefs | Some tydef -> tydefs@[tydef] in
      let id_s = string_of_int id in
      let new_ty = if redo then mk_try_ty bol_anchored ty_id else mk_record_ty bol_anchored ty_id in
      let var_s = variant_of_ptype new_ty in
	new_tydefs, var_s ^ " (* start: " ^ id_s ^ "*)"
    in

    let process_single_member_child (id,ty,tydefs,kind, bol_anchored) = 
      let process_non_container ty tydefs =
	let new_tydef, ty_id = tyid_of_ptype ty in
	let new_tydefs = match new_tydef with None -> tydefs | Some tydef -> tydefs@[tydef] in
	let rec_ty = mk_record_ty bol_anchored ty_id in
	  new_tydefs, type_of_ptype rec_ty
      in
      let new_tydefs, ty_s = 
	match kind with
	    Container (c,_) -> [], mk_type_name c
	  | Member -> process_non_container ty tydefs
	  | Pop _  -> process_non_container ty tydefs
      in
      let id_s = string_of_int id in
      let comment_s = match kind with
	  (* If ancestor = p.name, then its not a real pop -- it stays in this container. *)
	  Pop ancestor when not (ancestor = p.name)-> " (* pop to "
	| _ -> " (*"
      in
	new_tydefs, ty_s ^ comment_s ^ id_s ^ "*)"
    in
    let process_member_child (id,ty,tydefs,kind, bol_anchored) = 
      let process_non_container ty tydefs =
	let new_tydef, ty_id = tyid_of_ptype ty in
	let new_tydefs = match new_tydef with None -> tydefs | Some tydef -> tydefs@[tydef] in
	let rec_ty = mk_record_ty bol_anchored ty_id in
	let result = match rec_ty with
	    | Type tid_s -> fresh_variant_name (), tid_s
	    | Named_type(name,tid_s) -> mk_variant_name name, tid_s
	    | _ -> raise (Failure "Internal error: mk_record_ty returned non-Type ptype.")
	in
	  new_tydefs,result
      in
      let var_prefix v_name s = v_name ^ " of " ^ s in
      let term_name, new_tydefs, kind_s = 
	match kind with
	    Container (c,_)  -> [], [], mk_variant_name c ^ " of " ^ mk_type_name c  ^ " (*"
	  | Member ->  
	      let new_tydefs, (name, ty_s) = process_non_container ty tydefs in
		[], new_tydefs, var_prefix name ty_s  ^ " (*"
	  | Pop ancestor -> 
	      let new_tydefs, (name, ty_s) = process_non_container ty tydefs in
	      (* only terminates this parent if it pops *past* the
		 parent, so must ensure that ancestor is different than
		 parent first. *)
	      if ancestor = p.name then 
		[], new_tydefs, var_prefix name ty_s ^ " (*"
	      else 
		[name], new_tydefs, var_prefix name ty_s ^ " (* pop to " ^ ancestor ^ ": "
      in
      let id_s = string_of_int id in
	new_tydefs, (term_name, (kind_s ^ id_s ^ "*)"))
    in
    let type_name = mk_type_name p.name in


    let tydefs,ty_s = match (p.start_children, p.children) with
	(* optimize *)
	(* As there is only one start child, we don't need a separate
	   start type, and as there are no member children we can
	   define the entire container type simply as a type
	   definition.*)
	[(_, _, _,true,_)],[] -> 
	  raise (Failure ("Parent " ^ p.name ^ " has only one child, and it is marked as redo."))
      | [(id, ty, tydefs,false, _) as c],[] -> 
	  process_single_start_child c
      | _ ->	  
	  let start_tydefs, start_field = 
	    match p.start_children with
		[] -> [],[]
	      | children ->
		  let tydefs, start_ty =
		    match children with
			[c] -> process_single_start_child c
		      |_ ->
			 let tydefs_list,variants = List.split (List.rev_map process_start_child children) in
			 let tydefs = List.flatten tydefs_list in
			 let start_ty = String.concat "\n   | "  variants in
			   tydefs, start_ty			      
		  in
		  let start_name = p.name ^ "_start" in
		  let start_ty_name = mk_type_name start_name in
		  let start_field = mk_field_name start_name ^ " : " ^ start_ty_name in
		  let start_tydef = mk_named_typedef start_ty_name start_ty in
		    tydefs@[start_tydef], [start_field]
	  in

	  let members_tydefs, members_field =
	    let choose_term_tycon redo_tycon noredo_tycon =
	      match p.start_type with 
		    None -> prerr_endline ("Warning: Parent " ^ p.name ^ " does not have start type."); noredo_tycon
		  | Some true -> redo_tycon
		  | Some false -> noredo_tycon
	    in
	    let mk_term_tycon_singleton = function
		(* If ancestor = p.name, then its not a real pop -- it stays in this container. *)
		Pop ancestor when not (ancestor = p.name)-> "" (* The one child must be present *)
	      | _ -> choose_term_tycon "nonempty_list" "plist_longest"
	    in
	    let mk_term_tycon tyname = function
		[] -> choose_term_tycon "nonempty_list" "plist_longest"
	      | term_names ->
(* 		  let mod_prefix = String.capitalize tyname  ^ "."  in *)
(* 		  let term_matches = List.map (fun n -> mod_prefix ^ n ^ " _ -> true") term_names in *)
		  let term_matches = List.map (fun n -> n ^ " _ -> true") term_names in
		  let line_sep = "\n       | " in
		  let term_match = String.concat line_sep (term_matches @ ["_ -> false"]) in
		  let term_pred = "\n      fun rev_reps rev_pds -> match rev_reps with [] -> false | x::xs -> match x with\n         " ^ term_match in		    
		    (choose_term_tycon "nonempty_list_pred" "plist_longest_pred") 
		    ^ "(" ^ term_pred ^ ")"
	    in
	      match p.children with
		  [] -> [],[]
		| children ->
		    let members_name = p.name ^ "_members" in
		    let members_ty_name = mk_type_name members_name in
		    let tydefs, members_ty, term_tycon =
		      match children with
			  [(_,_,_,k,_) as c] -> 	
			    let new_tydefs, ty_s = process_single_member_child c in
			    let tycon = mk_term_tycon_singleton k in
			      new_tydefs, ty_s, tycon
			| _ ->
			    let tydefs_list, others = List.split (List.rev_map process_member_child children) in
			    let tydefs = List.flatten tydefs_list in
			    let _term_names, members_strings = List.split others in
			    let term_names = List.flatten _term_names in
			    let term_tycon = mk_term_tycon members_ty_name term_names in
			    let ty_s = String.concat "\n   | "  members_strings in
			      tydefs, ty_s, term_tycon			    
		    in
		    let members_field = mk_field_name members_name ^ " : " ^ (members_ty_name ^ " " ^ term_tycon) in
		    let members_tydef = mk_named_typedef members_ty_name members_ty in
		      tydefs@[members_tydef], [members_field]
	  in

	  let tydefs = start_tydefs @ members_tydefs in
	  let ty =
	    String.concat ";\n   " 
	      (List.flatten [
		  if split_start_and_members then start_field else [];
 		  (* ["pcommit"]; *)
		  members_field;
		])
	  in
	    tydefs,("{\n   " ^ ty ^ "\n}")
    in
    let typedef = mk_named_typedef type_name ty_s in
      List.fold_left (fun () td -> print_endline (string_of_typedef td)) () tydefs;
      print_endline (string_of_typedef typedef);
      print_newline ()

  let print_table tbl = List.fold_left (fun () p -> print_parent p) () tbl
    
end

(******* MAIN ***********)
  
let (_,arg_map),am_pd =
  if Array.length Sys.argv > 1 then
    Tester.parse_source Arg_map.Source.parse Sys.argv.(1) true
  else
    raise (Failure "Missing arg_map file.")

let debug =
  (Array.length Sys.argv > 2 && Sys.argv.(2) = "-D") ||
    (Array.length Sys.argv > 3 && Sys.argv.(3) = "-D")

let records,pd =
  if Array.length Sys.argv > 2 && not (Sys.argv.(2) = "-D") then
    Tester.parse_source Source.parse Sys.argv.(2) true
  else
    Tester.parse_with Source.parse

let _ =
  if debug then
    let _ = print_endline "Debug tool:" in
    let sDebug = TDebug.init () in
      TDebug.traverse records pd sDebug
  else
    ""

let process_re r = inject_nested0 r.Perlre.pattern

(* let process_re r =  *)
(*   convert_tl_perlre  *)
(*     (r.Perlre.pattern_id,arg_map) *)
(*     (inject_nested0 r.Perlre.pattern) *)

let print_preamble () = 
  print_endline "open Built_ins";
(*   print_endline "ptype (a) cisco_line = pre \"/[[:space:]]*/\" * a * pre \"/[[:space:]]*/\""; *)
(*   print_endline "ptype (a) cisco_line = a * pcommit"; *)
  (* absorb trailing data. *)
  print_endline "ptype empty_comment = pre \"/[!]$/\"";
  print_newline();  
  print_endline "ptype (a) cisco_line_prefix = pre \"/ */\" * a";
  print_endline "ptype (a) cisco_line = pre \"/ */\" * a * pre \"/.*/\"";
  print_endline "(* type is anchored to beginning of line. *)";
  print_endline "ptype (a) cisco_bol_line = a * pre \"/.*/\"";
  print_newline();
  print_endline "ptype (a) cisco_try = a cisco_line_prefix ptry";
  print_endline "ptype (a) cisco_bol_record = a cisco_bol_line precord";
  print_endline "ptype (a) cisco_record = a cisco_line precord";
  print_newline ();
  print_endline "let at_least_one = function [] -> false | _ -> true";
  print_endline "ptype (a) nonempty_list = [ l : a plist_longest | at_least_one l ]";
  print_endline "ptype (a) nonempty_list_pred (term_pred: (A.rep, A.pd) Plist_gen_pred.term_pred) = [ l : a plist_longest_pred (term_pred) | at_least_one l ]";
  print_newline ()

let _ =
  let process_record tbl r =
    let re = process_re r in
      Node.associate tbl arg_map (Node.of_record r re) r.Perlre.parent
  in
  let tbl = List.fold_left process_record Node.empty records in
  let unparsed_re = [[{base = Simple Perlre.Any; qual = Some Perlre.G_kleene}]] in
  let tbl_ext = Node.associate tbl arg_map (1000,unparsed_re,Node.Container ("Unparsed",false),true) "Config" in
    print_preamble ();
    Node.print_table tbl_ext


(* parser-pattern.txt: parent is parent-container. container is the
   XML container for the element itself. Elements with no container are
   not included in output.

   parser-arg_map.txt: container is parent-container. xml_name is the
   XML container for the argument.


to do:

   - Need to add nested types to PADS/ML.

   - Changed pattern 107 from redo to next. Check whether had any negative affect. 
     Duplicated and changed 30.
     308 - 310.  Stopped from pushing RtrOption and then immediately popping. instead rely on parser-arg_maps to correctly surround in RtrOption tags.
     Did the same for 292 for Tacacs container.

   - Should pattern 411 be a redo? As it is now, it eats the first ppp without recording its parameters.

   x redo patterns also need whitespace absorbed.

   - Add specia case for [!]$ commments.

   - BUG: pattern 62 (Interface) is losing its name.

   - BUG: pattern 236 is not processed correctly. Particularly: "\d.*\d".
          pattern 218 is not processed correctly. \S+based eats "based".

   x support string and char literals instead of just regexps.

   Q: what happens if unrecognized command found in middle of section?
   It seems that if the remainder of the section does not contain
   "start" elements, then it will all be marked as unparsed. This
   doesn't seem terribly robust. Yet, I don't see any obvious solutions.

   x Change cisco_record type to consume any trailing data.

   - Consider uncommenting "pcommit" between start and members of records.

   - Constrain uses of plist_longest to have at least on
   member. Otherwise, such branches can appear to succeed when the
   start field succeeds but no members match. In fact, they should
   fail and the data should end up in unparsed. (Even better would be
   if they would succeed, with some indication of why the start field
   succeeded but the member field had no elements.)

   - Tell Gary that pattern 138 was also missing "bgp". Also,
   134,135,137,138 all seem to need a "?" before "bgp" (after "(no)?").
   Pattern 333 was missing "ip".
   Patter 448: extra space before cdp
   Patterns 403,277,414,422,423. non optional space before ais.
   Pattern 413,85,363,364,...: missing ? after (no)?
   Patterns 443, ... : \d+.\d+. should be \d+\. Forgot to escape ".".
   - Q: is parser-pattern.txt consistent in use of "(no)? ?foo ..."
   vs. "(no)? foo ...", that is optionalizing the leading space.

   - Change cisco_record to *pcommit* at end of line instead of eating whitespace.
   Also, modify *all* regexps to use leading "^"?

   x Absolutely need special-purpose cisco_record type that eats whitespace at begin
   and end of line.

   ?- ArgMap.get_tag needs the name of the surrounding container to
   operate correctly. Currently, tags everything with a container,
   rather than just those that have container different than
   surrounding container.

   ?- looks like pop_to are placed in the wrong container, or term list.
   Need to correct termination semantics. See Gary's description:
   "If no pattern is matched within the parent container it pops the parent
   and checks the next higher level until it reaches the root container. If
   nothing still matches, it gets pushed onto the unparsed command list."
   From this description, it seems we simultaneously have Plongest and Pterm.
   Plan: join term children with member children, in order of appearance. 
   Then, use Plongest and Ppred, where the predicate will check for any of the term values.

   - it seems that Gary sometimes labels elements of retry lines,
   which is no good seeing as we omit them. We need to somehow
   propogate the name info on the particular line to fields which actually need the name.

   - Handle "end" parent_op to terminate parsing.

   - It seems that it should be an error if start matches but then there are no members.
   However, that depends on whether start consumed anything or not.

   ?- need to omit all elements that come from ptry.
   - In t_config, why is everything out of order?
   - fix redundancy in definition of t_popto_Config.
   x check whether I can eliminate term_children list from nodes. Yes.
   x why are t_popto_Config, t_Set, t_popto_Filters, t_PeerGroup, t_Neighbor not in the right place in the output?
   x escape keywords that show up as field names.
   x MplsOption is recursive. Should it be? no. bug. patt 298 was broken.
   x pattern 297 looks like it should be redo. Yes.
   x type t_IpExtNFilters has empty members type. (patt 192)

   x fix "(start : 15)" not placed in comments.
   - Q: Should Lit_re's converted to types should use Pstring_ME?
   - Check Lit_re's for "real" regular expressionness. if not, encode as chars or strings: easier to read.
   - Q: are containers that pop back to their parents dealt with correctly?
   x fix string_of_start_child for Lit_re case.
   x compute correct termination predicate for member list.
   x change pads/ml pcommit type.
   x add plist_longest, and plist_longest_pred.
   - pass name of current container down to re conversion functions. That way, generated types will have more sensible names.
   x Figure out how to deal with Pstring_ME(s) popt.
   x patterns are spit out in reverse order. Fix.
   x containers listed in parent once per starter. Should be only once.
   x only create records when named fields are involved. Otherwise, use tuples.
   x deal with empty start, members, or term lists.
   - Q: should start alternatives actually be pushed up to parent container? Not good for reuse, but might be fine in current context.
   x optimize lits in datatypes
   x remove type abbreviations by renaming fresh names with real names.
   x deal with redo patterns: ptry(p)
   x merge two systems correctly
   x move containers to start_children of themselves.
   x fix argument handling to not generate type for named arg unless *really* has to.
   x build start, member, and term datatypes correctly.
   - deal with non-existent arguments that *are*  mentioned in parser-arg_map table.
   - deal with container node that has Named_type. Must create new type for Named type.
   - Q: verify that pop_to refers to to name of container in which its found? Is this always the case?
*)


      (* Given type t (a string) and a set of type definitions, checks
	 whether t is simply the name of type in the t.d. set.  If it
	 is, it removes the defintion from the set and returns its
	 body and the set minus said ty. def. Otherwise, it returns the
	 unaltered t.d. set. and None  *)
(*     let optimize_singleton_tydef tydefs = function *)
(* 	Type t -> *)
(* 	  let find_t = match_typedef t in *)
(* 	    (\* check if t is the name of a defined ptype. *\) *)
(* 	  let t_ty_defs, other_ty_defs = List.partition find_t tydefs in *)
(* 	    (match t_ty_defs with *)
(* 		(\* No, t is not a ptype name. *\) *)
(* 		[] -> tydefs, None *)
(* 		  (\* Yes, t is a ptype name. *\) *)
(* 	      | [{ty=ty}] -> other_ty_defs, Some ty *)
(* 	      | _ -> raise (Failure ("Multiple definitions of ptype " ^ t ^ " found.")) *)
(* 	    ) *)
(*       | _ -> tydefs, None *)
(*     in *)

(*     let rename_tydef name tydefs = function *)
(* 	Type t -> *)
(* 	  let find_t = match_typedef t in *)
(* 	    (\* check if t is the name of a defined ptype. *\) *)
(* 	  let t_ty_defs, other_ty_defs = List.partition find_t tydefs in *)
(* 	  let ty_s = match t_ty_defs with *)
(* 		(\* No, t is not a ptype name. *\) *)
(* 		[] -> t *)
(* 		  (\* Yes, t is a ptype name. Return its definition. *\) *)
(* 	    | [{ty=ty}] -> ty *)
(* 	      | _ -> raise (Failure ("Multiple definitions of ptype " ^ t ^ " found.")) *)
(* 	  in *)
(* 	  let tydef = mk_named_typedef name ty_s in *)
(* 	    tydefs@[tydef] *)

(*       | _ -> tydefs *)
(*     in *)
