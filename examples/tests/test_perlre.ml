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
open Perlre
module E = Escaped_regexp
module S = Simple_regexp
module C = Char_class_regexp
module RQ = Regexp_qualifier
module SU = Simple_unqualified_regexp

module TDebug = Source.Traverse(Debug_tool)

exception Failure of string

let handle_res = function
    Pads.Ok p -> p 
  | Pads.Error -> raise (Failure "error result")

type ptype = Type of string | Lit_re of string

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
    SU.Char_class cc_re -> Char_class cc_re
  | SU.Simple      s_re -> Simple s_re

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

let fresh_field_count = ref 0
let fresh_variant_count = ref 0
let fresh_type_count = ref 0

let arg_count = ref 0

let reset_arg_count () = arg_count := 0
let get_arg_count () = !arg_count
let incr_arg_count () = incr arg_count; !arg_count

let freshid s fc = 
  let i = !fc in 
    fc := i + 1; 
    s ^ "__" ^ string_of_int i

let mk_type_name s = String.uncapitalize s ^ "_t"

type patt_info = int * Arg_map.Map.rep

module ArgMap = struct
  open Arg_map
  type arg_info = Arg_info.rep = {
      pattern_id : int;
      seq_num    : int;
      pattern_arg: int;
      xml_name   : string;
      container  : string;
      fixed_value: string option;
      post_pop   : string option;
      delimiter  : string option;
      inc_empty  : char;
    }

  let get_arg (patt_id,arg_map) arg = 
    let match_arg a = a.pattern_id = patt_id && a.pattern_arg = arg
    in
    try Some (List.find match_arg arg_map) with 
	Not_found -> None
end


let fresh_field_name () = freshid "field" fresh_field_count
let fresh_type_name ()  = freshid "type" fresh_type_count
let fresh_variant_name () = freshid "Variant" fresh_variant_count

let convert_escaped = function
    E.Word_char -> "[[:word:]]"
  | E.Non_word -> "[^[:word:]]"
  | E.Digit    -> "[[:digit:]]"
  | E.Non_digit   -> "[^[:digit:]]"
  | E.Ws -> "[[:space:]]"
  | E.Non_ws -> "[^[:space:]]"
  | E.Other c -> "\\" ^ (String.make 1 c)
  | _ -> raise (Failure "unsupported escaped regexp")

let convert_simple = function
    S.Begin_line -> Type "pstring_ME(\"/^/\")"
  | S.End_line   -> Type "pstring_ME(peor)"
  | S.Any        -> Type "pchar"
  | S.Literal_char c ->  Lit_re (String.make 1 c)
  | S.Escaped e_re -> Type ("pstring_ME(\"/" ^ (convert_escaped e_re) ^ "/\")")

let convert_char_class ((),s) = "pstring_ME(\"/[" ^ s ^ "/\")"

let string_of_qualifier = function
    RQ.G_kleene -> "*"
  | RQ.G_opt    -> "?"
  | RQ.G_plus   -> "+"
  | _ -> raise (Failure "unsupported numeric qualifier")

let convert_regexp_qualifier = function
    RQ.G_kleene -> "plist_longest"
  | RQ.G_opt    -> "popt"
  | RQ.G_plus   -> "plist_pred(Min_bound 1)"
(*   | RQ.N_qual nq -> "plist_pred("^ convert_numeric_qualifier nq ^")" *)
  | _ -> raise (Failure "unsupported numeric qualifier")

let rec convert_unqualified patt_info = function
    Grouped re -> incr_arg_count ();convert_perlre patt_info re
  | Char_class cc_re -> [],Type (convert_char_class cc_re)
  | Simple      s_re -> [],convert_simple s_re

and convert_qualified patt_info q_re =
  (* Check for special cases. *)
  match q_re.base, q_re.qual with
      (Simple S.Escaped E.Digit, Some RQ.G_plus) -> [],Type "pint32"
    | _ ->
	let type_defs, baseTy = convert_unqualified patt_info q_re.base in
	let t = match (q_re.qual, baseTy) with
	    (None,_) -> baseTy
	  | (Some qual, Type t)   -> Type (t ^ " " ^ convert_regexp_qualifier qual)
	  | (Some qual, Lit_re s) -> Lit_re ("(" ^ s ^ ")" ^ string_of_qualifier qual)
	in
	  type_defs, t

and convert_concat patt_info = function
    [] -> raise (Failure "concat re should not be empty")
  | [q_re] -> convert_qualified patt_info q_re
  | concat_re ->
      let type_defs,types = List.split (List.map (convert_qualified patt_info) concat_re) in 
      let merge_lits ty (lits,types) = 
	match ty,lits with
	    (Lit_re s,_) -> s::lits,types
	  | (t,[]) -> [],t::types
	  | (t,_)  -> 
	      let merged_lit = Lit_re (String.concat "" lits) in
		[],t::merged_lit::types
      in
      let lits,_merged_types = List.fold_right merge_lits types ([],[]) in
      let merged_types = 
	if lits = [] then _merged_types 
	else (Lit_re (String.concat "" lits))::_merged_types 
      in
	match merged_types with 
	    [t] -> (List.flatten type_defs), t
	  | _ ->
	      let field_of_type = function
		  Lit_re s -> "pre \"/"^s^"/\""
		| Type s   -> fresh_field_name() ^ " : " ^ s
	      in
	      let fields = List.map field_of_type merged_types in
	      let type_name = fresh_type_name() in
	      let type_def = String.concat " " ["ptype";type_name;"= {";(String.concat ";\n   " fields);"}"] in
		(List.flatten type_defs)@[type_def],Type type_name

and convert_alt patt_info = function
    [] -> raise (Failure "concat re should not be empty")
  | [c_re] -> 
      let current_arg = get_arg_count () in
	(match ArgMap.get_arg patt_info current_arg with
	    None -> convert_concat patt_info c_re 
	  | Some {ArgMap.xml_name=xname} -> 
	      let type_defs,ty = convert_concat patt_info c_re in
	      let variant_ty = match ty with
		  Lit_re s -> "pre \"/"^s^"/\""
		| Type s   -> s
	      in
	      let variant =  String.capitalize xname ^ " of " ^ variant_ty in
	      let type_name = mk_type_name xname in
	      let type_def = String.concat " " ["ptype";type_name;"=";variant] in
		type_defs @ [type_def], Type type_name)
  | alt_re ->
      let type_defs,types = List.split (List.map (convert_concat patt_info) alt_re) in 
      let variant_of_type t = 
	let s = match t with
	    Lit_re s -> "pre \"/"^s^"/\""
	  | Type s   -> s
	in
	  fresh_variant_name() ^ " of " ^ s
      in
      let variants = List.map variant_of_type types in
      let type_name = fresh_type_name() in
      let type_def = String.concat " " ["ptype";type_name;"=";(String.concat "|" variants)] in
	(List.flatten type_defs) @ [type_def], Type type_name

and convert_perlre patt_info re = convert_alt patt_info re

(* Convert a top-level perl regexp. *)
let convert_tl_perlre patt_info re = reset_arg_count(); convert_alt patt_info re

(********* NODE TABLE *******)
module Node = struct
  type kind = Member | Container of string | Pop of string
  type t = int * kind
  type parent = {
      name : string;
      mutable start_children : t list; 
      mutable children : t list; 
      mutable term_children : t list; 
    }      
  type table = parent list

  let of_record r = 
    let kind = match (r.Record.container,r.Record.post_pop) with
	(Some c, _) -> Container c
      | (None, Some p) -> Pop p
      | (None, None) -> Member
    in
      r.Record.pattern_id,kind

  let create_parent n sc c tc = 
    {name = n;
     start_children = sc;
     children = c;
     term_children = tc;}

  let empty = []

    (* Add child to tbl and associate it with specified parent. 
       Creates parent if it does not already exist.
    *)
  let associate tbl child parent_name =
    let (child_id, child_kind) = child in
    let parent, tbl = 
      try
	let parent = List.find (fun {name=name} -> name = parent_name) tbl in
	  parent,tbl
      with Not_found -> 
	let new_parent = create_parent parent_name [] [] [] in
	  new_parent, new_parent::tbl
    in
    let _ = match child_kind with
	  Container c ->
	    let new_children = child::parent.start_children in
	      parent.start_children <- new_children 
	| Member -> 
	    let new_children = child::parent.children in
	      parent.children <- new_children 
	| Pop ancestor ->
	    let new_children = child::parent.term_children in
	      parent.term_children <- new_children
    in
      tbl

  let print_parent p =
    let _ = print_endline ("ptype " ^ mk_type_name p.name ^ " = {") in
    let print_child (id,kind) = 
      string_of_int id ^ 
	(match kind with
	    Member -> ": member"
	  | Container c -> ": container: " ^ c
	  | Pop p -> ": pop: " ^ p)
    in
    let _ = List.rev_map (fun n -> print_endline (print_child n)) p.start_children in
    let _ = List.rev_map (fun n -> print_endline (print_child n)) p.children in
    let _ = List.rev_map (fun n -> print_endline (print_child n)) p.term_children in
      print_endline "}"

  let print_table tbl = List.fold_left (fun () p -> print_parent p) () tbl
    
end

(******* MAIN ***********)
  
let (_,arg_map),am_pd =
  if Array.length Sys.argv > 1 then 
    ToolDriver.parse_source Arg_map.Source.parse Sys.argv.(1) true
  else
    raise (Failure "Missing arg_map file.")

let records,pd =
  if Array.length Sys.argv > 2 then 
    PadsEasy.parse_source Source.parse Sys.argv.(2) true
  else
    PadsEasy.parse_with Source.parse

(* let _ =  *)
(*   if Array.length Sys.argv > 2 && Sys.argv.(2) = "-D" then *)
(*     let _ = print_endline "Debug tool:" in *)
(*     let sDebug = TDebug.init () in *)
(*       TDebug.traverse records pd sDebug *)
(*   else *)
(*     "" *)

let process_re r = 
  convert_tl_perlre 
    (r.Record.pattern_id,arg_map)
    (inject_nested0 r.Record.pattern)

(* let _ = *)
(*   let type_defs_list, types = List.split (List.map process_re records) in *)
(*   let type_defs = List.flatten type_defs_list in *)
(*   let _ = List.map (fun s -> print_endline s; print_endline "") type_defs in *)
(*   let _ = print_endline "ptype ciscoIOS = {" in *)
(*   let print_ty = function *)
(*       Lit_re s -> "pre \"/"^s^"/\"" *)
(*     | Type t -> fresh_field_name() ^ " : " ^ t *)
(*   in *)
(*   let _ = List.map (fun t -> print_endline (print_ty t ^ ";")) types in *)
(*     print_endline "}" *)

let _ =
  let process_record tbl r = Node.associate tbl (Node.of_record r) r.Record.parent in
  let tbl = List.fold_left process_record Node.empty records in
    Node.print_table tbl

(* parser-pattern.txt: parent is parent-container. container is the
   XML container for the element itself. Elements with no container are
   not included in output.

   parser-arg_map.txt: container is parent-container. xml_name is the
   XML container for the argument.
*)
