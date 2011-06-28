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
(* Global metadata names *)
let source_file = "source_file"

(* Standard identifier names *)
let rep  = "rep"
let pd   = "pd"
let pd_body   = "pd_body"
let mask  = "mask"
let mask_body  = "mask_body"
let parser_fun = "parse"
let genpd_fun  = "gen_pd"
let default_fun = "get_default"
let printer_fun = "print"
let specialize_fun = "specialize_tool"
let specialize_lazy_fun = "specialize_lazy_tool"
let specialize_producer_fun = "specialize_producer"
let serialize_fun = "serialize"
let default_val = "default"

(* Traversal related names. *)
let traversal_functor = "Traverse"
let traversal_mod = "Traverse"
let tool_mod = "Tool"
let tool_rec = "gen_tool"
let init_fun = "init"
let traversal_fun = "traverse"
let traversal_lazy_fun = "traverse"


(* Producer related names *)
let producer_mod = "Produce"
let producer_rec = "producer"
let producer_fun = "produce"
let producer_src = "source"


let make_rep = "make_rep"
let make_pd  = "make_pd"

let pads_handle = "pads"
let where_constraint = "c"

let pd_hdr_var = "_pd_hdr"

(* TypeRep related names *)
let tyrep = "tyrep"
let sub_tyrep = "sub_tyrep"
let tyrep_fun = "tyrep"
let tyrep_tool_rec = "tool"
let tyrep_unit_gftys = "UnitGFTys"

(* Tool module names *)
let gt_mod = "Generic_tool"
let gt_lazy_mod = "Generic_lazy_tool"

(* Invariant: id is capitalized. *)
let mk_rep_c_str  id = (PadscId.id2string id)
let mk_pd_c_str   id = (PadscId.id2string id)  ^ "_pd" 

(* Invariant: id is not capitalized. *)
let mk_rep_str  id = (PadscId.id2string id)
let mk_pd_str   id = (PadscId.id2string id)  ^ "_pd" 

let mk_top_level_rep_str  id = String.uncapitalize (PadscId.id2string id)
let mk_top_level_pd_str   id = String.uncapitalize (PadscId.id2string id)  ^ "_pd" 
let mk_top_level_pd_body_str id = String.uncapitalize (PadscId.id2string id)  ^ "_pd_body"

let mk_rep_tyvar_str id = String.uncapitalize (PadscId.id2string id)
let mk_pd_body_tyvar_str id = String.uncapitalize (PadscId.id2string id) ^ "_pdb"

let mk_fun_tyvar_str id f_name = String.uncapitalize (PadscId.id2string id) ^ "_" ^ f_name

(* Invariant: id is capitalized. *)
let mk_top_level_traversal_str id = PadscId.id2string id ^ "_traverse"
let mk_traversal_funvar_str id = PadscId.id2string id ^ "_traverse"

let def_vt    = "DtDefault"
let def_pd_vt = "DtDefault_pd"
let err_vt    = "DtErr"
let err_pd_vt = "DtErr_pd"

(* Library member names *)
let pads_mod = "Pads"
let scan_lit_char = "p_char_lit_scan1"
let scan_lit_string = "p_str_lit_scan1"
let scan_lit_int = "p_int_lit_scan1"
let make_pd_hdr  = "make_pd_hdr"
let make_tuple_pd_hdr = "make_tuple_pd_hdr"
let get_pd_hdr = "get_pd_hdr"
let print_lit_char    = "print_char_lit"
let print_lit_string  = "print_str_lit"
let print_lit_int     = "print_int_lit"
let print_lit_regexp_str  = "print_re_lit"
let print_lit_EOL  = "print_EOL_lit"

let record_init_pd = "create_pd_hdr"
let record_finish_pd = "finish_pd_hdr"

let record_parse_first = "parse_first"
let record_parse_next = "parse_next"

let record_absorb_first = "absorb_first"
let record_absorb_next = "absorb_next"

(** Name of char literal parsing functions *)
let record_absorb_first_litc = "absorb_first_char"
let record_absorb_next_litc = "absorb_next_char"

(** Name of string literal parsing functions *)
let record_absorb_first_lits = "absorb_first_string"
let record_absorb_next_lits = "absorb_next_string"

(** Name of int literal parsing functions *)
let record_absorb_first_liti = "absorb_first_int"
let record_absorb_next_liti = "absorb_next_int"

(** Name of regular expression parsing functions *)
let record_absorb_first_litre = "absorb_first_regexp"
let record_absorb_next_litre = "absorb_next_regexp"

(** Name of regular expression parsing functions for string-encoded reg. exps. *)
let record_absorb_first_litres = "absorb_first_regexp_str"
let record_absorb_next_litres = "absorb_next_regexp_str"

let dt_make_rep      = "make_rep"
let dt_make_absorb_pd = "make_absorb_pd"

let dt_parse_vt = "parse_variant"
let dt_absorb_vt = "absorb_variant"
let dt_absorb_litc = "absorb_char_variant"
let dt_absorb_lits = "absorb_string_variant"
let dt_absorb_liti = "absorb_int_variant"
let dt_absorb_litre = "absorb_regexp_variant"
let dt_absorb_litres = "absorb_regexp_str_variant"
let dt_absorb_litEOL = "absorb_EOL_variant"

let dt_parse_case = "parse_case"
let dt_absorb_case = "absorb_case"
let dt_absorb_litc_case = "absorb_char_case"
let dt_absorb_lits_case = "absorb_string_case"
let dt_absorb_liti_case = "absorb_int_case"
let dt_absorb_litre_case = "absorb_regexp_case"
let dt_absorb_litres_case = "absorb_regexp_str_case"
let dt_absorb_litEOL_case = "absorb_EOL_case"
let dt_gen_case = "gen_case"

let where_parse_underlying = "parse_underlying"

let handle_t = "handle"
let pd_header_t = "pd_header"
let mask_header_t = "mask_header"
let base_pd_t = "base_pd"
let base_mask_t = "base_mask"
let pervasives_mod = "Built_ins"
let getkey_fun = "get_key" 
let getpd_fun = "get_pd"
