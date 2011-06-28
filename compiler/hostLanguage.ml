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
(**
 Interface to host language in which embedded expressions are written.
 *)

module N = Names

type expr = Native of MLast.expr 
	    | RegExp of MLast.expr (* Any expression can be used to compute a regexp 
				      as long as its type is string. *)
	    | Annotated of MLast.expr * MLast.ctyp
	    | EOL

type patt = MLast.patt
type tp = MLast.ctyp
type item = MLast.str_item

type literal_tp = Char_lit | String_lit | Int_lit | RegExp_lit | EOL_lit

(** Mapping from identifiers to types. *)
type tp_ctxt = unit

(** Dummy location provided as default of quotations. Quotations that
    want to specify their own location should just locally rebind
    loc. The use of a dummy loc is ok as all of the locs in the ast
    are replaced by camlp4 once the ast is returned at the top
    level.*)
let loc = Ploc.dummy

let ocaml_of_host_e = function
    Native e -> e 
  | RegExp re -> re
  | Annotated (e,t) -> e
  | EOL -> <:expr<raise (Failure "EOL literal expression should not be used.")>> (* dummy expr. Rigged so that if it is used it will raise an exception.*)

let host_of_ocaml_e e = Native e
let host_of_regexp re = RegExp re
let host_of_annotated_e e t = Annotated (e,t)
let host_eol = EOL

let ocaml_of_host_p p = p
let host_of_ocaml_p p = p

let ocaml_of_host_si si = si
let host_of_ocaml_si si = si

let tp_of_ctyp t = t
let ctyp_of_tp c = c

let infer_lit_tp = function
    RegExp _ -> RegExp_lit
  | EOL -> EOL_lit
  | Annotated (_,t) ->
      let loc = MLast.loc_of_ctyp t in
	(match t with
	    <:ctyp<char>> -> Char_lit
	  | <:ctyp<string>> -> String_lit
	  | <:ctyp<int>> -> Int_lit
	  | _ -> PadscError.report_error loc "Type is not within known set of literal types.")
  | Native e ->
      let loc = MLast.loc_of_expr e in
	match e with
	    <:expr<$chr:c$>> -> Char_lit
	  | <:expr<$str:s$>> -> String_lit
	  | <:expr<$int:i$>> -> Int_lit
	  | _ -> PadscError.report_error loc "Expression is not within known set of literals."	 

let infer_type tp_ctxt = function
    RegExp _ -> <:ctyp<string>>
  | EOL -> <:ctyp<unit>>
  | Annotated (_,t) -> t
  | Native e ->
      let loc = MLast.loc_of_expr e in
	match e with
	    <:expr<$chr:c$>> -> <:ctyp<char>>
	  | <:expr<$str:s$>> -> <:ctyp<string>>
	  | <:expr<$int:i$>> -> <:ctyp<int>>
	  | _ -> PadscError.report_error loc "Failed to infer type of expression."

let empty_ctxt = ()
let add_var tp_ctxt id tp = ()
let lookup_var tp_ctxt id = None

