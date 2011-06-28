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
(** Generates specification-wide metadata as a pre phase *)

module C = Common
module N = Names

let loc = Ploc.dummy

(* generate the 'source_file' metadata *)
let make_source_file_decl () = 
  <:str_item<value $lid:N.source_file$ = $str:!C.global_source_file$>>

(* metadata to prepend to the generated .ml file *)
let whole_ast_pre_gen produce_sigs loc ast =
  [make_source_file_decl ()]

(* generate the table mapping module names as strings to specialize_producer functions *)
let make_serialize ast =
  let concat_map f ls = List.concat (List.map f ls) in

  (* all type declarations *)
  let decls = concat_map (function | Ast.Ptype (_, decls) -> decls | _ -> []) ast in

  (* select only unparameterized top-level types *)
  let decls = concat_map (function | ([], id, None, _, _) -> [id] | _ -> []) decls in

	(* for a top-level non-function type Foo, we generate the pattern-matching 
	   clause:

	   "Foo" -> Foo.specialize_producer producer
	*)
  let mk_clause id =
	let id_s = PadscId.id2string id in
	let v_p = <:patt<$str:id_s$>> in
	let v_e = <:expr<
	  let $lid:N.producer_rec$ = $uid:id_s$.$lid:N.specialize_producer_fun$ $lid:N.producer_rec$ in
		fun source -> 
		  let (rep, pd) = $lid:N.producer_rec$.$lid:N.producer_fun$ source in
			$uid:id_s$.$lid:N.printer_fun$ rep pd $lid:N.pads_handle$>> 
	in
	  (v_p, None, v_e)
  in
  let cases = List.map mk_clause decls in
	<:str_item<value $lid:N.serialize_fun$ name $lid:N.producer_rec$ $lid:N.pads_handle$ =
	  match name with [$list:cases$]>>

(* metadata to append to the generated .ml file *)
let whole_ast_post_gen produce_sigs loc ast =
  [<:str_item<open Type.SPProducer>>; make_serialize ast]
