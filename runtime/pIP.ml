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
type pIP = Puint8.rep * Puint8.rep * Puint8.rep * Puint8.rep

type pIP_pd_body =
  Puint8.pd_body Pads.pd * Puint8.pd_body Pads.pd * Puint8.pd_body Pads.pd *
    Puint8.pd_body Pads.pd

let to_string (i1,i2,i3,i4) = 
  String.concat "." 
    [string_of_int i1;
     string_of_int i2;
     string_of_int i3;
     string_of_int i4]

let from_string s =
  let nums = Str.split (Str.regexp "/\\./") s in
  let convert s =
	(* try to parse the integer and return a normal pd *)
	try (int_of_string s, Pads.gen_base_pd)

	(* if it fails, return an error pd *)
	with exn -> (0, Pads.base_pd_of_hdr 
	  { Pads.state = Pads.ps_init; Pads.nerr = 1; 
		Pads.error_code = Pads.Syn; 
		Pads.error_info = Pads.Corrupted_data s; 
		Pads.span = None })
  in
  let ([n1; n2; n3; n4], [pd1; pd2; pd3; pd4]) = List.split (List.map convert nums) in
	(n1, n2, n3, n4), (pd1, pd2, pd3, pd4)

(* This code is derived from generated code from 
   "puint8 * '.' * puint8 * '.' * puint8 * '.' * puint8"
*)
type rep = pIP
type pd_body = pIP_pd_body
type pd = pd_body Pads.pd
let rec gen_pd (elt''13_1, elt''13_2, elt''13_3, elt''13_4) =
  let _pd_hdr = Pads.spanless_pd_hdr in
  let (elt''13_1_pd, _pd_hdr) =
    Pads.Record.gen_pd _pd_hdr (Puint8.gen_pd elt''13_1)
  in
  let (elt''13_2_pd, _pd_hdr) =
    Pads.Record.gen_pd _pd_hdr (Puint8.gen_pd elt''13_2)
  in
  let (elt''13_3_pd, _pd_hdr) =
    Pads.Record.gen_pd _pd_hdr (Puint8.gen_pd elt''13_3)
  in
  let (elt''13_4_pd, _pd_hdr) =
    Pads.Record.gen_pd _pd_hdr (Puint8.gen_pd elt''13_4)
  in
    _pd_hdr, (elt''13_1_pd, elt''13_2_pd, elt''13_3_pd, elt''13_4_pd)
let rec parse pads =
  let make_rep (r''14_1, r''14_2, r''14_3, r''14_4) =
    r''14_1, r''14_2, r''14_3, r''14_4
  in
  let make_pd hdr (r''14_1_pd, r''14_2_pd, r''14_3_pd, r''14_4_pd) =
    hdr, (r''14_1_pd, r''14_2_pd, r''14_3_pd, r''14_4_pd)
  in
  let (r''14_1, r''14_1_pd, _pd_hdr) =
    Pads.Record.parse_first Puint8.parse pads
  in
  let _pd_hdr = Pads.Record.absorb_next_char '.' _pd_hdr pads in
  let (r''14_2, r''14_2_pd, _pd_hdr) =
    Pads.Record.parse_next Puint8.parse _pd_hdr pads
  in
  let _pd_hdr = Pads.Record.absorb_next_char '.' _pd_hdr pads in
  let (r''14_3, r''14_3_pd, _pd_hdr) =
    Pads.Record.parse_next Puint8.parse _pd_hdr pads
  in
  let _pd_hdr = Pads.Record.absorb_next_char '.' _pd_hdr pads in
  let (r''14_4, r''14_4_pd, _pd_hdr) =
    Pads.Record.parse_next Puint8.parse _pd_hdr pads
  in
  let reps = r''14_1, r''14_2, r''14_3, r''14_4 in
  let pds = r''14_1_pd, r''14_2_pd, r''14_3_pd, r''14_4_pd in
  let _pd_hdr = Pads.Record.finish_pd_hdr _pd_hdr pads in
    make_rep reps, make_pd _pd_hdr pds
let rec print
    (r''16_1, r''16_2, r''16_3, r''16_4)
    (_, (r''16_1_pd, r''16_2_pd, r''16_3_pd, r''16_4_pd)) pads =
  Puint8.print r''16_1 r''16_1_pd pads;
  Pads.print_char_lit '.' pads;
  Puint8.print r''16_2 r''16_2_pd pads;
  Pads.print_char_lit '.' pads;
  Puint8.print r''16_3 r''16_3_pd pads;
  Pads.print_char_lit '.' pads;
  Puint8.print r''16_4 r''16_4_pd pads

module GC=Generic_common

open Type.SPTraversal
let specialize_tool gen_tool =
  let init = gen_tool.Generic_tool.Rec_ver.extension_t.Generic_tool.Rec_ver.btext_init in
  let traverse r pd state =
    let (h,_) = pd in 
    let res = if Pads.pd_is_ok pd then Pads.Ok(to_string r) else Pads.Error in
      gen_tool.Generic_tool.Rec_ver.extension_t.Generic_tool.Rec_ver.btext_process 
	GC.BTExtension.IP state res h
  in
    {init = init; traverse = traverse}

open Type.SPIOTraversal
let specialize_lazy_tool gen_tool =
  let init = gen_tool.Generic_lazy_tool.Rec_ver.extension_t.Generic_lazy_tool.Rec_ver.bt_init in
  let traverse r pd state =
    let (h,_) = pd in 
    let res = if Pads.pd_is_ok pd then Pads.Ok(GC.BTExtension.IP, to_string r) else Pads.Error in
      gen_tool.Generic_lazy_tool.Rec_ver.extension_t.Generic_lazy_tool.Rec_ver.bt_process state res h
  in
    {init = init; traverse = traverse}

open Type.SPProducer
open Generic_producer
let specialize_producer producer =
  {
	produce = fun source ->
	  let string_rep, hdr = producer.Rec_ver.process_extension source GC.BTExtension.IP "0.0.0.0" in
	  let rep, pds = from_string string_rep in
		rep, (hdr, pds)
		
  }

open Generic
module UnitGFTys = GenFunTys.Make(UnitClass)
let tyrep = {UnitGFTys.trep = fun tool ->
  (* coercions to and from the generic rep *)
  let to_rep rep = (BTExtension.IP, to_string rep) in
  let from_rep (_, s) = let reps, _ = from_string s in reps in

  (* coercions to and from the generic pd *)
  let to_pd _ = () in
  let from_pd () = 
	let hdr = Pads.spanless_pd_hdr in
	let pd = (hdr, ()) in
	  (pd, pd, pd, pd) 
  in

  let ext_tyrep tool = tool.UnitGFTys.extension in
	tool.UnitGFTys.datatype {UnitGFTys.trep=ext_tyrep} (to_rep, from_rep) (to_pd, from_pd)}
