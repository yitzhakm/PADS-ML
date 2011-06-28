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
type pcommit = unit
type pcommit_pd_body = unit

type rep = unit
type pd_body = unit
type pd = unit Pads.pd

let gen_pd () = Pads.gen_base_pd

let parse pads = 
  if Pads.IO.is_speculative pads then
    Pads.IO.commit pads
  else ();
  (), Pads.make_empty_pd pads

let print rep pd pads = ()

open Type.SPTraversal
let specialize_tool gen_tool =
  {
    init = gen_tool.Generic_tool.Rec_ver.unit_t.Generic_tool.Rec_ver.bt_init;
    traverse = fun r pd state ->
      let (h,_) = pd in 
      let res = if Pads.pd_is_ok pd then Pads.Ok(r) else Pads.Error in
	gen_tool.Generic_tool.Rec_ver.unit_t.Generic_tool.Rec_ver.bt_process state res h;
  }

open Type.SPIOTraversal
let specialize_lazy_tool gen_tool =
  {
    init = gen_tool.Generic_lazy_tool.Rec_ver.unit_t.Generic_lazy_tool.Rec_ver.bt_init;
    traverse = fun r pd state ->
      let (h,_) = pd in 
      let res = if Pads.pd_is_ok pd then Pads.Ok(r) else Pads.Error in
	gen_tool.Generic_lazy_tool.Rec_ver.unit_t.Generic_lazy_tool.Rec_ver.bt_process state res h;
  }

open Generic_producer
open Type.SPProducer
let specialize_producer producer =
  {
	produce = fun source ->
	  let rep, hdr = producer.Rec_ver.process_unit source in
	  let pd = Pads.base_pd_of_hdr hdr in
		rep, pd
  }

open Generic
module UnitGFTys = GenFunTys.Make(UnitClass)
let tyrep = {UnitGFTys.trep = fun tool -> tool.UnitGFTys.unit}

(* type 'a pcommit = 'a *)
(* type 'a_pdb pcommit_pd_body = 'a_pdb *)
(* module Pcommit (Alpha : Type.S) = *)
(* struct *)
(*   type rep = Alpha.rep *)
(*   type pd_body = Alpha.pd_body *)
(*   type pd = Alpha.pd *)

(*   let gen_pd = Alpha.gen_pd *)

(*   let parse pads = *)
(*     let x = Alpha.parse pads in *)
(*       Pads.IO.commit(pads);x *)
(*       (\* No need to check whether PD is okay, as any error would have *)
(* 	 raised a Speculation_failure exception. *\) *)

(*   let print = Alpha.print *)
    
(*   module Traverse = Alpha.Traverse *)

(* end *)
