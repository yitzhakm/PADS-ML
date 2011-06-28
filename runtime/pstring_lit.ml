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
type rep = string
type pd_body = Pads.base_pd_body
type pd = Pads.base_pd

type val_param_type = string

let default = "DEFAULT"

let parse lit_s pads =
  let b_pos = Pads.get_current_pos pads in
    match Padsc.pstr_lit_match (Pads.get_padsc_handle pads) lit_s 1 with
	Padsc.P_OK -> 
	  let sp = Pads.close_span b_pos pads in
	    (lit_s, (Pads.make_valid_pd_hdr sp,()))
      | Padsc.P_ERR -> 
	  if Pads.IO.is_speculative pads then
	    raise Pads.Speculation_failure
	  else
	    let sp = Pads.make_incl_span b_pos b_pos in
	    let hdr = 
	      {Pads.state = Pads.ps_init;
	       Pads.nerr= 1; 
	       Pads.error_code=Pads.Syn; 
	       Pads.error_info=Pads.Error_span sp;
	       Pads.span=sp}
	    in
	      (default,(hdr,()))
	    
let gen_pd lit_s r = Pads.gen_base_pd

let print lit_s rep pd pads = 
  Pads.print_str_lit rep pads

open Type.SPTraversal
let specialize_tool gen_tool = 
{
  init = gen_tool.Generic_tool.Rec_ver.string_t.Generic_tool.Rec_ver.bt_init;
  traverse = fun r pd state ->
    let (h,_) = pd in 
    let res = if Pads.pd_is_ok pd then Pads.Ok( r) else Pads.Error in
      gen_tool.Generic_tool.Rec_ver.string_t.Generic_tool.Rec_ver.bt_process state res h
}

open Type.SPIOTraversal
let specialize_lazy_tool gen_tool = 
{
  init = gen_tool.Generic_lazy_tool.Rec_ver.string_t.Generic_lazy_tool.Rec_ver.bt_init;
  traverse = fun r pd state ->
    let (h,_) = pd in 
    let res = if Pads.pd_is_ok pd then Pads.Ok( r) else Pads.Error in
      gen_tool.Generic_lazy_tool.Rec_ver.string_t.Generic_lazy_tool.Rec_ver.bt_process state res h
}


open Generic_producer
open Type.SPProducer
let specialize_producer producer =
  {
	produce = fun source ->
	  let rep, hdr = producer.Rec_ver.process_string source in
	  let pd = Pads.base_pd_of_hdr hdr in
		rep, pd
  }


open Generic
module UnitGFTys = GenFunTys.Make(UnitClass)
let tyrep = {UnitGFTys.trep = fun tool -> tool.UnitGFTys.string}
