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
type pint = int
type pint_pd_body = Pads.base_pd_body

type rep = int
type pd_body = Pads.base_pd_body
type pd = Pads.base_pd

let default = 0

let parse pads = 
  let (res,pd,rep) = Pint_c.pint32_read (Pads.get_padsc_handle pads) 
    Padsc.p_CheckAndSet in      
  let c_pos = Pads.get_current_pos pads in
  let new_pd = Pads.base_pd_of_pbase_pd pd c_pos in
    match res with
	Padsc.P_OK -> (Int32.to_int rep,new_pd)
      | Padsc.P_ERR -> 
	  if Pads.IO.is_speculative pads then
	    raise Pads.Speculation_failure
	  else (default,new_pd)

let print rep pd pads = 
  let new_pd = Pads.pbase_pd_of_base_pd pd in
    ignore (Pint_c.pint32_write2io 
	      (Pads.get_padsc_handle pads) (Pads.get_out_stream pads) 
	      new_pd (Int32.of_int rep))
		
let gen_pd r = Pads.gen_base_pd

open Type.SPTraversal
let specialize_tool gen_tool = 
{
  init = gen_tool.Generic_tool.Rec_ver.int_t.Generic_tool.Rec_ver.bt_init;
  traverse = fun r pd state ->
    let (h,_) = pd in 
    let res = if Pads.pd_is_ok pd then Pads.Ok(r) else Pads.Error in
      gen_tool.Generic_tool.Rec_ver.int_t.Generic_tool.Rec_ver.bt_process state res h
}

open Type.SPIOTraversal
let specialize_lazy_tool gen_tool = 
{
  init = gen_tool.Generic_lazy_tool.Rec_ver.int_t.Generic_lazy_tool.Rec_ver.bt_init;
  traverse = fun r pd state ->
    let (h,_) = pd in 
    let res = if Pads.pd_is_ok pd then Pads.Ok(r) else Pads.Error in
      gen_tool.Generic_lazy_tool.Rec_ver.int_t.Generic_lazy_tool.Rec_ver.bt_process state res h
}

open Generic_producer
open Type.SPProducer
let specialize_producer producer =
  {
	produce = fun source ->
	  let rep, hdr = producer.Rec_ver.process_int source in
	  let pd = Pads.base_pd_of_hdr hdr in
		rep, pd
  }

open Generic
module UnitGFTys = GenFunTys.Make(UnitClass)
let tyrep = {UnitGFTys.trep=fun tool -> tool.UnitGFTys.int}
