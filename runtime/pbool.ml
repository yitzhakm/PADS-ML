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
type pbool = bool
type pbool_pd_body = unit

type rep = pbool
type pd_body = pbool_pd_body
type pd = pd_body Pads.pd
let gen_pd _ = Pads.gen_base_pd
    
let parse pads =
  match Pads.Datatype.absorb_string_variant "true" pads with
      Some sp -> true, (Pads.make_valid_pd_hdr sp,())
    | None ->
        match Pads.Datatype.absorb_string_variant "false" pads with
	    Some sp -> false, (Pads.make_valid_pd_hdr sp,())
          | None -> false, Pads.Datatype.handle_error_variant pads ()

let rec print rep pd pads =
  match rep, pd with
      true, _ ->
        Pads.print_str_lit "true" pads
    | false, _ when Pads.pd_is_ok pd ->
        Pads.print_str_lit "false" pads
    | false, _ -> ()

open Generic_tool.Rec_ver
open Type.SPTraversal
let rec specialize_tool gen_tool =
  let rec init () = gen_tool.datatype_t.dt_init ()
  and traverse r ((hdr, pd_body) as pd) state =
    let p_state = gen_tool.datatype_t.dt_start state hdr in
    match r, pd_body with
        true, _ ->
          gen_tool.datatype_t.process_empty_variant p_state "True"
      | false, _ when Pads.pd_is_ok pd ->
          gen_tool.datatype_t.process_empty_variant p_state "False"
      | _ ->
          gen_tool.datatype_t.process_empty_variant p_state "DtErr"
  in
  {init = init; traverse = traverse}
open Generic_lazy_tool.Rec_ver
open Type.SPIOTraversal
let rec specialize_lazy_tool gen_tool =
  let rec init () =
    gen_tool.Generic_lazy_tool.Rec_ver.datatype_t.dt_init ()
  and traverse r ((hdr, pd_body) as pd) state =
    let p_state =
      gen_tool.Generic_lazy_tool.Rec_ver.datatype_t.dt_start state hdr
    in
    match r, pd_body with
        true, _ ->
          gen_tool.Generic_lazy_tool.Rec_ver.datatype_t.
            process_empty_variant
            p_state "True"
      | false, _ when Pads.pd_is_ok pd ->
          gen_tool.Generic_lazy_tool.Rec_ver.datatype_t.
            process_empty_variant
            p_state "False"
      | _ ->
          gen_tool.datatype_t.process_empty_variant p_state "DtErr"
  in
  {init = init; traverse = traverse}
open Type.SPProducer
open Generic_producer
let rec specialize_producer producer =
  {produce =
      fun source ->
        let (rep, hdr) = producer.Rec_ver.process_datatype source in
        match rep with
            "true", None -> true, (hdr, ())
          | "false", None -> false, (hdr, ())
	  | _ ->  raise (Invalid_argument "Producer: source translated into invalid string.")}

open Generic
module UnitGFTys = GenFunTys.Make (UnitClass)
let rec tyrep =
  {UnitGFTys.trep = 
      fun tool ->
        let to_rep =
          function
	      true -> GenFunTys.Left ()
            | false -> GenFunTys.Right ()
        in
        let from_rep =
          function
	      GenFunTys.Left _ -> true
            | GenFunTys.Right _ -> false
        in
        let to_pd () = GenFunTys.Left (Pads.spanless_pd_hdr, ()) in
	let from_pd _ = () in
	let sub_tyrep =
	  {UnitGFTys.trep =
              fun tool ->
		tool.UnitGFTys.sum (GenFunTys.First_last "false") "true" Punit.tyrep true
		  Punit.tyrep true}
	in
	tool.UnitGFTys.datatype sub_tyrep (to_rep, from_rep)
	  (to_pd, from_pd)}



