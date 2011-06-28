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
type 'a plist_longest_pred = 'a list
type 'a_pdb plist_longest_pred_pd_body = 'a_pdb Pads.pd list

module Plist_base = 
  Plist_gen_spec.Make_plist(
    struct 
      type sep = unit
      type term = unit
      type proc_sep = sep
      type proc_term = term

      let pre_process_sep sep pads = sep
      let pre_process_term term pads = term

      let post_process_sep proc_sep pads = ()
      let post_process_term proc_term pads = ()

      let absorb_sep s h pads = Some h
      let term_match pads t i = Padsc.P_ERR

      let print_sep () pads = ()
    end)

module Plist_longest_pred = struct
  type 'a_rep rep = 'a_rep Plist_base.rep
  type 'a_pdb pd_body = 'a_pdb Plist_base.pd_body
  type 'a_pdb pd = 'a_pdb Plist_base.pd

  type ('a_rep, 'a_pdb) val_param_type = ('a_rep, 'a_pdb) Plist_gen_pred.term_pred

  let check_longest_term = function
      [] -> Plist_gen_spec.Proceed
    | pd::pds -> 
	if Pads.pd_is_ok pd then Plist_gen_spec.Proceed
	else Plist_gen_spec.Terminate_discard

  let check_term term_pred rev_reps rev_pds = 
    if term_pred rev_reps rev_pds then
      Plist_gen_spec.Terminate_keep
    else
      check_longest_term rev_pds

  let parse a_parse term_pred = Plist_base.parse a_parse ((),(),check_term term_pred)
  let print a_print term_pred = Plist_base.print a_print ((),(),check_term term_pred)
  let gen_pd a_gen_pd term_pred = Plist_base.gen_pd a_gen_pd ((),(),check_term term_pred)    
  let specialize_tool = Plist_base.specialize_tool
  let specialize_lazy_tool = Plist_base.specialize_lazy_tool
  let specialize_producer = Plist_base.specialize_producer

  open Generic
  module UnitGFTys = GenFunTys.Make(UnitClass)
  let tyrep alpha_tyrep = {UnitGFTys.trep = fun tool ->  tool.UnitGFTys.list alpha_tyrep} 
end
