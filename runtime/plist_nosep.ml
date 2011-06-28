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
(* Version of plist with string separator, terminated by lack of separator. *)
type 'a plist_nosep = 'a list
type 'a_pdb plist_nosep_pd_body = 'a_pdb Pads.pd list

module Plist_nosep_orig = 
  Plist_gen.Make_plist(
    struct 
      type sep = string
      type term = string
      type proc_sep = sep
      type proc_term = term

      let pre_process_sep sep pads = sep
      let pre_process_term term pads = term

      let post_process_sep proc_sep pads = ()
      let post_process_term proc_term pads = ()

      let absorb_sep sep hdr pads = 
	match Padsc.pstr_lit_match (Pads.get_padsc_handle pads) sep 1 with
	    Padsc.P_OK -> Some hdr
	  | Padsc.P_ERR -> None (* No sep found, so list terminates. *)

      let term_match pads sep i = Padsc.P_ERR

      let print_sep = Pads.print_str_lit
    end)

module Plist_nosep  = struct
  type 'a_rep rep = 'a_rep Plist_nosep_orig.rep
  type 'a_pdb pd_body = 'a_pdb Plist_nosep_orig.pd_body
  type 'a_pdb pd = 'a_pdb Plist_nosep_orig.pd

  type ('a_rep,'a_pdb) val_param_type = string
      
  let parse a_parse s = Plist_nosep_orig.parse a_parse (s,"")
  let print a_print s = Plist_nosep_orig.print a_print (s,"")
  let gen_pd a_gen_pd s = Plist_nosep_orig.gen_pd a_gen_pd (s,"")    
  let specialize_tool = Plist_nosep_orig.specialize_tool
  let specialize_lazy_tool = Plist_nosep_orig.specialize_lazy_tool
  let specialize_producer = Plist_nosep_orig.specialize_producer

  open Generic
  module UnitGFTys = GenFunTys.Make(UnitClass)
  let tyrep alpha_tyrep = {UnitGFTys.trep = fun tool ->  tool.UnitGFTys.list alpha_tyrep} 
end
