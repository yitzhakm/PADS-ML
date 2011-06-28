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
type 'a plist_re = 'a list
type 'a_pdb plist_re_pd_body = 'a_pdb Pads.pd list

module Plist_re = 
  Plist_gen.Make_plist(
    struct 
      type sep = string
      type term = string
      type proc_sep = Pads.reg_exp
      type proc_term = Pads.reg_exp

      let pre_process_sep sep pads = 
	let (err,re) = Padsc.pregexp_alloc (Pads.get_padsc_handle pads) in
	let err = Padsc.pregexp_compile_cstr (Pads.get_padsc_handle pads) sep re in
	  re

      let pre_process_term term pads = 
	let (err,re) = Padsc.pregexp_alloc (Pads.get_padsc_handle pads) in
	let err = Padsc.pregexp_compile_cstr (Pads.get_padsc_handle pads) term re in
	  re

      let post_process_sep proc_sep pads = 
	let err = Padsc.pregexp_cleanup (Pads.get_padsc_handle pads) proc_sep in
	let err = Padsc.pregexp_free (Pads.get_padsc_handle pads) proc_sep in
	  ()

      let post_process_term proc_term pads = 
	let err = Padsc.pregexp_cleanup (Pads.get_padsc_handle pads) proc_term in
	let err = Padsc.pregexp_free (Pads.get_padsc_handle pads) proc_term in
	  ()

      let absorb_sep re h pads = Some (Pads.Record.absorb_next_regexp ~lit:re h pads)
      let term_match = Padsc.pre_match

      (* XXX: hack until we have real regexp printing. *)
      let print_sep = Pads.print_re_lit
    end)
