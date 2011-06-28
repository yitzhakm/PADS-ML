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
type 'a precord = 'a
type 'a_pdb precord_pd_body = 'a_pdb

module Precord =
struct
  type 'a_rep rep = 'a_rep
  type 'a_pdb pd_body = 'a_pdb
  type 'a_pdb pd = 'a_pdb pd_body Pads.pd

  let gen_pd alpha_gen_pd rep = alpha_gen_pd rep

  let parse alpha_parse pads =
    let (r,(hdr,body)) = alpha_parse pads in
    let new_hdr = Pads.find_eor hdr pads in
      (r,(new_hdr,body))

  let print alpha_print rep pd pads = 
    Pads.print_open_rec pads;
    alpha_print rep pd pads;
    Pads.print_close_rec pads

  
  let specialize_tool alpha_specialize_tool = alpha_specialize_tool
  let specialize_lazy_tool alpha_specialize_tool = alpha_specialize_tool
  let specialize_producer alpha_specialize_producer = alpha_specialize_producer
  let tyrep alpha_tyrep = alpha_tyrep
end
