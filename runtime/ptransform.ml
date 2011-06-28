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
type ('s,'t) ptransform = 't
type ('s_pdb,'t_pdb) ptransform_pd_body = 't_pdb

type ('r,'r_image,'pdb, 'pdb_image) transformers = {
    rep_to_target : 'r -> 'r_image;
    rep_to_source : 'r_image -> 'r;
    pd_to_target : 'pdb Pads.pd -> 'pdb_image Pads.pd;
    pd_to_source : 'pdb_image Pads.pd -> 'pdb Pads.pd;
  }

type ('r,'r_image) rep_transformers = 
    ('r,'r_image,Pads.base_pd_body,Pads.base_pd_body) transformers

let id_fun x = x

let make_rep_transformers to_target to_source = 
  {rep_to_target = to_target; rep_to_source = to_source;
   pd_to_target = id_fun; pd_to_source = id_fun;} 

let make_transformers r_to_target r_to_source p_to_target p_to_source = 
  {rep_to_target = r_to_target; rep_to_source = r_to_source;
   pd_to_target = p_to_target; pd_to_source = p_to_source;} 

module Ptransform =
struct
  type ('s,'t) rep = ('s,'t) ptransform
  type ('s,'t) pd_body = ('s,'t) ptransform_pd_body
  type ('s,'t) pd = 't Pads.pd

  type ('s_rep,'s_pdb,'t_rep,'t_pdb) val_param_type = 
      ('s_rep,'t_rep,'s_pdb,'t_pdb) transformers

  let gen_pd s_gen_pd t_gen_pd trans rep = t_gen_pd rep

  let parse s_parse t_parse trans pads =
    let (r,pd) = s_parse pads in
      (trans.rep_to_target r, trans.pd_to_target pd)

  let print s_print t_print trans rep pd pads = 
    s_print (trans.rep_to_source rep) (trans.pd_to_source pd) pads
      
  open Type.SPTraversal

  let specialize_tool s_specialize t_specialize gen_tool =
    let t_tool = t_specialize gen_tool in
      {
	init = t_tool.init;
	traverse = t_tool.traverse;
      }

  open Type.SPIOTraversal
  let specialize_lazy_tool s_specialize t_specialize gen_tool =
    let t_tool = t_specialize gen_tool in
      {
	init = t_tool.init;
	traverse = t_tool.traverse;
      }

  let specialize_producer source_specialize target_specialize = target_specialize
  let tyrep source_tyrep target_tyrep = target_tyrep
end
