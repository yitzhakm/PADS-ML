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
type ('r,'r_image,'pdb, 'pdb_image) transformers = {
    rep_to_target : 'r -> 'r_image;
    rep_to_source : 'r_image -> 'r;
    pd_to_target : 'pdb Pads.pd -> 'pdb_image Pads.pd;
    pd_to_source : 'pdb_image Pads.pd -> 'pdb Pads.pd;
  }

type ('r,'r_image) rep_transformers = 
    ('r,'r_image,Pads.base_pd_body,Pads.base_pd_body) transformers

val make_rep_transformers : ('r -> 'r_image) -> ('r_image -> 'r) 
                            -> ('r,'r_image) rep_transformers

val make_transformers : 
  ('r -> 'r_image) -> ('r_image -> 'r) 
  -> ('pdb Pads.pd -> 'pdb_image Pads.pd) -> ('pdb_image Pads.pd -> 'pdb Pads.pd) 
  -> ('r,'r_image,'pdb, 'pdb_image) transformers

type ('s,'t) ptransform = 't
type ('s_pdb,'t_pdb) ptransform_pd_body = 't_pdb
module Ptransform : Type.TwoTypesAndValParam
  with type ('s,'t) rep = ('s,'t) ptransform
  and  type ('s,'t) pd_body = ('s,'t) ptransform_pd_body
  and  type ('s_rep,'s_pdb,'t_rep,'t_pdb) val_param_type = 
  ('s_rep,'t_rep,'s_pdb,'t_pdb) transformers
