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
include Type.ValParam with type rep = string 
	       and type pd_body = Pads.base_pd_body
	       and type val_param_type = string

(* type rep = string *)
(* type pd_body = Pads.base_pd_body *)
(* type pd = Pads.base_pd *)

(* val parse  : string ->  *)
(*   (rep,pd_body) Pads.parser *)

(* val print  : string ->  *)
(*   (rep,pd_body) Pads.printer *)

(* val gen_pd : string -> rep -> pd *)

(* module Traverse : *)
(* sig *)
(*   val init : ('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t -> unit -> 'state *)
(*   val traverse : ('state,'rps,'dps,'cps,'lps) Generic_tool.Rec_ver.t -> rep -> pd -> 'state -> 'state *)
(* end *)

