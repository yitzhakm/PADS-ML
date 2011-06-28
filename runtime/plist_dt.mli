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
(* Version of plist that accepts separatos and terminators of any type. *)

type ('a_rep,'a_pdb) term_pred = 
    int (* list length *) -> 
      'a_rep list -> 'a_pdb Pads.pd list -> bool
	
type sep_ty = 
    Char_sep of char
    | String_sep of string
    | Regexp_sep of string
    | No_sep (** No separator for this list *)	

type ('a_rep, 'a_pdb) term_ty = 
    Char_term of char
    | String_term of string
    | Regexp_term of string
    | No_sep_term (** Terminate this list when no separator is found. *)
    | Pred_term of ('a_rep, 'a_pdb) term_pred
    | Length_term of int
    | Longest_term
    | No_term     (** No terminator for this list -- by default, terminates at eof. *)	

(** Internal representation of separators and terminators. *)
type sep_specs = {
    post_process_s : Pads.handle -> unit;

    (** Attempt to absorb the next separator.  Also doubles as a test
	for termination. None indicates that the list should be
	terminated, whereas Some indicates that list process should
	continue.
    *)
    absorb : Pads.pd_header -> Pads.handle -> Pads.pd_header option;
    print : Pads.handle -> unit
  }

type ('a_rep,'a_pdb) term_specs = {
    post_process_t : Pads.handle -> unit;
    term_pred : Pads.padsc_handle -> int -> 'a_rep list -> 'a_pdb Pads.pd list -> bool;
  }

val get_sep_specs : sep_ty -> ('r,'pdb) term_ty -> Pads.handle -> sep_specs
val get_term_specs : ('r,'pdb) term_ty -> Pads.handle -> ('r,'pdb) term_specs

type 'a plist = 'a list
type 'a_pdb plist_pd_body = 'a_pdb Pads.pd list
module Plist : Type.TypeAndValParam 
  with type 'a_rep rep = 'a_rep plist
  and  type 'a_pdb pd_body = 'a_pdb plist_pd_body
  and  type ('a_rep,'a_pdb) val_param_type = sep_ty * ('a_rep, 'a_pdb) term_ty
