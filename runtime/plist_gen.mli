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
(* Generic version of plist. *)

module type Plist_specs = sig
  type sep
  type term

    (** A processed version of the separator. *)
  type proc_sep
    (** A processed version of the terminator. *)
  type proc_term

  val pre_process_sep : sep -> Pads.handle -> proc_sep
  val pre_process_term : term -> Pads.handle -> proc_term

  val post_process_sep : proc_sep -> Pads.handle -> unit
  val post_process_term : proc_term -> Pads.handle -> unit

    (** Attempt to absorb the next separator.  Also doubles as a test
	for termination. None indicates that the list should be
	terminated, whereas Some indicates that list process should
	continue.
    *)
  val absorb_sep : proc_sep -> Pads.pd_header -> Pads.handle -> Pads.pd_header option

  (** The integer argument indicates whether to consume the terminator
     from the stream if it is matched.*)
  val term_match : Pads.padsc_handle -> proc_term -> int -> Padsc.perror_t

  val print_sep : sep -> Pads.handle -> unit
  end

module Make_plist(Specs: Plist_specs) : Type.TypeAndValParam 
  with type 'a_rep rep = 'a_rep list
  and  type 'a_pdb pd_body = 'a_pdb Pads.pd list
  and  type ('a_rep, 'a_pdb) val_param_type = Specs.sep * Specs.term
