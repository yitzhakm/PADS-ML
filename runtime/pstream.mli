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
(* Lazy version of plist. *)

module Iterator : sig
  type 'a element_t =
      | Done
      | More of 'a * 'a t
  and 'a t = unit -> 'a element_t

  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t
  val iter : ('a -> unit) -> 'a t -> unit
  val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
end

type 'a pstream = 'a Iterator.t
type 'a_pdb pstream_pd_body = 'a_pdb Pads.pd Iterator.t    

(** N.B. While Pstream shares terminator variants with Plist, the
    predicate terminator variant is not as flexible in pstream. It
    will only ever be applied to empty lists, or singleton lists,
    containing the most recently parsed rep and its pd. Predicates that depend
    on the entire list will not function correctly.
*)
module Pstream : sig
  include Type.TypeAndValParam 
  with type 'a_rep rep = 'a_rep pstream
  and  type 'a_pdb pd_body = 'a_pdb pstream_pd_body
  and  type ('a_rep,'a_pdb) val_param_type = Plist_dt.sep_ty * ('a_rep,'a_pdb) Plist_dt.term_ty

  val mk_iterator : 
    ('a_rep,'a_pdb) Pads.parser__ 
    -> ('a_rep,'a_pdb) val_param_type 
    -> Pads.handle 
    -> ('a_rep * 'a_pdb Pads.pd * Pads.pd_header) Iterator.t
    
  end
