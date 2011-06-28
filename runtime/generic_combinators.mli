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
(*******************************************************)
(* The abstract type of runtime type representations.  *)
(*******************************************************)

type ('r,'pdb) type_rep

(*******************************************************)
(*          Type-representation combinators            *)
(*******************************************************)

val int       : (int   ,                 unit) type_rep
val float     : (float ,                 unit) type_rep
val char      : (char  ,                 unit) type_rep
val string    : (string,                 unit) type_rep
val unit      : (unit  ,                 unit) type_rep
val extension : (Generic.BTExtension.t * string, unit) type_rep
  
(** We don't include a field
    name for the right element, because we assume that it is a
    nested tuple. If it is a real element, then its name will
    be carried in the pos argument.  *)
val tuple :
  Generic.GenFunTys.pos -> string (* left field name *) ->
  ('a,'a_pdb) type_rep ->
  ('b,'b_pdb) type_rep ->
  ('a * 'b, ('a_pdb,'b_pdb) Generic.GenFunTys.tuple_pdb) type_rep
    
val sum : 
  Generic.GenFunTys.pos -> string ->
  ('a,'a_pdb) type_rep -> bool ->
  ('b,'b_pdb) type_rep -> bool ->
  (('a,'b) Generic.GenFunTys.sum, ('a_pdb,'b_pdb) Generic.GenFunTys.sum_pdb) type_rep

val datatype :
  ('a,'b) type_rep -> 
  ('a,'r) Generic.GenFunTys.iso -> 
  ('b,'pdb) Generic.GenFunTys.iso -> 
  ('r,'pdb) type_rep

val ty_constraint :
  ('a,'pdb) type_rep -> ('a,'pdb Pads.pd) type_rep
  
val list : 
  ('a, 'pdb) type_rep -> 
  ('a list, 'pdb Pads.pd list) type_rep

val table: 
  ('a, 'a_pdb) type_rep ->
  ('b, 'b_pdb) type_rep ->
  (('a, 'b) Hashtbl.t, 
  ('a, ('a_pdb Pads.pd * 'b_pdb Pads.pd)) Hashtbl.t) type_rep

(************************************)
(* Types and functions for building *)
(* matches on type representations  *)
(************************************)

(* The type constructor for the match result. *)
module type Result = sig type ('r,'pdb,'s) tycon end

module MakeTys(R : Result) :
sig
  type ('r,'pdb,'s) result = ('r,'pdb,'s) R.tycon

  type 's type_case = {
    int       : (int,                    unit, 's) result;
    float     : (float,                  unit, 's) result;
    char      : (char,                   unit, 's) result;
    string    : (string,                 unit, 's) result;
    unit      : (unit,                   unit, 's) result;
    extension : (Generic.BTExtension.t * string, unit, 's) result;

    (** A first-class polymorphic field. We don't include a field
	    name for the right element, because we assume that it is a
	    nested tuple. If it is a real element, then its name will
	    be carried in the pos argument.  *)
    tuple : 'a 'b 'a_pdb 'b_pdb. 
      Generic.GenFunTys.pos -> string (* left field name *) ->
          ('a,'a_pdb) type_rep ->
          ('b,'b_pdb) type_rep ->
              ('a * 'b, ('a_pdb,'b_pdb) Generic.GenFunTys.tuple_pdb, 's) result;

    sum : 'a 'b 'a_pdb 'b_pdb. 
      Generic.GenFunTys.pos -> string ->
      ('a,'a_pdb) type_rep -> bool ->
      ('b,'b_pdb) type_rep -> bool ->
          (('a,'b) Generic.GenFunTys.sum, ('a_pdb,'b_pdb) Generic.GenFunTys.sum_pdb, 's) result;

    datatype : 'a 'r 'b 'pdb.
	  ('a,'b) type_rep -> 
	  ('a,'r) Generic.GenFunTys.iso -> 
	  ('b,'pdb) Generic.GenFunTys.iso -> 
	  ('r,'pdb,'s) result;

    ty_constraint : 'a 'pdb. 
	  ('a,'pdb) type_rep -> ('a,'pdb Pads.pd,'s) result;

    list : 'a 'pdb. 
	  ('a, 'pdb) type_rep -> 
      ('a list, 'pdb Pads.pd list, 's) result;

    table: 'a 'a_pdb 'b 'b_pdb.
	  ('a, 'a_pdb) type_rep ->
	  ('b, 'b_pdb) type_rep ->
	  (('a, 'b) Hashtbl.t, ('a, ('a_pdb Pads.pd * 'b_pdb Pads.pd)) Hashtbl.t, 's) result;
    }

  (** If desired, a polymorphic cast can be defined from gmatch as:
      let cast tr = fun tc -> gmatch tr tc
   *)
  val gmatch : ('a,'b) type_rep -> 's type_case -> ('a,'b,'s) result

  val (@@) : 's type_case -> ('a,'b) type_rep -> ('a,'b,'s) result
end

module Consumer_match : sig type ('a, 'b, 'c) tycon = 'a -> 'b Pads.pd -> 'c end
module Producer_match : sig type ('a, 'b, 'c) tycon = 'c -> 'a * 'b Pads.pd end
