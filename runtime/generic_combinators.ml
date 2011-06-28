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
module G = Generic.GenFunTys

(*******************************************************)
(* The abstract type of runtime type representations.  *)
(*******************************************************)

(* this type is a placeholder. actual value doesn't matter. *)
type ('a, 'b, 's) dummy_ty = 'a -> 'b -> 's 

type 's type_case = {
      int       : (int,                    unit, 's) dummy_ty;
      float     : (float,                  unit, 's) dummy_ty;
      char      : (char,                   unit, 's) dummy_ty;
      string    : (string,                 unit, 's) dummy_ty;
      unit      : (unit,                   unit, 's) dummy_ty;
      extension : (Generic.BTExtension.t * string, unit, 's) dummy_ty;

      (** A first-class polymorphic field. We don't include a field
	      name for the right element, because we assume that it is a
	      nested tuple. If it is a real element, then its name will
	      be carried in the pos argument.  *)
      tuple : 'a 'b 'a_pdb 'b_pdb. 
    G.pos -> string (* left field name *) ->
	('a,'a_pdb) type_rep ->
	('b,'b_pdb) type_rep ->
	    ('a * 'b, ('a_pdb,'b_pdb) G.tuple_pdb, 's) dummy_ty;

      sum : 'a 'b 'a_pdb 'b_pdb. 
	G.pos -> string ->
        ('a,'a_pdb) type_rep -> bool ->
        ('b,'b_pdb) type_rep -> bool ->
	(('a,'b) G.sum, ('a_pdb,'b_pdb) G.sum_pdb, 's) dummy_ty;

      datatype : 'a 'r 'b 'pdb.
	('a,'b) type_rep -> 
	('a,'r) G.iso -> 
        ('b,'pdb) G.iso -> 
	('r,'pdb,'s) dummy_ty;

      ty_constraint : 'a 'pdb. 
	    ('a,'pdb) type_rep -> ('a,'pdb Pads.pd,'s) dummy_ty;

      list : 'a 'pdb. 
	    ('a, 'pdb) type_rep -> 
	('a list, 'pdb Pads.pd list, 's) dummy_ty;

      table: 'a 'a_pdb 'b 'b_pdb.
	    ('a, 'a_pdb) type_rep ->
	    ('b, 'b_pdb) type_rep ->
	    (('a, 'b) Hashtbl.t, ('a, ('a_pdb Pads.pd * 'b_pdb Pads.pd)) Hashtbl.t, 's) dummy_ty
  }
and ('r,'pdb) type_rep = {trep:'s.'s type_case -> ('r,'pdb,'s) dummy_ty}


(*******************************************************)
(*                 Type Combinators                    *)
(*******************************************************)

let int = {trep=fun r -> r.int}
let float = {trep=fun r -> r.float}
let char = {trep=fun r -> r.char}
let string = {trep=fun r -> r.string}
let unit = {trep=fun r -> r.unit}
let extension = {trep=fun r -> r.extension}
let tuple pos n t1 t2 = {trep=fun r -> r.tuple pos n t1 t2}
let sum pos n t1 b1 t2 b2 = {trep=fun r -> r.sum pos n t1 b1 t2 b2}
let datatype t iso_r iso_pd = {trep=fun r -> r.datatype t iso_r iso_pd}
let ty_constraint t = {trep=fun r -> r.ty_constraint t}
let list t = {trep=fun r -> r.list t}
let table t1 t2 = {trep=fun r -> r.table t1 t2}

(************************************)
(* Types and functions for building *)
(* matches on type representations  *)
(************************************)
module type Result = sig type ('r,'pdb,'s) tycon end

module MakeTys(R : Result) = 
struct
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
      G.pos -> string (* left field name *) ->
          ('a,'a_pdb) type_rep ->
          ('b,'b_pdb) type_rep ->
              ('a * 'b, ('a_pdb,'b_pdb) G.tuple_pdb, 's) result;

    sum : 'a 'b 'a_pdb 'b_pdb. 
      G.pos -> string ->
      ('a,'a_pdb) type_rep -> bool ->
      ('b,'b_pdb) type_rep -> bool ->
          (('a,'b) G.sum, ('a_pdb,'b_pdb) G.sum_pdb, 's) result;

    datatype : 'a 'r 'b 'pdb.
	  ('a,'b) type_rep -> 
	  ('a,'r) G.iso -> 
	  ('b,'pdb) G.iso -> 
	  ('r,'pdb,'s) result;

    ty_constraint : 'a 'pdb. 
	  ('a,'pdb) type_rep -> ('a,'pdb Pads.pd,'s) result;

    list : 'a 'pdb. 
	  ('a, 'pdb) type_rep -> 
      ('a list, 'pdb Pads.pd list, 's) result;

    table: 'a 'a_pdb 'b 'b_pdb.
	  ('a, 'a_pdb) type_rep ->
	  ('b, 'b_pdb) type_rep ->
	  (('a, 'b) Hashtbl.t, ('a, ('a_pdb Pads.pd * 'b_pdb Pads.pd)) Hashtbl.t, 's) result
    }

  let gmatch: ('a,'b) type_rep -> ('s type_case -> ('a,'b,'s) result) = Obj.magic

  let (@@) m tr = gmatch tr m
end

module Consumer_match = struct type ('a, 'b, 'c) tycon = 'a -> 'b Pads.pd -> 'c end
module Producer_match = struct type ('a, 'b, 'c) tycon = 'c -> 'a * 'b Pads.pd end
