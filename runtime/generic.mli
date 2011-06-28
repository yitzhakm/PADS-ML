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
module BTExtension : sig type t = IP | Bool |Other_ty end
module GenFunTys :
  sig
    module type GenFunClass_sig = sig type ('a, 'b, 'c) t end
    type ('a, 'b) iso = ('b -> 'a) * ('a -> 'b)
    type ('a, 'b) sum = Left of 'a | Right of 'b
    type pos = First | Middle | Last of string | First_last of string | Singleton
    type ('a, 'b) tuple_pdb = 'a Pads.pd * 'b Pads.pd
    type ('a, 'b) sum_pdb = ('a Pads.pd, 'b Pads.pd) sum
    module type S =
      sig
	type ('r,'pdb,'s) gf_class

	(** The encoding of generic functions. *)
	type 's genfun = {
	      int       : (int,                    unit, 's) gf_class;
	      float     : (float,                  unit, 's) gf_class;
	      char      : (char,                   unit, 's) gf_class;
	      string    : (string,                 unit, 's) gf_class;
	      unit      : (unit,                   unit, 's) gf_class;
	      extension : (BTExtension.t * string, unit, 's) gf_class;

	      (** A first-class polymorphic field. We don't include a field
		      name for the right element, because we assume that it is a
		      nested tuple. If it is a real element, then its name will
		      be carried in the pos argument.  *)
	      tuple : 'a 'b 'a_pdb 'b_pdb. 
	    pos -> string (* left field name *) ->
		('a,'a_pdb) type_rep ->
		('b,'b_pdb) type_rep ->
		    ('a * 'b, ('a_pdb,'b_pdb) tuple_pdb, 's) gf_class;

	      sum : 'a 'b 'a_pdb 'b_pdb. 
	    pos -> string ->
	    ('a,'a_pdb) type_rep -> bool ->
	    ('b,'b_pdb) type_rep -> bool ->
		(('a,'b) sum, ('a_pdb,'b_pdb) sum_pdb, 's) gf_class;

	      datatype : 'a 'r 'b 'pdb.
		    ('a,'b) type_rep -> 
		    ('a,'r) iso -> 
	    ('b,'pdb) iso -> 
		    ('r,'pdb,'s) gf_class;

	      ty_constraint : 'a 'pdb. 
		    ('a,'pdb) type_rep -> ('a,'pdb Pads.pd,'s) gf_class;

	      list : 'a 'pdb. 
		    ('a, 'pdb) type_rep -> 
		('a list, 'pdb Pads.pd list, 's) gf_class;

	      table: 'a 'a_pdb 'b 'b_pdb.
		    ('a, 'a_pdb) type_rep ->
		    ('b, 'b_pdb) type_rep ->
		    (('a, 'b) Hashtbl.t, 
		    ('a, ('a_pdb Pads.pd * 'b_pdb Pads.pd)) Hashtbl.t, 's) gf_class
	  }
	and ('r,'pdb) type_rep = {trep:'s.'s genfun -> ('r,'pdb,'s) gf_class}
      end
    module Make (GenFunClass : GenFunClass_sig) : S with
          type ('a, 'b, 'c) gf_class = ('a, 'b, 'c) GenFunClass.t
    module Cast (C1 : GenFunClass_sig) (C2 : GenFunClass_sig) :
          sig
            val cast :
              ('a, 'b) Make(C1).type_rep ->
              ('a, 'b) Make(C2).type_rep
          end
    module ConsumerClass : sig type ('a, 'b, 'c) t = 'a -> 'b Pads.pd -> 'c end
    module ProducerClass : sig type ('a, 'b, 'c) t = 'c -> 'a * 'b Pads.pd end
    module ConsumerGFTys : S with type ('a, 'b, 'c) gf_class = ('a, 'b, 'c) ConsumerClass.t
    module ProducerGFTys : S with type ('a, 'b, 'c) gf_class = ('a, 'b, 'c) ProducerClass.t
  end

(* universal, castable generic tool *)
module UnitClass : sig type ('a, 'pdb, 's) t = 'a -> 'pdb Pads.pd -> 's end

(* specialize : generic_function -> type_rep -> specialized generic function *)
val specialize : 'a -> ('a -> 'b) -> 'b
