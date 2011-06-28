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
module type BaseType = sig

  (**  the producer's source type *)
  type source

  (** the base type's type *)
  type t

  (** read from the source and produce a value in the basetype along with a parse header *)
  val process : source -> t * Pads.pd_header
end

(** The module interface with which generic producers must comply. 

	A generic producer takes a source type and traverses it;
	specialized code generated for PADS descriptions can use this
	traversal to build a PADS representation.

	This is a simple generic producer, and it will only work with
	source types that have a tree structure similar to PADS
	representations.
*)
module type S = sig
  type source

  val init : unit -> unit

  module Int    : BaseType with type t = int    and type source = source
  module Float  : BaseType with type t = float  and type source = source
  module Char   : BaseType with type t = char   and type source = source
  module String : BaseType with type t = string and type source = source
  module Unit   : BaseType with type t = unit   and type source = source


  (* Extensions get special treatment -- like records, the specializer
	 should give the producer the expected type tag; it also passes in
	 the default value *)
  module Extension : sig
	val process : source -> Generic_common.BTExtension.t -> string -> string * Pads.pd_header
  end

  module Record : sig
	val process : source -> string list -> source list * Pads.pd_header
  end

  module Datatype : sig
	val process : source -> (string * source option) * Pads.pd_header
  end

  module List : sig
	val process : source -> source list * Pads.pd_header
  end
  module Table: sig
	val process : source -> (source, source) Hashtbl.t * Pads.pd_header
  end
end

module Rec_ver : sig
  type ('source, 't) base_type = 'source -> 't * Pads.pd_header
  type     'source extension_t = 'source -> Generic_common.BTExtension.t -> string -> string * Pads.pd_header

  type   'source record_t = 'source -> string list -> 'source list * Pads.pd_header
  type 'source datatype_t = 'source -> (string * 'source option) * Pads.pd_header
  type     'source list_t = 'source -> 'source list * Pads.pd_header
  type     'source table_t = 'source -> ('source, 'source) Hashtbl.t * Pads.pd_header

  type 'source t = {
	init : unit -> unit;

	process_int : ('source, int) base_type;
	process_float : ('source, float) base_type;
	process_char : ('source, char) base_type;
	process_string : ('source, string) base_type;
	process_unit : ('source, unit) base_type;
	
	process_extension : 'source extension_t;

	process_record : 'source record_t;
	process_datatype : 'source datatype_t;
    	process_list : 'source list_t;
    	process_table: 'source table_t;
  }

  module From_mod (Producer : S) : 
  sig
	val producer : Producer.source t
  end
end
