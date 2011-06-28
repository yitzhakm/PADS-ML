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
(** 
    Types, classes and values for representing pads 
    descriptions withing the compiler
*)

(*  
     The different kinds of padsML descriptions. 
type kind = Base 
	    | Datatype 
	    | Tuple 
	    | Record 
	    | Where 
	    | Array 
	    | Abstract
*)

type tyclass = Current   (** The type definition currently being compiled. *)
	       | Ty_var  (** A type variable. *)
	       | Defined (** A previously bound type definition. *)
		
type kind = Base 
	    | Fun of metadata list (* names and kinds of type params. *)
(* 		* HostLanguage.tp option (\* type of val. param. *\) *)

(**
   information about a padsml description.
*)
and metadata = {
  name : PadscId.id; (** The (visible) name of the description. *)
  kind : kind;
  tyclass: tyclass; 
  (* more to come ... *)
}
    
(** A table to store assorted information about pads descriptions. *)
(* Should the table be its own class rather than just parts of the
   module? *)
type table
  
val new_table : table
val add_descr : table -> metadata -> table
val add_table : table -> table -> table
val lookup_descr: table -> PadscId.id -> metadata option

  
