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
(* Flattening tool. *)

type labeled_data = 
      Unlabeled of string 
    | Suffix_labeled of string * string
    | Labeled of string * string

type mail_header_data = labeled_data list


type state = mail_header_data
type global_state = state

(* An error condition in the execution of a tool
 * state: The state at the moment of the error
 * string: An error msg *)
exception Tool_error of state * string
  
(* Global tool initialization *)
let init () = ()

let ec_to_string = Pads.string_of_error_code

let is_sub sub s start length =
  let rec check sub_i s_i =
    if sub_i = length then 
      true
    else if not (String.get sub sub_i = String.get s s_i) then
      false
    else
      check (sub_i + 1) (s_i + 1)
  in
    check 0 start

let is_suffix s suf = 
  let s_len = String.length s in
  let suf_len = String.length suf in
    if suf_len > s_len then false
    else
      is_sub suf s (s_len - suf_len) suf_len

let is_prefix s pre = 
  let s_len = String.length s in
  let pre_len = String.length pre in
    if pre_len > s_len then false
    else
      is_sub pre s 0 pre_len

(* Label unlabeled and suffix_labeled data.*)
let label label_name = function
    Unlabeled s -> Labeled (label_name,s)
  | Suffix_labeled (suf, v) -> Labeled (label_name ^ "_" ^ suf, v)
  | Labeled _ as l -> l

(* For unlabled and suffix_label data, add a partial label that will
   serve as the suffix of some other label.*)
let add_suffix suffix_name = function
    Unlabeled s -> Suffix_labeled (suffix_name,s)
  | Suffix_labeled (suf, v) -> Suffix_labeled (suffix_name ^ "_" ^ suf, v)
  | Labeled _ as l-> l

let hdr_to_string (h : Pads.pd_header) = "ERROR"    

let process_hdr pd_hdr = 
  match pd_hdr.Pads.error_code with
      Pads.Good -> [] (* Blank partial state *)
    | _ -> [hdr_to_string pd_hdr]
	
let process_base result base_to_string pd_hdr =
  match result with
      Pads.Ok r -> [Unlabeled (base_to_string r)]	
    | Pads.Error -> [Unlabeled (hdr_to_string pd_hdr)]
	
let init_base = []

module Int = struct
  type t = int
  type state = global_state
  let init _ = init_base
  let process state result pd_hdr = process_base result string_of_int pd_hdr
end

module Float = struct
  type t = float
  type state = global_state
  let init _ = init_base
  let process state result pd_hdr = process_base result string_of_float pd_hdr
end

module Char = struct
  type t = char
  type state = global_state
  let init _ = init_base
  let process state result pd_hdr = process_base result (String.make 1) pd_hdr
end

(* Name it StringImpl here so that it doesn't shadow the pervasive String module.
   Rebind at end of module.
 *)
module StringImpl = struct
  type t = string
  type state = global_state
  let init _ = init_base
  let process state result pd_hdr = process_base result (fun s -> s) pd_hdr
end

module Unit = struct
  type t = unit
  type state = global_state
  let init () = init_base
  let process state result pd_hdr = process_base result (fun () -> "") pd_hdr
end

module Extension = struct
  type t = Generic_common.BTExtension.t * string
  type state = global_state
  let init () = init_base
  let process state result pd_hdr = process_base result (fun (_,s) -> s) pd_hdr
end

module Record = struct
  (* Partial state is a list of XML objects representing the
   * fields processed so far. *)
  type partial_state = (string * state) list

  let init named_states = init_base

  let project state field_name = [] 

  let start state pd_hdr = []

  (* Build up list in **reverse** order *)
  let process_field fields field_name field_data = (field_name,field_data)::fields

  let _process_field fields (field_name, field_data) =
    let labeled_field_data = 
      if is_prefix field_name "elt" then
	(* this record is a tuple, so just add numeric suffix as prefix. *)
	let elt_num = String.sub field_name 3 (String.length field_name - 3) in
	  List.rev_map (add_suffix elt_num) field_data
      else
	List.rev_map (label field_name) field_data 
    in      
      List.rev_append labeled_field_data fields

  let process_last_field fields field_name field_data =
    let final_fields = process_field fields field_name field_data in
      match final_fields with
	  [(n,d)] when is_prefix n "elt" -> d (* only 1 element, so don't bother with the label.*)
	| _ -> 
	    List.fold_left _process_field [] final_fields

end

(* module Record = struct *)
(*   (\* Partial state is a list of XML objects representing the *)
(*    * fields processed so far. *\) *)
(*   type partial_state = state *)

(*   let init named_states = init_base *)

(*   let project state field_name = []  *)

(*   let start state pd_hdr = [] *)

(*   (\* Build up list in **reverse** order *\) *)
(*   let process_field fields field_name field_data =  *)
(*     let labeled_field_data =  *)
(*       if is_prefix field_name "elt" then *)
(* 	(\* this record is a tuple, so just add numeric suffix as prefix. *\) *)
(* 	let elt_num = String.sub field_name 3 (String.length field_name - 3) in *)
(* 	  List.map (add_suffix elt_num) field_data *)
(*       else *)
(* 	List.map (label field_name) field_data  *)
(*     in       *)
(*       List.rev_append labeled_field_data fields *)

(*   let process_last_field fields field_name field_data = *)
(*     let final_fields = process_field fields field_name field_data in *)
(*       List.rev final_fields *)

(* end *)

module Datatype = struct
  type partial_state = unit

  let init () = init_base

  let start _ pd_hdr = ()

  let project state variant_name = Some []

  let process_variant () variant_name variant_data = 
    List.map (label variant_name) variant_data

  module Empty = struct
    let init () = init_base
    let process state = []
  end
end

module Constraint = struct
  type partial_state = unit

  let init _ = init_base

  let start state pd_hdr = ()

  let project state = state

  let process () sub_data = sub_data
end

module List = struct
  
  type partial_state = int * state

  let init () = init_base

  let start _ pd_hdr = 1,[]

  let project_next _ = [], Some []

  let process_next (n, elts) elt_data = 
    let elt_num = string_of_int n in
    let labeled_elt_data = List.map (add_suffix elt_num) elt_data in      
      (n+1, List.rev_append labeled_elt_data elts)

  let process_last p_state elt_data = 
    let (n,final_fields) = process_next p_state elt_data in
      List.rev final_fields

  let process_empty (n,data) = data

end

module String = StringImpl
