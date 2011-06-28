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
(* XML formatter tool: Traverses an arbitrary data source
 * and outputs the data formatted with XML tags. Specifically,
 * this tool uses the MotionTwin XML-Light library to
 * generate a Xml.xml object as its final state that
 * contains tagged data items collected throughout the
 * traversal. *)

(* State: Collects data values into an Xml.xml structure *)
type state = Xml.xml list
type global_state = state

(* Create and alias for the standard lib. String module so that we can reference it later after defining module String. *)
module StdString = String

(* Wrap up top-level list of XML elements into single xml element. *)
let wrap elements name = Xml.Element(name,[],elements)

(* An error condition in the execution of a tool
 * state: The state at the moment of the error
 * string: An error msg *)
exception Tool_error of state * string
  
(* Global tool initialization *)
let init () = ()

let ec_to_string = Pads.string_of_error_code

(* Split a string in two. Prefix is up to, but not including
   sep. Suffix is everything after sep. *)
let split sep s = 
  let sep_re = Str.regexp_string sep in
    Str.split sep_re s

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

let hdr_to_xml (h : Pads.pd_header) =
  Xml.Element ("pd", [], 
	      [Xml.Element("nerr",[],[Xml.PCData (string_of_int h.Pads.nerr)]);
	       Xml.Element("error_code",[],[Xml.PCData (ec_to_string h.Pads.error_code)])])
    
let process_hdr pd_hdr = 
  match pd_hdr.Pads.error_code with
      Pads.Good -> [] (* Blank partial state *)
    | _ -> [hdr_to_xml pd_hdr]
	
let process_base result base_to_string pd_hdr =
  match result with
      Pads.Ok r -> [Xml.PCData(base_to_string r)]
    | Pads.Error -> [hdr_to_xml pd_hdr]
	
module Int = struct
  type t = int
  type state = global_state
  let init _ = []
  let process _ result pd_hdr = process_base result string_of_int pd_hdr
end

module Float = struct
  type t = float
  type state = global_state
  let init _ = []
  let process _ result pd_hdr = process_base result string_of_float pd_hdr
end

module Char = struct
  type t = char
  type state = global_state
  let init _ = []
  let process _ result pd_hdr = process_base result (String.make 1) pd_hdr
end

module String = struct
  type t = string
  type state = global_state
  let init _ = []
  let process _ result pd_hdr = process_base result (fun s -> s) pd_hdr
end

module Unit = struct
  type t = unit
  type state = global_state
  let init () = []
  let process _ result pd_hdr = process_base result (fun () -> "") pd_hdr
end

module Extension = struct
  type t = Generic_common.BTExtension.t * string
  type state = global_state
  let init () = []
  let process _ result pd_hdr = process_base result (fun (_,s) -> s) pd_hdr
end

(* Records are XML objects with nested components *)
module Record = struct
  (* Partial state is a list of XML objects representing the
   * fields processed so far and an optional prefix representing
   * an open virtual (not in description) container. *)
  type partial_state = state

  let init named_states = []

  let start state pd_hdr = process_hdr pd_hdr

  let project state field_name = []

    (* Build up list in **reverse** order *)
  let process_field fields field_name field_state =
      if (is_prefix field_name "elt") 
	|| (is_suffix field_name "_anon") 
      then
        List.rev_append field_state fields
      else
        (Xml.Element (StdString.capitalize field_name, [], field_state))::fields
	  
  let process_last_field state field_name field_state =
    let fields = process_field state field_name field_state in
      List.rev fields

end

module Datatype = struct
  type partial_state = state

  let init () = []

  let start _ pd_hdr = process_hdr pd_hdr

  let project state variant_name = None

  let process_variant state variant_name variant_state =
    match variant_name, variant_state with
	"Section", Xml.Element ("Name", [], [Xml.PCData section_name])::fields -> 
	  state @ [Xml.Element (section_name, [], fields)]
      | "Some",_ | "None",_ -> variant_state
      | _ -> state @ [Xml.Element (variant_name, [], variant_state)]

  module Empty = struct
    let init () = []
    let process state = state
  end
end

module Constraint = struct
  type partial_state = state

  let init _ = []

  let start _ pd_hdr = process_hdr pd_hdr

  let project state = []

  let process state sub_state = state @ sub_state
end

(** Functions for lists. *)
module List = struct
  
  type partial_state = state list

  let init () = []

  let start _ pd_hdr = [process_hdr pd_hdr]

  let project_next s = (s,None)

  let process_next elts elt_s = elt_s::elts

  let process_last elts elt_s = List.flatten (List.rev (elt_s::elts))

  let process_empty p_state = []

end
