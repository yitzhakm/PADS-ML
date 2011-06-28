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

(* Wrap up top-level list of XML elements into single xml element. *)
let wrap elements name = Xml.Element(name,[],elements)

(* An error condition in the execution of a tool
 * state: The state at the moment of the error
 * string: An error msg *)
exception Tool_error of state * string
  
(* Global tool initialization *)
let init () = ()

let ec_to_string = Pads.string_of_error_code

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
      Pads.Ok r -> [Xml.Element ("val", [], [Xml.PCData(base_to_string r)])]
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
  let init () = []
  let process bty _ result pd_hdr = process_base result (fun s -> s) pd_hdr
end

(* Records are XML objects with nested components *)
module Record = struct
  (* Partial state is a list of XML objects representing the
   * fields processed so far. *)
  type partial_state = state

  let init named_states = []

  let start state pd_hdr = process_hdr pd_hdr

  let project state field_name = []

    (* Build up list in **reverse** order *)
  let process_field fields field_name field_state =
     (Xml.Element (field_name, [], field_state))::fields

  let process_last_field fields field_name state =
     List.rev ((Xml.Element (field_name, [], state))::fields)

end

module Datatype = struct
  type partial_state = state

  let init () = []

  let start _ pd_hdr = process_hdr pd_hdr

  let project state variant_name = None

  let process_variant state variant_name variant =
		 state @ [Xml.Element (variant_name, [], variant)]

  let process_empty_variant state variant_name  =
		 state @ [Xml.Element (variant_name, [], [])]
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
  
  type partial_state = state

  let init () = []

  let start _ pd_hdr = process_hdr pd_hdr

  let project_next s = (s,None)

  let process_next elts elt_s = (Xml.Element ("list_elt",[],elt_s))::elts

  let process_last elts elt_s = List.rev ((Xml.Element ("list_elt",[],elt_s))::elts)

  let process_empty p_state = []

end
