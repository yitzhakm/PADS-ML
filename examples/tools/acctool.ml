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
(* Accumulator tool: conforms to Generic_tool.S *)

module G = Generic_tool

(* number of errors * total number seen *)
type baseAcc = int * int
      
(* Base accumulator with error descriptions:
 * (baseAcc, list of headers describing errors) *)
type baseAccErrs = baseAcc * (Pads.pd_header list)
      
type acc = baseAccErrs G.metadata

(* Main state representation is the accumulator data structure *)
type state = acc

let initBaseAcc = (0,0)
let initBaseAccErrs = (initBaseAcc, [])
    
(* Converts an accumulator state to XML using Generic_tool's
 * metadata_to_xml utility *)
let state_to_xml (s : state) : Xml.xml =
  G.metadata_to_xml s
    (function 
	((errs, total), cur_header::_) ->
	  let hdr_str = Pads.string_of_error_code cur_header.Pads.error_code
	  in
	  [Xml.PCData ("errors: " ^ (string_of_int errs)
		       ^ "; total: " ^ (string_of_int total)
		       ^ "; cur. header: " ^ hdr_str)]
      |	((errs, total), nil) ->
	  [Xml.PCData ("errors: " ^ (string_of_int errs)
		       ^ "; total: " ^ (string_of_int total))])

(* An error condition in the execution of a tool
 * state: The state at the moment of the error
 * string: An error msg *)
exception Tool_error of state * string
    
(* called first *)
let init () = ()
    
(* Aux. function for processing a base type
 * bacc : baseTypeAcc (state for cur elt)
 * elt : t Pads.result (an optional base type val or error flag)
 * header : Pads.pd_header (the PD header for cur elt) *)
let processBaseType bacc elt (header : Pads.pd_header) =
  let ((errs, total), headers) = bacc in
  (* For base types, accumulate list of headers for which
   * nerr > 0 *)
  if header.Pads.nerr > 0
  then ((errs + 1, total + 1), header::headers)
  else ((errs, total + 1), headers)
      
(* Base types: int, char, string, unit *)
module Int : (G.BaseType with type t = int and type state = state) = struct
  type t = int
  type state = acc
	
  let init () = G.IntData initBaseAccErrs

  let process state elt header =
    match state with
      G.IntData bacc -> G.IntData (processBaseType bacc elt header)
    | _ -> raise (Tool_error (state, "Acctool.Int.process: Expected state IntData _"))
end
    
module Float : (G.BaseType with type t = float and type state = state) = struct
  type t = float
  type state = acc
	
  let init () = G.FloatData initBaseAccErrs

  let process state elt header =
    match state with
      G.FloatData bacc -> G.FloatData (processBaseType bacc elt header)
    | _ -> raise (Tool_error (state, "Acctool.Float.process: Expected state FloatData _"))
end
    
module Char : (G.BaseType with type t = char and type state = state) = struct
  type t = char
  type state = acc
	
  let init () = G.CharData initBaseAccErrs
      
  let process state elt header =
    match state with
      G.CharData bacc -> G.CharData (processBaseType bacc elt header)
    | _ -> raise (Tool_error (state, "Acctool.Char.process: Expected state CharData _"))
end
    
module String : (G.BaseType with type t = string and type state = state) = struct
  type t = string
  type state = acc
	
  let init () = G.StringData initBaseAccErrs
      
  let process state elt header =
    match state with
      G.StringData bacc -> G.StringData (processBaseType bacc elt header)
    | _ -> raise (Tool_error (state, "Acctool.String.process: Expected state StringData _"))
end
    
module Unit : (G.BaseType with type t = unit and type state = state) = struct
  type t = unit
  type state = acc
	
  let init () = G.UnitData initBaseAccErrs
      
  let process state elt header =
    match state with
      G.UnitData bacc -> G.UnitData (processBaseType bacc elt header)
    | _ -> raise (Tool_error (state, "Acctool.Unit.process: Expected state UnitData _"))
end
    
module Record = struct
  (* Record/tuple state: baseAcc, accs for fields
   * (indexed by field names) *)
  type partial_state = baseAccErrs * acc G.StringMap.t
	
	(* Initial state for a record does not need any
	 * field data as that data is built up via
	 * process_fields. *)
  let init accs = G.RecordData (initBaseAccErrs, G.buildStringMap accs)
      
      (* Generate starting state; assumes given an initial state *)
  let start state header =
    match state with
      G.RecordData (((errs, total), headers), accs) ->
	(let (newerrs, newheaders) =
	  match header.Pads.error_code with
	    Pads.Good -> (errs, headers)
	  | Pads.Maybe -> (errs, headers)
	  | _ -> (errs + 1, header::headers)
	in (((newerrs, total + 1), newheaders), accs))
    | _ -> raise (Tool_error (state, "Acctool.Record.start: Expected state RecordData _"))
	  
  let project state label =
    match state with
      G.RecordData (_, accs) ->
	(try G.findStringMap label accs
	with _ -> raise (Tool_error (state, "Acctool.Record.project: Component " 
				     ^ label ^ " not found in record")))
    | _ -> raise (Tool_error (state, "Acctool.Record.project: Expected state RecordData _"))

	  (* Process field data by adding it to the map contained
	   * in the partial state *)
  let process_field (baccErrs, accs) (label : string) acc =
    (baccErrs, G.addStringMap label acc accs)
      
  let finish (baccErrs, accs) =
    G.RecordData (baccErrs, accs)
end
    
module Datatype = struct
  (* Partial state: whole datatype header, whole datatype 
   * error descriptors, and variant descriptors *)
  type partial_state = Pads.pd_header * baseAccErrs * (acc G.StringMap.t)
	
(* XXX init changed to accomodate recursive datatypes; now returns an empty map
   let init branches = 
   G.DatatypeData (String.init (), buildStringMap branches)*)
  let init () = 
    G.DatatypeData (initBaseAccErrs, G.StringMap.empty)

  let start state header =
    match state with
      G.DatatypeData (baccErrs, accs) -> (header, baccErrs, accs)
    | _ -> raise (Tool_error (state, "Acctool.Datatype.start: Expected state DatatypeData _"))
	  
  let project state label =
    match state with
      G.DatatypeData (_, accs) ->
	(try Some (G.findStringMap label accs)
	with _ -> None)
    | _ -> raise (Tool_error (state, "Acctool.Datatype.project: Expected state DatatypeData_"))
	  
  let process_variant (header, ((errs, total), headers), accs) label acc =
    let newbaccErrs =
      match header.Pads.error_code with
	Pads.Good -> ((errs, total + 1), headers)
      | Pads.Maybe -> ((errs, total + 1), headers)
      | _ -> ((errs + 1, total + 1), header::headers)
    in
    G.DatatypeData (newbaccErrs, G.addStringMap label acc accs)
      
  module Empty = struct
    let init () = Unit.init()
	
    let process state =
      match state with
	G.UnitData ((errs, total), headers) ->
	  G.UnitData ((errs, total + 1), headers)
      | _ -> raise (Tool_error (state, "Acctool.Datatype.Empty.process: Expected state UnitData _"))
  end
end
    
module Constraint = struct
  (* Partial state
   * Error descriptions for whole constrained value (baseAccErrs) *)
  type partial_state = baseAccErrs
	
	(* Builds initial constrained state
	 * undAcc: Initial underlying state *)
  let init undAcc =
    G.ConstraintData ((initBaseAcc, []), undAcc)
      
      (* XXX Starts processing a constraint
       * Arg 1: Prior constrained state (state)
       * header: PD header for whole constraint (pd_header)
       * Returns: Partial state for whole constraint *)
  let start state header =
    match state with
      G.ConstraintData (((errs, total), headers), _) ->
	(match header.Pads.error_code with
	  Pads.Good -> ((errs, total + 1), headers)
	| Pads.Maybe -> ((errs, total + 1), headers)
	| _ -> ((errs + 1, total + 1), header::headers))
    | _ -> raise (Tool_error (state, "Acctool.Constraint.start: Expected state ConstraintData _"))
	  
  let project state =
    match state with
      G.ConstraintData (_, undAcc) -> undAcc
    | _ -> raise (Tool_error (state, "Acctool.Constraint.project: Expected state ConstraintData _"))
	  
  let process partialAcc undAcc =
    G.ConstraintData (partialAcc, undAcc)
end
