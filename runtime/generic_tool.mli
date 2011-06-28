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
(* Utility for maintaining maps of componenets indexed by
 * some name string *)
module StringMap : Map.S with type key = string
val buildStringMap : (string * 'a) list -> 'a StringMap.t

(* Metadata: arbitrary (and XML-convertible) nested
 * descriptions of PADS values; used by tools such as
 * the accumulator *)
type 'a metadata = 
    IntData of 'a
  | FloatData of 'a
  | CharData of 'a
  | StringData of 'a
  | UnitData of 'a
  | RecordData of 'a * ('a metadata) StringMap.t
  | DatatypeData of 'a * ('a metadata) StringMap.t
  | ConstraintData of 'a * ('a metadata)
  | ExtData of 'a
  | ListData of 'a * ('a metadata)
  | TableData of 'a * ('a metadata)
  | Empty
(* Convert metadata to XML *)
val metadata_to_xml : 'a metadata -> ('a -> Xml.xml list) -> Xml.xml

val xml_to_metadata : Xml.xml -> (Xml.xml -> 'a) -> 'a metadata

module type BaseType = sig

  (** type of value of this base type. *)
  type t

  (** type of tool state for this base type. *)
  type state 

  (** Generate initial state for value of type t. *)
  val init   : unit -> state

  (** Process a value of type t. *)
  val process  : state -> t Pads.result -> Pads.pd_header -> state
end

(* Interface with which generic tools must comply; specifies
   the view of a tool as a set of callback functions to be
   invoked by the traversal routine *)
module type S = sig

  type state  (* single type for the data that gets accumulated as we traverse
               the data structure *)

  (* An error condition in the execution of a tool
   * state: The state at the moment of the error
   * string: An error msg *)
  exception Tool_error of state * string


  (** 
    Call first to intialize the tool
  *)
  val init : unit -> unit

  module Int    : BaseType with type t = int    and type state = state
  module Float  : BaseType with type t = float  and type state = state
  module Char   : BaseType with type t = char   and type state = state
  module String : BaseType with type t = string and type state = state
  module Unit   : BaseType with type t = unit   and type state = state

  (** 
    Handles all extended base types that are not part of the core set. 
    Caller indicates which the new base type and a string encoding the value.
    Note that the intent is to be extensible with respect to existing tools,
    not with respect to the runtime. In this scheme, the runtime will need 
    to be updated regardless when new base types are added as the 
    Generic_tool.BTExtension.t type will need to be changed.
  *)
  module Extension : sig
    val init     : unit -> state      
    val process  : Generic_common.BTExtension.t
                   -> state -> string Pads.result -> Pads.pd_header 
                   -> state
    end

(** Functions for tuples and records. *)
  module Record : sig
    (* The type of intermediate data accumulated during record processing *)
    type partial_state

    (** 
      Generate initial state for record after initial state for
      all components have been generated; argument is list of states
      for components, with each environement labeled with the corresponding 
      field name.
    **)
    val init  : (string * state) list -> state

    val start : state -> Pads.pd_header -> partial_state

    (** 
      Retrieve the state for subcomponent "name" of a record.
    **)
    val project : state -> string -> state

    val process_field  : partial_state -> string -> state -> partial_state

    (**
       process the last field of the record and finish processing the entire record.
    **)
    val process_last_field  : partial_state -> string -> state -> state

  end

  module Datatype : sig
    (* The type of intermediate data accumulated during sum processing *)
    type partial_state

    (** Generate initial state for sum without any state for the
	branches. Branch state will be added dynamically.  *)
    val init : unit -> state

    (**
       start sumState sumStat

       Start processing a sum
       sumState      : sum state
       sumPD     : sum status
       
       Returns: sum state
    **)
    val start : state -> Pads.pd_header -> partial_state

    (** 
      Retrieve the subcomponent state for tag "name" of a sum. 
      None is returned if there is no state corresponding to that name.
    **)
    val project : state -> string -> state option

    (**
      proc sumState eltTag eltState : 

      Process content of datatype,
      sumState    : state for sum
      eltTag    : tag of element
      eltState    : state for element

      Returns: updated sum state.
    **)
    val process_variant : partial_state -> string -> state -> state

    (** For variants that have no contents. *)
    val process_empty_variant : partial_state -> string -> state

  end

  module Constraint : sig
    (* Partially processed constraint *)
    type partial_state

    (**
      Takes state of underlying type.
    **)
    val init : state -> state

    (* Starts processing a constraint
       Prior constrained state (state)
       PD header for whole constraint (pd_header)
       Returns: Partial state for whole constraint *)
    val start : state -> Pads.pd_header -> partial_state

    (** 
      Retrieve the state of underlying type.
    **)
    val project : state -> state

    (**
      args: constrained-state underlying-state
      returns: new constrained-state
    **)
    val process : partial_state -> state -> state

  end
(** Functions for lists. *)
  module List : sig
    (* The type of intermediate data accumulated during list processing *)
    type partial_state

    (** 
      Generate initial state for record after initial state for
      all components have been generated; argument is list of states
      for components, with each environement labeled with the corresponding 
      field name.
    **)
    val init  : unit -> state

    val start : state -> Pads.pd_header -> partial_state

    (** Retrieve the state for next list element.  Returns state for
	list and optionally for the element, depending on whether there
	is any existing state for that element.  **)
    val project_next : state -> state * state option

    val process_next  : partial_state -> state -> partial_state

    (** 
	Finish processing the list.
    **)
    val process_last  : partial_state -> state -> state

    (** Process an empty list. This function is used in place of
	process_next and process_last when the list is empty. However,
	it is still paired with start.  **)
    val process_empty  : partial_state -> state

    end

  end

(************* RECORD VERSION ****************)
module Rec_ver : 
sig
  type ('t,'state) base_type = {
      (** Generate initial state for value of type t. *)
      bt_init   : unit -> 'state;

      (** Process a value of type t. *)
      bt_process  : 'state -> 't Pads.result -> Pads.pd_header -> 'state
    }

  type 'state base_type_ext = {
      btext_init   : unit -> 'state;
      btext_process  : Generic_common.BTExtension.t -> 'state -> string Pads.result -> Pads.pd_header -> 'state
    }

  type ('state,'partial_state) record_t = {
      (** 
	  Generate initial state for record after initial state for
	  all components have been generated; argument is list of states
	  for components, with each environement labeled with the corresponding 
	  field name.
      **)
      r_init  : (string * 'state) list -> 'state;

      r_start : 'state -> Pads.pd_header -> 'partial_state;

      (** 
	  Retrieve the 'state for subcomponent "name" of a record.
      **)
      r_project : 'state -> string -> 'state;

      process_field  : 'partial_state -> string -> 'state -> 'partial_state;

      (**
	 process the last field of the record and finish processing the entire record.
      **)
      process_last_field  : 'partial_state -> string -> 'state -> 'state;

    }

  type ('state,'partial_state) datatype_t = {
      (** Generate initial state for sum without any state for the
	  branches. Branch state will be added dynamically.  *)
      dt_init : unit -> 'state;

      (**
	 start sumState sumStat

	 Start processing a sum
	 sumState      : sum state
	 sumPD     : sum status
	 
	 Returns: sum state
      **)
      dt_start : 'state -> Pads.pd_header -> 'partial_state;

      (** 
	  Retrieve the subcomponent state for tag "name" of a sum. 
	  None is returned if there is no state corresponding to that name.
      **)
      dt_project : 'state -> string -> 'state option;

      (**
	 proc sumState eltTag eltState : 

	 Process content of datatype,
	 sumState    : state for sum
	 eltTag    : tag of element
	 eltState    : state for element

	 Returns: updated sum state.
      **)
      process_variant : 'partial_state -> string -> 'state -> 'state;

      (* For variants that have no contents. *)
      process_empty_variant : 'partial_state -> string -> 'state;
    }

  type ('state,'partial_state) constraint_t = {
      (** Takes state of underlying type. *)
      c_init : 'state -> 'state;

      (* Starts processing a constraint
	 Prior constrained state (state)
	 PD header for whole constraint (pd_header)
	 Returns: Partial state for whole constraint *)
      c_start : 'state -> Pads.pd_header -> 'partial_state;

      (** Retrieve the state of underlying type. **)
      c_project : 'state -> 'state;

      (**
	 args: constrained-state underlying-state
	 returns: new constrained-state
      **)
      c_process : 'partial_state -> 'state -> 'state;
    }

  (** Functions for lists. *)
  type ('state,'partial_state) list_t = {
      (** 
	  Generate initial state for record after initial state for
	  all components have been generated; argument is list of states
	  for components, with each environement labeled with the corresponding 
	  field name.
      **)
      l_init  : unit -> 'state;

      l_start : 'state -> Pads.pd_header -> 'partial_state;

      (** Retrieve the state for next list element.  Returns state for
	  list and optionally for the element, depending on whether there
	  is any existing state for that element.  **)
      l_project_next : 'state -> 'state * 'state option;

      process_next  : 'partial_state -> 'state -> 'partial_state;

      (** 
	  Finish processing the list.
      **)
      process_last  : 'partial_state -> 'state -> 'state;

      (** Process an empty list. This function is used in place of
	  process_next and process_last when the list is empty. However,
	  it is still paired with start.  **)
      l_process_empty  : 'partial_state -> 'state;
    }

  type ('state, 'record_p_state, 'datatype_p_state, 
       'con_p_state, 'list_p_state) 
      t = {
	  
	  (** Call first to intialize the tool *)
	  init : unit -> unit;

	  int_t    : (int, 'state)    base_type;
	  float_t  : (float, 'state)  base_type;
	  char_t   : (char, 'state)   base_type;
	  string_t : (string, 'state) base_type;
	  unit_t   : (unit, 'state)   base_type;
	  extension_t   : 'state base_type_ext;
	  record_t     : ('state, 'record_p_state)     record_t;
	  datatype_t   : ('state, 'datatype_p_state)   datatype_t;
	  constraint_t : ('state, 'con_p_state)        constraint_t;
	  list_t       : ('state, 'list_p_state)       list_t;
	}

  module From_mod (Tool:S) : 
  sig 
    val tool : (Tool.state,Tool.Record.partial_state,Tool.Datatype.partial_state,
	       Tool.Constraint.partial_state,Tool.List.partial_state) t
  end
end
