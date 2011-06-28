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
open Generic_common

module type BaseType = sig

  (** type of value of this base type. *)
  type t

  (** type of tool input state for this base type. *)
  type in_state 

  (** type of tool output state for this base type. *)
  type out_state 

  (** Generate initial state for value of type t. *)
  val init   : unit -> in_state

  (** Process a value of type t. *)
  val process  : in_state -> t Pads.result -> Pads.pd_header -> out_state
end

module Stream = struct
  type 'a stream_elt = End | More of 'a * 'a t
  and 'a t = 'a stream_elt lazy_t

  let map f s =
	let rec _map s = 
	  match Lazy.force s with
		  End -> End
		| More (v, s) -> More ((f v), lazy (_map s))
	in
	  lazy (_map s)

  let rec to_list s =
    match Lazy.force s with
		End -> []
      | More (v, s') -> v::(to_list s')
end

(* Interface with which generic tools must comply; specifies
   the view of a tool as a set of callback functions to be
   invoked by the traversal routine *)
module type S = sig

  type in_state  (* input type for the data that gets accumulated as we traverse
                    the data structure -- these are passed in to the process 
                    functions*)
  type out_state (* output type for the data that gets accumulated as we traverse
                    the data structure -- these are returned from the process
                    functions *)

  type in_out_state = In of in_state | Out of out_state

  (* An error condition in the execution of a tool
   * state: The state at the moment of the error
   * string: An error msg *)
  exception Tool_error of in_out_state * string

  (** 
    Call first to intialize the tool
  *)
  val init : unit -> unit

  module Int    : BaseType with type t = int    and type in_state = in_state and type out_state = out_state
  module Float  : BaseType with type t = float  and type in_state = in_state and type out_state = out_state
  module Char   : BaseType with type t = char   and type in_state = in_state and type out_state = out_state
  module String : BaseType with type t = string and type in_state = in_state and type out_state = out_state
  module Unit   : BaseType with type t = unit   and type in_state = in_state and type out_state = out_state
  module Extension : BaseType with type t = BTExtension.t * string and type in_state = in_state and type out_state = out_state

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
    val init  : (string * in_state) list -> in_state

    val start : in_state -> Pads.pd_header -> partial_state

    (** 
      Retrieve the state for subcomponent "name" of a record.
    **)
    val project : in_state -> string -> in_state

    val process_field  : partial_state -> string -> out_state lazy_t -> partial_state
      
    (**
       process the last field of the record and finish processing the entire record.
    **)
    val process_last_field  : partial_state -> string -> out_state lazy_t -> out_state

  end

  module Datatype : sig
    (* The type of intermediate data accumulated during sum processing *)
    type partial_state

    (** Generate initial state for sum without any state for the
	branches. Branch state will be added dynamically.  *)
    val init : unit -> in_state

    (**
       start sumState sumStat

       Start processing a sum
       sumState      : sum state
       sumPD     : sum status
       
       Returns: sum state
    **)
    val start : in_state -> Pads.pd_header -> partial_state

    (** 
      Retrieve the subcomponent state for tag "name" of a sum. 
      None is returned if there is no state corresponding to that name.
    **)
    val project : in_state -> string -> in_state option

    (**
      proc sumState eltTag eltState : 

      Process content of datatype,
      sumState    : state for sum
      eltTag    : tag of element
      eltState    : state for element

      Returns: updated sum state.
    **)
    val process_variant : partial_state -> string -> out_state lazy_t -> out_state


    (**
      proc_empty sumState eltTag

      Process content of empty datatype,
      sumState    : state for sum
      eltTag    : tag of element

      Returns: updated sum state.
    **)
	val process_empty_variant : partial_state -> string -> out_state

  end

  module Constraint : sig
    (* Partially processed constraint *)
    type partial_state

    (**
      Takes state of underlying type.
    **)
    val init : in_state -> in_state

    (* Starts processing a constraint
       Prior constrained state (state)
       PD header for whole constraint (pd_header)
       Returns: Partial state for whole constraint *)
    val start : in_state -> Pads.pd_header -> partial_state

    (** 
      Retrieve the state of underlying type.
    **)
    val project : in_state -> in_state

    (**
      args: constrained-state underlying-state
      returns: new constrained-state
    **)
    val process : partial_state -> out_state lazy_t -> out_state
  end
  
(** Functions for lists (as lazy streams) *)
  module List : sig
    (* The type of intermediate data accumulated during list processing *)
    type partial_state 

    (** 
      Generate initial state
    **)
    val init  : unit -> in_state

    val start : in_state -> Pads.pd_header -> partial_state

    (** 
	  Retrieve the state for next list element. Returns state for element and for list
    **)
    val project_next : in_state -> in_state * in_state option

    (**
      Process the list or stream as an iterated stream; this function is called once
    **)
    val process_stream : partial_state -> out_state Stream.t -> out_state
      
  end

end

(************* RECORD VERSION ****************)
module Rec_ver = struct
  type ('t,'in_state,'out_state) base_type = {
      (** Generate initial state for value of type t. *)
      bt_init   : unit -> 'in_state;

      (** Process a value of type t. *)
      bt_process  : 'in_state -> 't Pads.result -> Pads.pd_header -> 'out_state
    }

  type ('in_state,'out_state,'partial_state) record_t = {
      (** 
	  Generate initial state for record after initial state for
	  all components have been generated; argument is list of states
	  for components, with each environement labeled with the corresponding 
	  field name.
      **)
      r_init  : (string * 'in_state) list -> 'in_state;

      r_start : 'in_state -> Pads.pd_header -> 'partial_state;

      (** 
	  Retrieve the 'in_state for subcomponent "name" of a record.
      **)
      r_project : 'in_state -> string -> 'in_state;

      process_field  : 'partial_state -> string -> 'out_state lazy_t -> 'partial_state;

      (**
	 process the last field of the record and finish processing the entire record.
      **)
      process_last_field  : 'partial_state -> string -> 'out_state lazy_t -> 'out_state;

    }

  type ('in_state,'out_state,'partial_state) datatype_t = {
      (** Generate initial state for sum without any state for the
	  branches. Branch state will be added dynamically.  *)
      dt_init : unit -> 'in_state;

      (**
	 start sumState sumStat

	 Start processing a sum
	 sumState      : sum state
	 sumPD     : sum status
	 
	 Returns: sum state
      **)
      dt_start : 'in_state -> Pads.pd_header -> 'partial_state;

      (** 
	  Retrieve the subcomponent state for tag "name" of a sum. 
	  None is returned if there is no state corresponding to that name.
      **)
      dt_project : 'in_state -> string -> 'in_state option;

      (**
	 proc sumState eltTag eltState : 

	 Process content of datatype,
	 sumState    : state for sum
	 eltTag    : tag of element
	 eltState    : state for element

	 Returns: updated sum state.
      **)
      process_variant : 'partial_state -> string -> 'out_state lazy_t -> 'out_state;


      (**
	 proc sumState eltTag  :

	 Process content of datatype,
	 sumState    : state for sum
	 eltTag    : tag of element

	 Returns: updated sum state for empty variant.
      **)
	  process_empty_variant : 'partial_state -> string -> 'out_state;
    }

  type ('in_state,'out_state,'partial_state) constraint_t = {
      (** Takes state of underlying type. *)
      c_init : 'in_state -> 'in_state;

      (* Starts processing a constraint
	 Prior constrained state (state)
	 PD header for whole constraint (pd_header)
	 Returns: Partial state for whole constraint *)
      c_start : 'in_state -> Pads.pd_header -> 'partial_state;

      (** Retrieve the state of underlying type. **)
      c_project : 'in_state -> 'in_state;

      (**
	 args: constrained-state underlying-state
	 returns: new constrained-state
      **)
      c_process : 'partial_state -> 'out_state lazy_t -> 'out_state;
    }

  (** Functions for lists. *)
  type ('in_state,'out_state,'partial_state) list_t = {
      (** 
	  Generate initial state
      **)
      l_init  : unit -> 'in_state;

      l_start : 'in_state -> Pads.pd_header -> 'partial_state;

      (** Retrieve the state for next list element.  Returns state for
	  list and optionally for the element, depending on whether there
	  is any existing state for that element.  **)
      l_project_next : 'in_state -> 'in_state * 'in_state option;
      (**
        Process the list or stream as an iterated stream; this function is called once
      **)
      process_stream  : 'partial_state -> 'out_state Stream.t -> 'out_state;

    }

  type ('in_state,'out_state, 'record_p_state, 'datatype_p_state, 
       'con_p_state, 'list_p_state) 
      t = {
	  
	  (** Call first to intialize the tool *)
	  init : unit -> unit;

	  int_t    : (int, 'in_state,'out_state)    base_type;
	  float_t  : (float, 'in_state,'out_state)  base_type;
	  char_t   : (char, 'in_state,'out_state)   base_type;
	  string_t : (string, 'in_state,'out_state) base_type;
	  unit_t   : (unit, 'in_state,'out_state)   base_type;
	  extension_t   : (BTExtension.t * string, 'in_state,'out_state) base_type;

	  record_t     : ('in_state,'out_state, 'record_p_state)     record_t;
	  datatype_t   : ('in_state,'out_state, 'datatype_p_state)   datatype_t;
	  constraint_t : ('in_state,'out_state, 'con_p_state)        constraint_t;
	  list_t       : ('in_state,'out_state, 'list_p_state)       list_t;
	}
      
  module From_mod (Tool:S) = 
  struct
    let tool = {
	init = Tool.init;
	int_t = {
	    bt_init = Tool.Int.init;
	    bt_process = Tool.Int.process;
	  };
	float_t = {
	    bt_init = Tool.Float.init;
	    bt_process = Tool.Float.process;
	  };
	char_t = {
	    bt_init = Tool.Char.init;
	    bt_process = Tool.Char.process;
	  };
	string_t = {
	    bt_init = Tool.String.init;
	    bt_process = Tool.String.process;
	  };
	unit_t = {
	    bt_init = Tool.Unit.init;
	    bt_process = Tool.Unit.process;
	  };
	extension_t = {
	    bt_init = Tool.Extension.init;
	    bt_process = Tool.Extension.process;
	  };
	record_t = {
	    r_init    = Tool.Record.init;
	    r_start   = Tool.Record.start;
	    r_project = Tool.Record.project;
	    process_field = Tool.Record.process_field;
	    process_last_field = Tool.Record.process_last_field;
	  };
	datatype_t = {
	    dt_init    = Tool.Datatype.init;
	    dt_start   = Tool.Datatype.start;
	    dt_project = Tool.Datatype.project;
	    process_variant = Tool.Datatype.process_variant;
		process_empty_variant = Tool.Datatype.process_empty_variant;
	  };
	constraint_t = {
	    c_init    = Tool.Constraint.init;
	    c_start   = Tool.Constraint.start;
	    c_project = Tool.Constraint.project;
	    c_process = Tool.Constraint.process;
	  };
	list_t = {
	    l_init    = Tool.List.init;
	    l_start   = Tool.List.start;
	    l_project_next = Tool.List.project_next;
	    process_stream = Tool.List.process_stream;
	  };
      }
  end
end
