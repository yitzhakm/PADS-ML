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
(* store indentation value *)
type global_state = string

let indent_str = "   " (* A unit of indentation *)
(* let indent_str = "" (\* A unit of indentation *\) *)

let print_pd_hdr h = 
  print_string
    (String.concat ", " ["nerr = " ^ string_of_int h.Pads.nerr; 
			 "error_code = " ^ Pads.string_of_error_code h.Pads.error_code;
			 "error_info = " ^ Pads.string_of_error_info h.Pads.error_info;
			 "span = " ^ Pads.string_of_span h.Pads.span])

let print_record_header indent = print_string "("
let print_record_footer indent = print_endline ")"; print_string indent

let print_dt_header indent = print_string "<"
let print_dt_footer name indent = print_endline (" " ^ name ^ ">"); print_string indent

let print_list_header indent = print_string "["
let print_list_footer indent = print_endline "]"; print_string indent

let print_constraint_header indent = print_string "{"
let print_constraint_footer indent = print_endline "}"; print_string indent


type state = global_state
exception Tool_error of state * string

let init _ = ()

module Int = struct
  type state = global_state
  type t = int
  let init () = ""
  
  let process indent res hdr = 
    (match res with
	Pads.Ok i -> print_string (string_of_int i)
      | Pads.Error -> print_string "ERROR");
    indent
end

module Float = struct
  type state = global_state
  type t = float
  let init () = ""
  
  let process indent res hdr = 
    (match res with
	Pads.Ok i -> print_string (string_of_float i)
      | Pads.Error -> print_string "ERROR");
    indent
end

module Char = struct
  type state = global_state
  type t = char
  let init () = ""
  
  let process indent res hdr = 
    (match res with
	Pads.Ok c -> print_string (String.make 1 c)
      | Pads.Error -> print_string "ERROR");
    indent
end

module String = struct
  type state = global_state
  type t = string
  let init () = ""
  
  let process indent res hdr = 
    (match res with
	Pads.Ok s -> print_string s
      | Pads.Error -> print_string "ERROR");
    indent
end

module Unit = struct
  type state = global_state
  type t = unit
  let init () = ""
  
  let process indent res hdr = 
    (match res with
	Pads.Ok () -> ()
      | Pads.Error -> print_string "ERROR");
    indent
end

module Extension = struct
  let init () = ""
  let process bty indent res hdr = 
    (match res with
	Pads.Ok s -> print_string s
      | Pads.Error -> print_string "ERROR");
    indent
end

module Record = struct
  type partial_state = string

  let sep_str = " | "  (* string used as separator between fields. *)
  
  let init _ = ""

  let start indent hdr =
    if hdr.Pads.nerr > 0 then
      (print_endline ""; print_string indent; print_pd_hdr hdr;) 
    else ();
    print_endline ""; print_string indent; 
    print_record_header indent;
    indent 

  let project indent _ = indent ^ indent_str

  let process_field indent field_name field_indent = print_string sep_str; indent

  let process_last_field indent field_name field_indent = 
    print_record_footer indent; indent

end

module Datatype = struct
  type partial_state = string

  let init _ = ""
  let start indent hdr =
    if hdr.Pads.nerr > 0 then
      (print_endline ""; print_string indent; print_pd_hdr hdr;) 
    else ();
    print_endline ""; print_string indent; 
    print_dt_header indent; 
    indent

  let project indent variant_name = Some (indent ^ indent_str)

  let process_variant indent vt_name _ = print_dt_footer vt_name indent; indent
  let process_empty_variant indent vt_name = print_dt_footer vt_name indent; indent
end

module Constraint = struct
  type partial_state = string

  let init _ = ""
  let start indent hdr = 
    if hdr.Pads.nerr > 0 then
      (print_endline ""; print_string indent; print_pd_hdr hdr;) 
    else ();
    print_endline ""; print_string indent; 
    print_constraint_header indent;
    indent

  let project indent = indent ^ indent_str
  let process indent _ = 
    print_constraint_footer indent;
    indent 
end

(** Functions for lists. *)
module List = struct
  
  type partial_state = state

  let sep_str = ";"

  let init () = ""

  let start indent hdr = 
    if hdr.Pads.nerr > 0 then
      (print_endline ""; print_string indent; print_pd_hdr hdr;) 
    else ();
    print_endline ""; print_string indent; 
    print_list_header indent;
    indent 

  let project_next indent = (indent,Some (indent ^ indent_str))

  let process_next indent elt_indent = print_string sep_str; indent

  let process_last indent elt_indent = 
    print_list_footer indent; indent

      (* Paired with start, not stand-alone. *)
  let process_empty indent = 
    print_list_footer indent;
    indent 


end
