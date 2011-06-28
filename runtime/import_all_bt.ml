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
open Str

let spec_re = regexp "\\([^[]*\\)\\(\\[\\(.*\\)\\]\\)?";;

(** tabs and spaces *)
let ts_re    = regexp "[ \t]+"
let comma_re = regexp ","

let output_endline oc l = (output_string oc l; output_string oc "\n")

let process_line bt_specs_ic bt_make_oc bt_make_var () = 
  try 
    let line = input_line bt_specs_ic in
    let well_formed = string_match spec_re line 0 in
    let _ = 
      if not well_formed then 
	failwith ("Invalid base-type specification: " ^ line)
      else ();
    in
    let required = matched_group 1 line in
    let add_params = 
      try 
	split comma_re (matched_group 3 line)	
      with Not_found -> []
    in
    let exit_code = 
      match split ts_re required with	  
	  [] when add_params = [] -> 0         (* Empty line *)
	|  x::xs when x.[0] = '#' -> 0 	       (* Comment line. *)
	| [ptype; idl_mod; rep_type; def_val; 
	   gentool_conv; genproducer_conv; gentool_mod] ->       (* Standard line. *)
	    (* add entry to base-type make file *)
	    let _ = output_endline bt_make_oc (bt_make_var ^ " += " ^ ptype) in
		let parse_conv conv tag = if conv = "X" then [] else [tag; conv] in
	    let args = (parse_conv gentool_conv "--cc") @ (parse_conv genproducer_conv "--pc") @
	      ["-p"; ptype; "-i"; idl_mod; "-r"; rep_type; "-d"; def_val; "-m"; gentool_mod] 
	    in	  
	    let all_args = List.fold_right (fun s args -> "-a"::s::args) add_params args in
              Sys.command (String.concat " " ("./import_bt"::all_args))
	| r1::r2::r3::r4::r5::r6::r7::rs ->     (* Too many arguments. *)
	    failwith ("Invalid base-type specification: Extra required argument(s) in specification:" ^ required)	      
	| _ ->   (* Too few arguments. *)
	    failwith ("Invalid base-type specification: Missing required argument(s) in specification:" ^ required)
    in
      exit_code = 0
  with 
      End_of_file -> false

let process_all bt_specs bt_make bt_make_var = 
  let bt_specs_ic = open_in bt_specs in
  let bt_make_oc = open_out bt_make in
  let p_line = process_line bt_specs_ic bt_make_oc bt_make_var in
  let rec p_all () = 
    if p_line () then
      p_all ()
    else ()
  in
    p_all();
    close_in bt_specs_ic;
    close_out bt_make_oc

let clean_line bt_specs_ic = 
  try 
    let line = input_line bt_specs_ic in
    let well_formed = string_match spec_re line 0 in
    let _ = 
      if not well_formed then 
	failwith ("Invalid base-type specification: " ^ line)
      else ();
    in
    let required = matched_group 1 line in
    let add_params = 
      try 
	split comma_re (matched_group 3 line)	
      with Not_found -> []
    in
    let exit_code = 
      match split ts_re required with	  
	  [] when add_params = [] -> 0         (* Empty line *)
	|  x::xs when x.[0] = '#' -> 0 	       (* Comment line. *)
	| [ptype; _;_;_;_;_;_] ->   (* Standard line. *)
            Sys.command ("./import_bt --clean " ^ ptype)
	| r1::r2::r3::r4::r5::r6::r7::rs ->     (* Too many arguments. *)
	    failwith ("Invalid base-type specification: Extra required argument(s) in specification:" ^ required)	      
	| _ ->   (* Too few arguments. *)
	    failwith ("Invalid base-type specification: Missing required argument(s) in specification:" ^ required)
    in
      exit_code = 0
  with 
      End_of_file -> false

let clean_all bt_specs = 
  let bt_specs_ic = open_in bt_specs in
  let rec c_all () = 
    if clean_line bt_specs_ic then
      c_all ()
    else ()
  in
    c_all();
    close_in bt_specs_ic
;;

if Sys.argv.(1) = "--clean" then
  let bt_specs = Sys.argv.(2) in
    clean_all bt_specs
else
  let bt_specs    = Sys.argv.(1) in
  let bt_make     = Sys.argv.(2) in
  let bt_make_var = Sys.argv.(3)in
    process_all bt_specs bt_make bt_make_var

