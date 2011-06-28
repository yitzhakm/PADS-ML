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
(** Driver for the compiler *)

(* The source for the file; uses a randomized name by default

   This name will be used as the schema URL.
*)
let source_file : string ref = 
  (* make a random string of six digits *)
  Random.self_init ();
  let random_number = Random.int 1000000 in
  let random_string = Printf.sprintf "%06d" random_number in
	ref ("unnamed_spec" ^ random_string ^ ".pml")

(* The channel from which to read the .pml file *)
let input = ref stdin

(* The name of the file to which to write the .ml file *)
let output_file = ref "/dev/stdout"

(* The (optional) channel to which to write the .xsd file *)
let schema_output = ref None
(* Whether or not to produce module signatures *)
let produce_sigs = ref false

let close_channels () =
  close_in !input;
  match !schema_output with
	  Some out -> close_out out
	| None -> ()

(* Functions to set up the input and output files based on CL arguments *)
let set_output_file ofilename =
  output_file := ofilename

let set_input_file ifilename =
  source_file := ifilename;
  input := open_in ifilename;
  if !output_file = "/dev/stdout"
  then let ofilename = (Filename.chop_extension ifilename) ^ ".ml" in
		 set_output_file ofilename

let set_schema_output_file sofilename =
  schema_output := Some (open_out sofilename)

(* Command-line options for the Arg argument parsing module *)
let cli_options = [
  ("-o", Arg.String set_output_file, "Set the filename for ML output (if an input file fmt.pml is specified, then fmt.ml is written; otherwise, results are on stdout)");
  ("--schema", Arg.String set_schema_output_file, "Set the filename for XSchema output; by default, nothing is done");
  ("-s", Arg.Set produce_sigs, "Produce signatures along with each module; by default, signatures are not produced.")
]

let run () =
  try 
    (* Parse the input PML file *)
    let ast = PadsML_grammar.parse_pml (Stream.of_channel !input) in
    
    (* Run the compiler *)
    let compiled = Compiler.compile_pml_to_ocaml !produce_sigs !source_file ast in
    let output = open_out !output_file in
    
	(* Show the generated XSchema *)
	(match !schema_output with
		Some out -> XSchemaCompiler.output_schema out
	  | None -> ());

	(* Output the standard code preamble. *)  
	PadsML_grammar.fprint_pml output Compiler.preamble;
	(* Output the generated .ml file *)
	PadsML_grammar.fprint_pml output compiled;

	(* Clean up *)
	close_out output;
	close_channels ();
    with 
	Ploc.Exc(loc, e) -> 
	  let loc_string =   
	    let b = Ploc.first_pos loc - Ploc.bol_pos loc in
	    let e = Ploc.last_pos loc - Ploc.bol_pos loc in
	    "Line " ^ string_of_int (Ploc.line_nb loc) ^
	      ", characters " ^ string_of_int b ^
	      " - " ^ string_of_int e
	  in
	  prerr_endline (loc_string ^ " : parse error.");
	  close_channels ();
	  raise e

(* Parse the command-line options *)
let _ = 
  Arg.parse cli_options set_input_file "padsmlc [options] [input filename]";
  run ()

