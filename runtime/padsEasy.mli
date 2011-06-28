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
    A simplified (easy) interface to the pads runtime system.
*)

(** Indicates a failure in one of the functions.
    We define it here so that a client can distinguish between a 
    general failure (i.e. Pervasives.Failure) and a pads-specific one.
*)
exception Failure of string

(******************************************************************************************)
(* Parsing Functions                                                                          *)
(******************************************************************************************)

(** Parse one element from the named file with the given parse function. *)
val parse_with_from_file : (Pads.handle -> 'a) -> string -> 'a

(** Parse one element from stdin with the given parse function. *)
val parse_with : (Pads.handle -> 'a) -> 'a

(** Parse one element from stdin with the given parse function,
    assuming an io source without records. *)
val parse_with_norec : (Pads.handle -> 'a) -> 'a

(** Parse one element from the named file with the given parse function,
    with record discipline set by bool value. *)
val parse_source : (Pads.handle -> 'a) -> string -> bool -> 'a

(** Like parse_source, but with error reporting level set to maximum (PerrorRep_Max).
*)
val parse_source_debug : (Pads.handle -> 'a) -> string -> bool -> 'a

(** Parse one element from the file descriptor with the given parse function,
    with record discipline set by bool value. *)
val parse_fd : (Pads.handle -> 'a) -> int -> bool -> 'a

(** Launch a command (specified in the first parameter) in a separate process,
    and parse the data it sends to stdout.

    Invocation:
    parse_process command input_parser input_has_records. 

    Returns: the parsed data and the exit code of the launched command.
*)
val parse_process :
  string -> (** command *)
  (Pads.handle -> 'a) -> bool -> ('a * int)

(** Parse the source described by D.Source and print whether or not it succeeded.
    The name of the source file is optionally specified on the command line as the first argument.
    In the absence of a first argument, reads from stdin.

    Retrieves record discipline from argument D.

    Returns nothing. *)
module Parse (D:Type.Description) : sig end

(******************************************************************************************)
(* Printing Functions                                                                     *)
(******************************************************************************************)

(** arguments: a print function, a file name, boolean indicating use of records,
    a data element and its parse descriptor.
*)
val print_source : ('r,'pdb) Pads.printer -> string -> bool -> 'r -> 'pdb Pads.pd -> unit

(** arguments: a print function, boolean indicating use of records,
    a data element and its parse descriptor.
*)
val print_source_to_string : ('r,'pdb) Pads.printer -> bool -> 'r -> 'pdb Pads.pd -> string

(** arguments: a print function, boolean indicating use of records,
    a data element and its parse descriptor.
*)
val print_source_to_fd : int -> ('r,'pdb) Pads.printer -> bool -> 'r -> 'pdb Pads.pd -> unit



(** arguments: a print function for rep *only*, a file name, boolean indicating use of records,
    and a data element.
*)
val print_clean_source : 'a Pads.clean_printer -> string -> bool -> 'a -> unit

(** arguments: a print function for rep *only*, boolean indicating use of records,
    and a data element.
*)
val print_clean_source_to_string : 'a Pads.clean_printer -> bool -> 'a -> string

(** arguments: a print function for rep *only*, boolean indicating use of records,
    and a data element.
*)
val print_clean_source_to_fd : int -> 'a Pads.clean_printer -> bool -> 'a -> unit



(** 
    print_source with user-specified mode for opening the file.
*)
val print_source_mode : Pads.IO.mode -> ('r,'pdb) Pads.printer -> string -> bool -> 'r -> 'pdb Pads.pd -> unit

(** 
    print_clean_source  with user-specified mode for opening the file.
*)
val print_clean_source_mode : Pads.IO.mode -> 'a Pads.clean_printer -> string -> bool -> 'a -> unit

(******************************************************************************************)
(* Hybrid Functions                                                                       *)
(******************************************************************************************)
(** Launch a command (specified in the first parameter) in a separate process,
    print data to its stdin, and parse data from its stdout.

    Invocation:
    interact_with_process 
       command 
       clean_output_printer output_has_records output_data
       input_parser input_has_records. 

    Returns: the parsed data and the exit code of the launched command.
*)
val interact_with_process :
  string -> (** command *)
  'a Pads.clean_printer -> bool -> 'a -> 
  (Pads.handle -> 'b) -> bool -> ('b * int)

val print_process :
  string -> (** command *)
  'a Pads.clean_printer -> bool -> 'a -> 
  int
