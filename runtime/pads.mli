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
(* XXX: This interface was originally generated automatically from the
implementation. We should review it and specify some types more
appropriately than the type inference engine did. E.g. some types 'a
could be more specific.*)

(** parsing flags that can affect parsing *)
type parse_state

(** create an initial parsing state *)
val ps_init        : parse_state

(* functions for setting, unsetting and testing parse state flags *)

val ps_set_panic   : parse_state -> parse_state
val ps_unset_panic : parse_state -> parse_state
val ps_is_panic    : parse_state -> bool

val ps_set_partial   : parse_state -> parse_state
val ps_unset_partial : parse_state -> parse_state
val ps_is_partial    : parse_state -> bool

type pos

type span_info
type span = span_info option

(** arguments: function name, span and error message. *)
exception Runtime_error of string * span * string

type 'a result = Ok of 'a | Error

type error_code = Good | Maybe | Nest of span | Syn | Sem

type corrupted = string

type error_info = 
    No_info 
  | Padsc_error_code of Padsc.perrCode_t
  | Error_span of span 
  | Corrupted_data of corrupted
  | Unexpected_data_before_eor
  | Eof_before_eor
  | Unmatched_constructors
      
type pd_header = {
  state : parse_state;
  nerr : int;
  error_code : error_code;
  error_info : error_info;
  span : span;
}

type padsc_handle = Padsc.p_t Com.opaque
type handle

type 'a pd = pd_header * 'a

type base_pd_body = unit
type base_pd = unit pd

(** This type is so named to avoid a clash with keyword "parser".*)
type ('r,'pdb) parser__ = handle -> 'r * ('pdb pd)
type ('r,'pdb) printer = 'r -> ('pdb pd) -> handle -> unit
(** printer for clean data. *)
type 'r clean_printer = 'r -> handle -> unit

type reg_exp = Padsc.pregexp_t Com.opaque

val base_pd_of_pbase_pd : Padsc.pbase_pd -> pos -> base_pd

val base_pd_of_hdr : pd_header -> base_pd

val pbase_pd_of_base_pd : base_pd -> Padsc.pbase_pd

val string_of_pos : pos -> string

val string_of_span : span -> string

(** Pretty print pd header into string. Note: spans multiple lines. *)
val string_of_pd_hdr : pd_header -> string

(** 
    Convert span to descriptive string suitable for printing in parse error messsages.
*)
val location_msg_of_span : span -> string

val string_of_error_code : error_code -> string

val string_of_error_info : error_info -> string

val p_char_lit_scan1 : handle -> char -> int result

val p_str_lit_scan1 : handle -> string -> int result

val p_int_lit_scan1 : handle -> int -> int result

val p_regexp_scan1 : handle -> reg_exp -> int result

val p_regexp_str_scan1 : handle -> string -> int result

val print_char_lit: char -> handle -> unit

(* print an ocaml string as a C string. Note that any '\0's in the
   string will prematurely terminate it. *)
val print_str_lit: string -> handle -> unit

val print_int_lit: int -> handle -> unit

(* print a canonical member of a pads/ml regular expression as a C string. Note that any '\0's in the
   string will prematurely terminate it. *)
val print_re_lit: string -> handle -> unit

(** Print a stand-alone end of line marker. Used for ascii files that
    have a notion of "line".*)
val print_EOL_lit: handle -> unit

val get_pd_hdr : 'a * 'b -> 'a

(** Check whether a pd describes an error-free parse. *)
val pd_is_ok : 'a pd -> bool

module Log : sig

  type report_level =
      Info_level
    | Warning_level
    | Error_level
    | Fatal_level

  val report : report_level -> fun_name:string -> span -> error_info -> 
    msg:string option -> handle -> unit
  val report_info : fun_name:string -> span -> error_info -> msg:string option -> handle -> unit
  val report_warning : fun_name:string -> span -> error_info -> msg:string option -> handle -> unit
  val report_error : fun_name:string -> span -> error_info -> msg:string option -> handle -> unit
  val report_fatal : string -> span -> error_info -> string option -> handle -> 'a

  module NoHandle : sig
    val report: fun_name:string -> error_info -> msg:string -> unit
  end

end

val get_current_pos : handle -> pos

(** If current position is 0 (or 1:1), then returns -1 (or 1:0). *)
val get_prev_pos : handle -> pos

(* Get the current record number. *)
val get_current_record : handle -> int

(** Compare two positions. Compatible with Pervasives.compare *)
val comp_pos: pos -> pos -> int

(** Compare two positions for equality. *)
val eq_pos: pos -> pos -> bool

(** Create a zero-length span at the current position. *)
val make_empty_span : handle -> span

(** Create a span from a pair of begin and end positions, including byte at end position. *)
val make_incl_span : b:pos -> e:pos -> span

(** Create a span from a pair of begin and end positions, excluding byte at end position. *)
val make_excl_span : b:pos -> e:pos -> span

(** Merge two spans by taking beginning position of first
    and ending position of second. 
    Precondition (unchecked): first and second must be adjacent.
*)
val merge_spans : first:span -> second:span -> span

(** Create a span from the given position until the current position, exclusive. *)
val close_span : pos -> handle -> span

(** 
 * Set the end position of the given span to the current position.
 * Assumes that span is *not* None. Otherwise, will raise Match_failure. 
 *)
val finish_span : span -> handle -> span

(** Get the beginning position of a span. 
    N.B. Assumes span is of form Some x.
*)
val get_begin_pos : span -> pos

(** Get the ending position of a span 
    N.B. Assumes span is of form Some x.
*)
val get_end_pos : span -> pos

val make_valid_pd_hdr : span -> pd_header

val make_lit_error_pd_hdr : span -> pd_header

val make_empty_pd_hdr : handle -> pd_header

(** Valid pd header with span set to None. *)
val spanless_pd_hdr : pd_header

(** Create a valid PD with an empty span. *)
val make_empty_pd : handle -> base_pd

(** A good base pd for use with base type gen_pd functions. *)
val gen_base_pd : base_pd

(** Get the PADS/C handle from a PADS/ML handle *)
val get_padsc_handle: handle -> padsc_handle

(** Get the output stream from a PADS/ML handle *)
val get_out_stream: handle -> Padsc.sfioPtr

(** Initialize the system, specifying that the data source uses newline-terminated records. *)
val open_handle : unit -> handle result

(** Initialize the system, specifying that the data source does not use records. *)
val open_handle_norec : unit -> handle result

(** Initialize the system with the given io discipline. Intended for expert use only. *)
val open_handle_with_io_disc : Padsc.pio_disc_t Com.opaque -> handle result

(** Shutdown the system. *)
val close_handle : handle -> unit result

(** Create a copy of the given handle where output state is duplicated
    in the new handle and other state is shared between the original and
    the copy. *)
val share_handle : handle -> handle

type timezone = Padsc.tm_zone_t

val timezone_of_string : string -> timezone option

(** 
    Functions for manipulating values within the Pads handle's discipline.
*)
module Discipline : sig
  val get_error_reporting : handle -> Padsc.perrorRep
  val set_error_reporting : handle -> Padsc.perrorRep -> unit
end

module IO : sig
  type mode_option = Write | Append | Read | Read_write | Mutex | String | String_rw
  type mode = mode_option list

  (** DEPRECATED. Use open_in_file instead.
      Open the handle's input file. The second argument is the file name. *)
  val open_file : handle -> string -> unit result

  (** Launch a separate process.

      The string argument is a command to be passed to /bin/sh.  
      The mode option controls to what the returned sfio stream is connected.
        - Read: read-only stream connected to process's stdout.
        - Write: write-only stream connected to process's stdin.
        - Read_write: read, write stream connected to process's stdout, stdin, respectively.      
   *)
  val open_process : string -> mode_option -> Padsc.sfioPtr result


  (** Launch a separate process and set the provided handle to read
      from the process' stdout.

      The string argument is a command to be passed to /bin/sh.  
  *)
  val open_process_read : handle -> string -> Padsc.sfioPtr result

  (** Open the handle's input file. 
      The second argument is the file name. 

      TODO: document what happens to any existing open input stream.
  *)
  val open_in_file : handle -> string -> unit result

  (** 
      Set the handle's input stream to the given sfio stream.
      If there is already an installed sfio stream, P_io_close is
      implicitly called first.
  *)
  val set_in :  handle -> Padsc.sfioPtr -> unit result

  (** 
      Set the handle's input stream to the given file descriptor. 
      If there is already an installed sfio stream, P_io_close is
      implicitly called first.

      Mode is Read.
  *)
  val set_in_to_fd :  handle -> int -> unit result

  (** Open the handle's output file. The second argument is the file name.
    Does *not* close current output file (if any). Merely replaces it.  
  *)
  val open_out_file : handle -> string -> unit result

  (** Open a new ouput stream for the handle without replacing the
      current out stream. Return a new handle containing the new out
      stream. All other state is unchanged.  The second argument is
      the file name.  *)
  val open_add_out_file : handle -> string -> handle result

  (** 
      Open the handle's output file. The second argument is the file name.
      Does *not* close current output file (if any). Merely replaces it.  

      User can specify the mode of the file. 
  *)
  val open_out_file_mode :  mode -> handle -> string -> unit result

  (** Open a new ouput stream for the handle without replacing the
      current out stream. Return a new handle containing the new out
      stream. All other state is unchanged.  The second argument is
      the file name. 

      User can specify the mode of the file. 
   *)
  val open_add_out_file_mode : mode -> handle -> string -> handle result

  (** 
      Open the handle's output stream as a string. 
      Does *not* close current output file (if any). Merely replaces it.  
  *)
  val open_out_string :  handle -> unit result

  (** 
      Set the handle's output stream to the given sfio stream.
      Does *not* close current output stream (if any). Merely replaces it.  
  *)
  val set_out :  handle -> Padsc.sfioPtr -> unit

  (** 
      Set the handle's output stream to the given file descriptor. 
      Does *not* close current output file (if any). Merely replaces it.  

      File mode is write.
  *)
  val set_out_to_fd :  handle -> int -> unit result

  (** 
      Set the handle's output stream to the given file descriptor. 
      Does *not* close current output file (if any). Merely replaces it.  

      User can specify the mode of the file. (Should be Append or Write).
  *)
  val set_out_to_fd_mode :  mode -> handle -> int -> unit result

  (** DEPRECATED. Use close_in instead.
      Close the input file for this pads handle.  
   *)
  val close_in_file : handle -> unit result

  (** Close the input IO for this pads handle. 

      If it is a file, opened with open_in_file, then the file is closed, 
      and this call is equivalent to close_in_file.

      If it is an sfio stream set by the user, the stream is not closed, but
      unused bytes are returned.
  *)
  val close_in : handle -> unit result

  (** Close the output IO file for this pads handle. *)
  val close_out_file : handle -> unit result

  (** Close the output IO string for this pads handle, and return the string. 
      integer argument is optional maximum size of returned string. If None, 
      maximum size is max_int.
  *)
  val close_out_string : int option -> handle -> string result

  (** Close the output sfio stream. See sfio documentation for details
      on how sfclose affects different sfio streams, and for meaning of return result.
  *)
  val close_out : handle -> int

  val is_speculative : handle -> bool
  val get_spec_level : handle -> int

  (* XXX:These functions should really only be available to other runtime modules, not 
     the general module public.  *)
  (** First argument indicates whether checkpoint is for speculative parsing.
      true indicates that it is for speculation.
  *)
  val checkpoint : bool -> handle -> unit
  val commit :     handle -> unit
  val restore :    handle -> unit
end

(** 
  Raised when an error is encounterd during a speculative
  parse. Speculative parses are used by datatypes to try out the
  different variants.
*)
exception Speculation_failure

val find_eor : pd_header -> handle -> pd_header

(** Append an open record marker (as applicable) to the current out stream. 
    The out stream must be open.
*)
val print_open_rec : handle -> unit

(** Append a close record marker (as applicable) to the current out stream. 
    The out stream must be open.
*)
val print_close_rec : handle -> unit

module Record : sig
  val create_pd_hdr : handle -> pd_header
  val finish_pd_hdr : pd_header -> handle -> pd_header

  (* Update an existing hdr and with the pd from a subcomponent. *)
  val update_pd_hdr : pd_header -> pd_header -> pd_header

  val parse_first : ('a,'b) parser__ -> handle -> 'a * ('b pd) * pd_header
  val parse_next : ('a,'b) parser__ -> pd_header -> handle -> 'a * ('b pd) * pd_header

  val absorb_first : ('a,'b) parser__ -> handle -> pd_header
  val absorb_next : ('a,'b) parser__ -> pd_header -> handle -> pd_header

  val absorb_first_char : lit:char -> handle -> pd_header
  val absorb_next_char  : lit:char -> pd_header -> handle -> pd_header

  val absorb_first_string :  lit:string -> handle -> pd_header
  val absorb_next_string  :  lit:string -> pd_header -> handle -> pd_header

  val absorb_first_int : lit:int -> handle -> pd_header
  val absorb_next_int  : lit:int -> pd_header -> handle -> pd_header

  val absorb_first_regexp :  lit:reg_exp -> handle -> pd_header
  val absorb_next_regexp  :  lit:reg_exp -> pd_header -> handle -> pd_header

  val absorb_first_regexp_str :  lit:string -> handle -> pd_header
  val absorb_next_regexp_str  :  lit:string -> pd_header -> handle -> pd_header

  val gen_pd : pd_header -> 'a pd -> 'a pd * pd_header
end

module Compute : sig

  (** Make a parsing function from a computed value and gen_pd function. *)
  val generate_parser : 'a -> ('a -> 'b pd) -> ('a,'b) parser__

end

module Where : sig
  val gen_pd : 'a pd -> bool -> ('a pd) pd 
  val make_pd : 'a pd -> bool -> handle -> ('a pd) pd
  val parse_underlying : ('a,'b) parser__ -> ('a -> bool) -> ('a,('b pd)) parser__
end

module Datatype : sig
  val parse_variant  : ('a,'b) parser__ -> handle -> ('a * ('b pd)) option
  val absorb_variant : ('a,'b) parser__ -> handle -> span option
  val absorb_char_variant   : lit:char   -> handle -> span option
  val absorb_string_variant : lit:string -> handle -> span option
  val absorb_int_variant    : lit:int    -> handle -> span option
  val absorb_regexp_variant : lit:reg_exp -> handle -> span option
  val absorb_regexp_str_variant : lit:string -> handle -> span option
  val absorb_EOL_variant : lit:unit -> handle -> span option

  val parse_case  : ('a,'b) parser__ -> ('a -> 'c) -> ('b pd -> 'd) -> ('c, 'd) parser__
  val absorb_case : ('a,'b) parser__ -> 'c -> 'd -> ('c, 'd) parser__
  val absorb_char_case   : char -> 'a -> 'b -> ('a, 'b) parser__
  val absorb_string_case : string -> 'a -> 'b -> ('a, 'b) parser__
  val absorb_int_case    : int -> 'a -> 'b -> ('a, 'b) parser__
  val absorb_regexp_case : reg_exp -> 'a -> 'b -> ('a, 'b) parser__
  val absorb_regexp_str_case : string -> 'a -> 'b -> ('a, 'b) parser__
  val absorb_EOL_case : unit -> 'a -> 'b -> ('a, 'b) parser__

  (** args: gen_rep genpd_fn rep_constructor pd_constructor *)
  val gen_case : 'a -> ('a -> 'b pd) -> ('a -> 'c) -> ('b pd -> 'd) -> ('c,'d) parser__
  
  val make_pd_hdr : pd_header -> pd_header
  val make_rep : 'a -> 'a

  (** Create a pd when there are no errors. *)   
  val make_pd : pd_header * 'a -> 'a pd

  val make_err_pd : handle -> 'a -> 'a pd
  (** If speculative, raise exception. Otherwise, return pd. *)
  val handle_error_variant : handle -> 'a -> 'a pd

(*   val make_gen_pd : handle -> 'a -> 'a pd *)
  val make_absorb_pd : span -> 'a -> 'a pd

  val gen_pd : 'a pd -> ('a pd -> 'b) -> 'b pd
  (* Generate the pd when the variant has no subcomponent. *)
  val gen_pd_empty : 'b -> 'b pd
end
