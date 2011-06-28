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
(** parsing flags that can affect parsing *)
type parse_state = bool * bool
    
let ps_init = (false,false)

let ps_set_panic (_,par) = (true,par)
let ps_unset_panic (_,par) = (false,par)
let ps_is_panic (p,_) =  p

let ps_set_partial (pan,_) = (pan,true)
let ps_unset_partial (pan,_) = (pan,false)
let ps_is_partial (_,p) =  p

(** position in data source *)
type pos = Padsc.ppos_t

type span_info = Padsc.ploc_t
(** span in data source *)
type span = span_info option

(** arguments: function name, span and error message. *)
exception Runtime_error of string * span * string

(* XXX: I think that all or nearly all uses of results should be replaced by exceptions. *)

type 'a result = Ok of 'a | Error

type error_code =
    Good                   (* all good recursively *)
  | Maybe                  (* possible error, possibly not *)
  | Nest of span           (* nested error at specified span. *)
  | Syn                    (* syntactic error *)
  | Sem                    (* do we want to say somehow what the
                              semantic constraint was or why it
                              was violated?  How would we say that?
                              Perhaps use an 'a pd with clause
                              Sem of 'a -> bool  where the function is the
                              semantic function that returns false? *)

type corrupted = string   (* the raw string that we couldn't parse. *)

(** detailed information about the error. *)
type error_info =
    No_info
  | Padsc_error_code of Padsc.perrCode_t
  | Error_span of span
  | Corrupted_data of corrupted
  | Unexpected_data_before_eor
  | Eof_before_eor
  | Unmatched_constructors

type pd_header   = {
  state        : parse_state;
  nerr         : int;
  error_code   : error_code;
  error_info   : error_info;
  span         : span
}

type padsc_handle = Padsc.p_t Com.opaque
(* type sfioStream = Sf_file of Padsc.sfioPtr | Sf_string of Padsc.sfioPtr * string *)
(* type handle = padsc_handle * (sfioStream ref) *)
type handle = padsc_handle * (Padsc.sfioPtr ref)

type 'a pd = pd_header * 'a
type base_pd_body = unit
type base_pd     = unit pd

(* This definition appends __ to avoid a clash with keyword "parser".*)
type ('a,'b) parser__ = handle -> 'a * ('b pd)
type ('a,'b) printer = 'a -> ('b pd) -> handle -> unit
type 'a clean_printer = 'a -> handle -> unit

type reg_exp = Padsc.pregexp_t Com.opaque  

let get_padsc_handle (h,_) = h
let get_out_stream (_,os_ref) = !os_ref

let string_of_pos p = 
  if p.Padsc.num > 0 then
    "record " ^ string_of_int p.Padsc.num ^ ", " ^ 
      "character " ^ string_of_int p.Padsc.fIELD_BYTE
  else
    "offset " ^ Int64.to_string p.Padsc.offset
	  
let location_msg_of_span = function
    None -> "unknown location"
  | Some {Padsc. b=b; e=e} -> 
      if b.Padsc.num > 0 then
	begin
	  if b.Padsc.num = e.Padsc.num then
	    "record " ^ string_of_int b.Padsc.num ^ ", " ^ 
	      (if b.Padsc.fIELD_BYTE = e.Padsc.fIELD_BYTE then 
		  "character " ^ string_of_int b.Padsc.fIELD_BYTE
		else
		  "characters " ^ string_of_int b.Padsc.fIELD_BYTE ^
		    " - " ^ string_of_int e.Padsc.fIELD_BYTE)
	  else
	    "record " ^ string_of_int b.Padsc.num ^ ", " ^ "character " ^ string_of_int b.Padsc.fIELD_BYTE ^
	      " - record " ^ string_of_int e.Padsc.num ^ ", " ^ "character " ^ string_of_int e.Padsc.fIELD_BYTE
	end
      else
	begin
	  if b.Padsc.offset = e.Padsc.offset then
	    "offset " ^ Int64.to_string b.Padsc.offset
	  else if b.Padsc.offset = (Int64.succ e.Padsc.offset) then
	    "around offset " ^ Int64.to_string b.Padsc.offset
	  else 
	    "offsets " ^ Int64.to_string b.Padsc.offset ^ " - " ^ Int64.to_string e.Padsc.offset
	end

let string_of_span = function
    None -> "No span recorded"
  | Some {Padsc.
	  b={Padsc.num=0; offset=off_b};
	  e={Padsc.num=0; offset=off_e}} ->
      "[" ^ Int64.to_string off_b ^ "," ^ Int64.to_string off_e  ^ "]"
  | Some {Padsc.
	  b={Padsc.fIELD_BYTE=fb_b; num=num_b};
	  e={Padsc.fIELD_BYTE=fb_e; num=num_e}} ->
	    "[" ^ string_of_int num_b ^ ":" ^ string_of_int fb_b ^
	      "," ^ string_of_int num_e ^ ":" ^ string_of_int fb_e ^ "]"

let string_of_error_code = function
    Good -> "Good"
  | Maybe -> "Maybe"
  | Nest sp -> "Nested error at " ^ location_msg_of_span sp
  | Syn -> "Syn"
  | Sem -> "Sem"

let string_of_error_info = function
    No_info -> "No information"
  | Padsc_error_code _ -> "PADS/C error code"
  | Error_span sp -> "Error at span " ^ string_of_span sp
  | Corrupted_data d -> "Corrupted data: " ^ d
  | Unexpected_data_before_eor -> "Unexpected data before EOR"
  | Eof_before_eor -> "EOF before EOR"
  | Unmatched_constructors -> "Rep and PD datatype constructors do not match"

let string_of_pd_hdr h = 
  String.concat "\n" (
      [("   Data location: " ^ location_msg_of_span h.span)] @
	(if h.nerr > 0 then 
	    ["   Number of subcomponents with errors: " ^ string_of_int h.nerr]
	  else [])  @
	(if not (h.error_code = Good) then 
	    ["   Error code: " ^ string_of_error_code h.error_code]
	  else []) @
	(if not (h.error_info = No_info) then 
	    ["   Error message: " ^ string_of_error_info h.error_info ^ "."]
	  else []))

let comp_pos p1 p2 = compare p1 p2

let eq_pos p1 p2 = p1 = p2

let get_begin_pos = function Some {Padsc.b=b} -> b

let get_end_pos = function Some {Padsc.e=e} -> e

let make_incl_span ~b ~e = 
  Some {Padsc.b=b; e=e}

let make_excl_span ~b ~e:{Padsc.fIELD_BYTE = fb; num = n; offset = o} = 
  (* FIX: If fb = 0 then we should really go back to previous
     record. Need special pads/c function to do this so as to get new fb
     correct.*)
  Some {Padsc.b=b; e={Padsc.fIELD_BYTE = fb-1; num = n; offset = Int64.pred o}}

let merge_spans ~first ~second = 
  match (first,second) with
      (Some {Padsc.b=b}, Some {Padsc.e=e}) -> Some {Padsc.b=b;Padsc.e=e}

let make_valid_pd_hdr sp = 
  {state= ps_init;
   nerr= 0; 
   error_code = Good; 
   error_info = No_info;
   span= sp}

let spanless_pd_hdr = make_valid_pd_hdr None

(* Make a pd header for case when we encounter an error while parsing a literal. *)
let make_lit_error_pd_hdr sp  = 
  {state = ps_init; nerr = 1; error_code = Syn;
   error_info = Error_span sp; span = sp}
    
let gen_base_pd = (spanless_pd_hdr,())

let get_pd_hdr (h,_) = h

let pd_is_ok (h,_) = h.nerr = 0

(* PADS/C --->  PADS/ML: Conversion functions from PADS/C to PADS/ML. *)

let parse_state_of_pflags f =
  (* We can cut it down to an int first because we know we only care
    about lower bits. *)
  let int_f = Int32.to_int f in
  let panic = (int_f land 0x0001) > 0 in
  let partial = (int_f land 0x0002) > 0 in
    (panic,partial)  

let pflags_of_parse_state (panic,partial) =
  let f = if panic then 0x0001 else 0x0 in
  let f = if partial then f lor 0x0002 else f in
    Int32.of_int f

(* Conversion function between padsc results (type perror_t) and
   padsml results (type result). *)

let result_of_perror_t v = 
  function
      Padsc.P_OK -> Ok v
    | Padsc.P_ERR -> Error
	
(* Conversion function between padsc error codes (type perrCode_t) and
   padsml error codes (type error_code).  *)

let ec_of_pec = function
    Padsc.P_NO_ERR -> Good
  | _ -> Syn

(* Conversion function between padsc error codes (type perrCode_t) and
   padsml error information (type error_info). *)

let ei_of_pec = function 
    Padsc.P_EXTRA_BEFORE_EOR -> Unexpected_data_before_eor
  | Padsc.P_EOF_BEFORE_EOR -> Eof_before_eor
  | pec -> Padsc_error_code pec

let pec_of_ei = function
    Padsc_error_code ec -> ec
  | No_info -> Padsc.P_NO_ERR
  | Unexpected_data_before_eor -> Padsc.P_EXTRA_BEFORE_EOR
  | Eof_before_eor -> Padsc.P_EOF_BEFORE_EOR
  | _ -> Padsc.P_UNEXPECTED_ERR

(* Ploc to span. *)
let span_of_loc (l:Padsc.ploc_t) = Some l
let bpos_of_loc (l:Padsc.ploc_t) = l.Padsc.b
let epos_of_loc (l:Padsc.ploc_t) = l.Padsc.e

let empty_loc =
  let bpos = {Padsc.fIELD_BYTE = 0; Padsc.num = 0; Padsc.offset = Int64.zero} in
  let epos = {Padsc.fIELD_BYTE = 0; Padsc.num = 0; Padsc.offset = Int64.zero} in
    {Padsc.b = bpos; Padsc.e = epos}

let loc_of_span  = function
    None -> empty_loc
  | Some l -> l

(* PADS/C base pd to PADS/ML base pd. *)
let base_pd_of_pbase_pd 
    { Padsc.pstate = pstate;
      nerr = nerr;
      errCode = errCode;
      loc = loc;} 
    prev_pos =
  let span = 
    if nerr = 0 then
      (* End loc is not set in PADS/C runtime, so must set it now. *)
      make_incl_span (bpos_of_loc loc) prev_pos
    else 
      (* There was an error, so the end pos. is set. *)
      span_of_loc loc 
  in
  let hdr = {state      = parse_state_of_pflags pstate;
	     nerr       = nerr;
	     error_code = ec_of_pec errCode;
	     error_info = ei_of_pec errCode;
	     span       = span} in
    (hdr,())

let base_pd_of_hdr hdr = (hdr, ())

let pbase_pd_of_base_pd 
    ({state = state; nerr = nerr;
      error_info = ei; span = sp}, ()) =
  {Padsc.pstate = pflags_of_parse_state state;
   Padsc.nerr = nerr;
   Padsc.errCode = pec_of_ei ei;
   Padsc.loc = loc_of_span sp;}

(* INTERNAL LOGGING *)

module Log = struct

  type report_level =
      Info_level
    | Warning_level
    | Error_level
    | Fatal_level

  let (pc_LEV_INFO,
       pc_LEV_WARN, 
       pc_LEV_ERROR, 
       pc_LEV_FATAL) =
    Padsc.p_get_error_levels ()


  let pc_lev_of_lev = function
      Info_level -> pc_LEV_INFO
    | Warning_level -> pc_LEV_WARN
    | Error_level -> pc_LEV_ERROR
    | Fatal_level -> pc_LEV_FATAL
      

  let report level ~fun_name span error_info ~msg pads = 
    ignore (Padsc.pDCI_report_err 
      (get_padsc_handle pads) 
      (pc_lev_of_lev level)
      (loc_of_span span) 
      (pec_of_ei error_info)
      fun_name msg)

  let report_info = report Info_level
  let report_warning = report Warning_level
  let report_error = report Error_level
  let report_fatal fun_name span error_info msg pads = 
    report Fatal_level fun_name span error_info msg pads;
    (* This line is never reached as Fatal_level kills the program. 
       We include the code so that the type checker will assign it type 'a. *)
    raise Exit 

  module NoHandle = struct
    let report ~fun_name error_info ~msg =
      print_endline ("Error in function " ^ fun_name ^ ": " ^ msg)
  end
    
end

(* Span and pos-related functions that use the pads handle. *)
 
let get_current_pos pads = 
  match Padsc.p_io_getPos (get_padsc_handle pads) 0 with
      (Padsc.P_OK,p) -> p
    | (Padsc.P_ERR,_) -> Log.report_fatal "Pads.get_current_pos" None (Padsc_error_code Padsc.P_IO_ERR) 
	(Some "Failed to get current position") pads (* Should never happen. *)

let get_prev_pos pads = 
  match Padsc.p_io_getPos (get_padsc_handle pads) (-1) with
      (Padsc.P_OK,p) -> p
    | (Padsc.P_ERR,p) -> 
	(* In case of error, pads/c returns {num=0;byte=0;offset=-1}.
	   While that's right if we're not using records,
	   it's wrong for records, as we assume num > 0 (always). 
	   So, we get the current position to check whether we're using records
	   and then act accordingly.
	*)
	let p_cur = get_current_pos pads in
	if (p_cur.Padsc.num = 0) then 
	  (* we're not using records, so just return the pads/c position. *)
	  p
	else 
	  (* we're using records, so provide our own value*)
	  {Padsc.num=1; fIELD_BYTE=0; offset=Int64.minus_one}

let get_current_record pads = 
  match Padsc.p_io_getPos (get_padsc_handle pads) 0 with
      (Padsc.P_OK,{Padsc.num = n}) -> n
    | (Padsc.P_ERR,_) -> Log.report_fatal "Pads.get_current_record" None (Padsc_error_code Padsc.P_IO_ERR) (Some "Failed to get current recrod") pads (* Should never happen. *)

let make_empty_span pads = 
  let c = get_current_pos pads in 
  let p = get_prev_pos pads in 
  make_incl_span c p

let close_span b pads =
  make_incl_span b (get_prev_pos pads)

(** Assumes that span is *not* None. Otherwise, will raise Match_failure. *)
let finish_span span pads =
  let b_pos = get_begin_pos span in
    close_span b_pos pads

let make_empty_pd_hdr pads = make_valid_pd_hdr (make_empty_span pads)

let make_empty_pd pads = (make_empty_pd_hdr pads,())
    
(* These functions dealt with pstrings when they were opaque. Hence,
   no longer applicable. *)
(* let string_of_pstring = Padsc.string_of_pstring *)

(* let pstring_of_string s pads = *)
(*   let len = String.length s in *)
(*     match (Padsc.pstring_cstr_copy (get_padsc_handle pads) s len) with *)
(* 	(Padsc.P_OK,pstring) -> pstring *)
(*       | (Padsc.P_ERR,_) -> Log.report_fatal "pstring_of_string" None No_info  *)
(* 	  "C string to Pstring copy failed." pads *)
	  

(* SCANNING FUNCTIONS *)

let p_char_lit_scan1 pads c = 
  let (e,offset) = Padsc.pchar_lit_scan1 (get_padsc_handle pads) c 1 0 in
    (result_of_perror_t offset e)

let p_str_lit_scan1 pads s = 
  let (e,offset) = Padsc.pstr_lit_scan1 (get_padsc_handle pads) s 1 0 in
    (result_of_perror_t offset e)

let p_int_lit_scan1 pads i = 
  let (e,offset) = Padsc.pstr_lit_scan1 (get_padsc_handle pads) (string_of_int i) 1 0 in
    (result_of_perror_t offset e)

let p_regexp_scan1 pads regexp = 
  let (e,offset) = Padsc.pre_scan1 (get_padsc_handle pads) regexp 1 0 in
    (result_of_perror_t offset e)

let p_regexp_str_scan1 pads regexp_str =
    (* XXX: use exceptions here for error handling. *)
    let ph = get_padsc_handle pads in
    let (err,re) = Padsc.pregexp_alloc ph in
    let err = Padsc.pregexp_compile_cstr ph regexp_str re in
    let res = p_regexp_scan1 pads re in
    let err = Padsc.pregexp_cleanup ph re in
    let err = Padsc.pregexp_free ph re in
      res

(* include unused () parameter to make type consistent with that of other scan funcitons.*)
let p_EOL_lit_scan pads () = 
  let res, v = Padsc.p_io_next_rec (get_padsc_handle pads) in
    result_of_perror_t v res

let print_char_lit c pads = 
  ignore (Padsc.pchar_lit_write2io (get_padsc_handle pads) (get_out_stream pads) c)

let print_str_lit s pads = 
  ignore (Padsc.pstr_lit_write2io (get_padsc_handle pads) (get_out_stream pads) s)

let print_int_lit i pads = 
  ignore (Padsc.pstr_lit_write2io (get_padsc_handle pads) (get_out_stream pads) 
                                (string_of_int i))

let print_re_lit s pads = 
  let mem = Strhack.gen_canonical (String.sub s 1 (String.length s - 2)) in
    print_str_lit mem pads

(** Call open handle with all parameters *)
let open_handle_with_all disc_opt rec_disc =
  let (ec,h) =  Padsc.p_open disc_opt rec_disc in
    match Padsc.p_fopen (Some "/dev/stdout") "a" with
	None -> Error
      | Some out -> result_of_perror_t (h,ref out) ec

let open_handle_with_io_disc rec_disc =
  let (ec,h) =  Padsc.p_open None rec_disc in
  match ec with
      Padsc.P_ERR -> Error
    | Padsc.P_OK -> 
	begin
          Padsc.p_set_disc_e_rep h Padsc.PerrorRep_None;
	  match Padsc.p_fopen (Some "/dev/stdout") "a" with
	      None -> Error
	    | Some out -> Ok (h,ref out)
	end

(** Open the handle, and set the io discipline to use newline-terminated records. *)
let open_handle () = open_handle_with_io_disc (Padsc.p_ctrec_make (int_of_char '\n') 0)

(** Open the handle, and set the io discipline not to use records. *)
let open_handle_norec () = open_handle_with_io_disc (Padsc.p_norec_make 0)

let close_handle pads = result_of_perror_t () (Padsc.p_close (get_padsc_handle pads))  

let share_handle (ph,out_ref) = let out = !out_ref in (ph, ref out)

type timezone = Padsc.tm_zone_t

let timezone_of_string = Padsc.p_cstr2timezone

module Discipline = struct
  let set_error_reporting (handle,_) level = Padsc.p_set_disc_e_rep handle level
  let get_error_reporting (handle,_) = Padsc.p_get_disc_e_rep handle
end

module IO = struct
  type mode_option = Write | Append | Read | Read_write | Mutex | String | String_rw
  type mode = mode_option list

  let read_flag, write_flag, appendwr_flag, string_flag, mtsafe_flag = Padsc.sfgetmodeflags ()

  let sfio_of_mode_op = function 
      Write -> "w" 
    | Append -> "a" 
    | Read -> "r" 
    | Read_write -> "r+" 
    | Mutex -> "m"
    | String -> "s"
    | String_rw -> "s+"
    
  let sfio_of_mode modes = String.concat "" (List.map sfio_of_mode_op modes)

  let sfio_flag_of_mode_op = function 
      Write -> write_flag
    | Append -> appendwr_flag
    | Read -> read_flag
    | Read_write -> read_flag lor write_flag 
    | Mutex -> mtsafe_flag
    | String -> string_flag
    | String_rw -> string_flag lor read_flag lor write_flag

  let sfio_flags_of_mode modes = 
    let or_flag current mop = current lor (sfio_flag_of_mode_op mop) in
    List.fold_left or_flag 0 modes

  let open_in_file pads file_name = result_of_perror_t () (Padsc.p_io_fopen (get_padsc_handle pads) file_name)
  let open_file = open_in_file

  let set_in (ph,_) sfio = result_of_perror_t () (Padsc.p_io_set ph sfio)

  let set_in_to_fd (ph,_) fd = 
    match Padsc.simple_sfnew fd (sfio_flag_of_mode_op Read) with
	None -> Error
      | Some io -> result_of_perror_t () (Padsc.p_io_set ph io)

  let open_out_string (_, out_ref) = 
    match Padsc.sfopen None None (sfio_of_mode [String_rw]) with
	None -> Error
      | Some io -> 
	  out_ref := io; 
	  Ok ()

  let set_out (_, out_ref) sfio = out_ref := sfio

  let set_out_to_fd (_, out_ref) fd = 
    match Padsc.simple_sfnew fd (sfio_flag_of_mode_op Write) with
	None -> Error
      | Some out -> 
	  out_ref := out; 
	  Ok ()

  let set_out_to_fd_mode mode (_, out_ref) fd = 
    match Padsc.simple_sfnew fd (sfio_flags_of_mode mode) with
	None -> Error
      | Some out -> 
	  out_ref := out; 
	  Ok ()

  let open_out_file_mode mode (_, out_ref) file_name = 
    match Padsc.p_fopen (Some file_name) (sfio_of_mode mode) with
	None -> Error
      | Some out -> (out_ref := out; Ok ())

  let open_add_out_file_mode mode pads file_name = 
    match Padsc.p_fopen (Some file_name) (sfio_of_mode mode) with
	None -> Error
      | Some out -> Ok (get_padsc_handle pads, ref out)

  let open_out_file pads file_name = open_out_file_mode [Write] pads file_name
  let open_add_out_file pads file_name = open_add_out_file_mode [Write] pads file_name

  let open_process cmd mode_op = 
    match Padsc.sfpopen None cmd (sfio_of_mode_op mode_op) with
	None -> Error
      | Some x -> Ok x

  let open_process_read (ph2, _) cmd = 
    let res = Padsc.sfpopen None cmd (sfio_of_mode_op Read) in
    match res with
	None -> Error
      | Some io ->
	  let res = Padsc.p_io_set ph2 io in
	  match res with 
	      Padsc.P_OK -> Ok io
	    | Padsc.P_ERR -> 
		Padsc.sfclose io; Error

  let close_in (ph,_) = 
    result_of_perror_t () (Padsc.p_io_close ph)

  let close_in_file = close_in

  let close_out_file (ph, out_ref) = 
    result_of_perror_t () (Padsc.p_fclose !out_ref)

  let close_out_string max_buf_size_opt (ph, out_ref) = 
    let max_buf_size = match max_buf_size_opt with Some m -> m | None -> max_int in
    let n = Padsc.sfsize !out_ref in
      if n > (Int64.of_int max_buf_size) then
	Error
      else
	match Padsc.sfsetbuf !out_ref (Some "") 0 with (* get the buffer. *)
	    None -> Error 
	  | Some buf ->
	      match Padsc.sfclose !out_ref with
		  0 -> Ok buf
		| -1 -> Error 
		| _ -> Error (* shouldn't happen. *)

  (* TODO: Does this leave the file descriptor open?  *)
  let close_out (ph, out_ref) = Padsc.sfclose !out_ref 	    

  let checkpoint speculative pads = 
    let spec = if speculative then 1 else 0 in
      ignore (Padsc.p_io_checkpoint (get_padsc_handle pads)  spec)
  let commit pads = ignore (Padsc.p_io_commit (get_padsc_handle pads)) 
  let restore pads = ignore (Padsc.p_io_restore (get_padsc_handle pads)) 
  let is_speculative pads = (Padsc.p_is_current_spec (get_padsc_handle pads)) > 0
  let get_spec_level pads = Padsc.p_spec_level (get_padsc_handle pads)
end

(** 
  Raised when an error is encounterd during a speculative
  parse. Speculative parses are used by datatypes to try out the
  different variants.
*)
exception Speculation_failure

let find_eor hdr pads = 
  let b_pos = get_current_pos pads in
  let res, v = Padsc.p_io_next_rec (get_padsc_handle pads) in
    match result_of_perror_t v res with
	Ok 0 -> 
	  if not (ps_is_panic hdr.state) then hdr
	  else {hdr with state = ps_unset_panic hdr.state}
      | Ok bytes_skipped ->
	  let err_span = close_span b_pos pads in
	    if ps_is_panic hdr.state then
	      (Log.report_info "Pads.find_eor" err_span 
		 (Padsc_error_code Padsc.P_NO_ERR)
		 (Some "Resynching at EOR") pads;
	       {hdr with state = ps_unset_panic hdr.state})
	    else if IO.is_speculative pads then
	      raise Speculation_failure
	    else
	      begin
		Log.report_warning "Pads.find_eor" err_span 
		  (Padsc_error_code Padsc.P_EXTRA_BEFORE_EOR)
		  None pads;
		let new_state = ps_unset_panic hdr.state in
		let new_nerr = hdr.nerr + 1 in
		let new_ec, new_einfo = 
		  if new_nerr = 1 then
		    Nest err_span, Unexpected_data_before_eor 
		  else hdr.error_code, hdr.error_info
		in
		{state=new_state; nerr=new_nerr; 
		 error_code=new_ec; error_info=new_einfo; 
		 span=hdr.span}
	      end	    
      | Error -> 
	  if IO.is_speculative pads then
	    raise Speculation_failure
	  else	      
	    begin
	      let err_span = close_span b_pos pads in
	      Log.report_warning "Pads.find_eor" err_span 
		(Padsc_error_code Padsc.P_EOF_BEFORE_EOR)
		None pads;
	      let new_state = ps_unset_panic hdr.state in
	      let new_nerr = hdr.nerr + 1 in
	      let new_ec, new_einfo = 
		if new_nerr = 1 then
		  Nest err_span, Eof_before_eor 
		else hdr.error_code, hdr.error_info
	      in
	      {state=new_state; nerr=new_nerr; 
	       error_code=new_ec; error_info=new_einfo; 
	       span=hdr.span}
	    end

let print_open_rec pads = 
  ignore (
    Padsc.pDCI_io_rec_open_write2io 
      (get_padsc_handle pads)
      (get_out_stream pads)
      "Pads.print_open_rec"
  )
    
let print_close_rec pads = 
  ignore (
    Padsc.pDCI_io_rec_close_write2io 
      (get_padsc_handle pads)
      (get_out_stream pads)
      "Pads.print_close_rec"
  )

let print_EOL_lit pads = 
    print_open_rec pads;
    print_close_rec pads

module Record = struct

  let create_pd_hdr pads = 
    (* finish_pd_hdr will adjust the span to be non-empty. *)
    make_valid_pd_hdr (make_empty_span pads)

  let finish_pd_hdr hdr pads =
    try
      {hdr with span = finish_span hdr.span pads}
    with
	(* should never happen. finish_pd_hdr should always be
	   paired with create_pd_hdr to ensure that span exists. *)
	Match_failure _ -> 
	  Log.report_fatal "Pads.Record.finish_pd_hdr" None (Padsc_error_code Padsc.P_SYS_ERR) 
	    (Some "Misuse of finish_pd_hdr: not paired with create_pd_hdr") pads (* Should never happen. *)
      

  (* Update an existing header with the pd from a subcomponent. *)
  let update_pd_hdr h sub =
    if sub.nerr = 0 then h
    else
      let new_state = if ps_is_panic sub.state then 
	ps_set_panic h.state else h.state in
      let new_nerr = h.nerr + 1 in
      let new_ec, new_einfo = 
	if new_nerr = 1 then 
	  match sub.error_code with
	      Nest sp -> Nest sp, No_info
	    | Syn | Sem -> Nest sub.span, No_info
	    | Good | Maybe -> 
		(* These codes shouldn't be listed if nerr > 0 *)
		raise (Runtime_error ("Pads.Record.update_pd_hdr", None,
				     "Invariant violated: nerr > 0, but error code is Good or Maybe"))
	else h.error_code, h.error_info
      in
        {state=new_state; nerr=new_nerr; error_code=new_ec;
	 error_info=new_einfo; span=h.span}

  (* Process result of parsing subcomponent. 
     h : record header.
     sub : subcomponent header.
  *)
  let process_result h sub pads = 
    if sub.nerr > 0 && IO.is_speculative pads then
      raise Speculation_failure
    else
      update_pd_hdr h sub

  (* Process result of scanning for literal. *)
  let process_scan_result h res type_name string_of_lit lit span pads = 
    match res with
	Ok 0 -> update_pd_hdr h (make_valid_pd_hdr span)
      | Ok n -> 
	  if IO.is_speculative pads then
	    raise Speculation_failure
	  else
	    begin
	      Log.report_warning "Pads.process_scan_result" span 
		(Padsc_error_code Padsc.P_STRUCT_EXTRA_BEFORE_SEP)
		(Some ("Extra data before " ^ type_name ^ " " ^ string_of_lit lit)) pads;
	      update_pd_hdr h (make_lit_error_pd_hdr span)
	    end
      | Error -> 
	  if IO.is_speculative pads then
	    raise Speculation_failure
	  else
	    begin
	      Log.report_warning "Pads.process_scan_result" span 
		(Padsc_error_code Padsc.P_MISSING_LITERAL)
		(Some ("Missing " ^ type_name ^ " " ^ string_of_lit lit)) pads;
	      update_pd_hdr h (make_lit_error_pd_hdr span)
	    end
(*"Extra data before string separator %s"*)

  let parse_next parse_fn hdr pads =
    let rep,pd = parse_fn pads in
    let h = get_pd_hdr pd in
      rep,pd,process_result hdr h pads

  let parse_first parse_fn pads =
    let initial_hdr = create_pd_hdr pads in
      parse_next parse_fn initial_hdr pads

  let absorb_next parse_fn hdr pads =
    let _,pd = parse_fn pads in 
    let h = (get_pd_hdr pd) in
      process_result hdr h pads

  let absorb_first parse_fn pads =
    let initial_hdr = create_pd_hdr pads in
      absorb_next parse_fn initial_hdr pads

  let absorb_next_literal scan_lit type_name string_of_lit ~lit hdr pads = 
    let begin_pos = get_current_pos pads in
    let res = scan_lit pads lit in
    let end_pos = get_prev_pos pads in
    let lit_span = make_incl_span begin_pos end_pos in
      process_scan_result hdr res type_name string_of_lit lit lit_span pads
	  
  let absorb_first_literal scan_lit type_name string_of_lit ~lit pads = 
    let initial_hdr = create_pd_hdr pads in
      absorb_next_literal scan_lit type_name string_of_lit ~lit:lit initial_hdr pads
	  
  let string_of_char c = String.make 1 c
  let string_of_str s = s
  let string_of_re re = ""

  let absorb_first_char   = absorb_first_literal p_char_lit_scan1 "character literal" string_of_char
	  
  let absorb_next_char    = absorb_next_literal p_char_lit_scan1 "character literal" string_of_char
	    
  let absorb_first_string = absorb_first_literal p_str_lit_scan1 "string literal" string_of_str

  let absorb_next_string  = absorb_next_literal p_str_lit_scan1 "string literal" string_of_str
	    
  let absorb_first_int    = absorb_first_literal p_int_lit_scan1 "integer literal" string_of_int

  let absorb_next_int     = absorb_next_literal p_int_lit_scan1 "integer literal" string_of_int

  let absorb_first_regexp = absorb_first_literal p_regexp_scan1 "regular expression" string_of_re

  let absorb_next_regexp  = absorb_next_literal p_regexp_scan1 "regular expression" string_of_re

  let absorb_first_regexp_str = absorb_first_literal p_regexp_str_scan1 "regular expression" string_of_str

  let absorb_next_regexp_str  = absorb_next_literal p_regexp_str_scan1 "regular expression" string_of_str

  let gen_pd hdr sub_pd = 
    let new_pd_hdr = update_pd_hdr hdr (get_pd_hdr sub_pd) in
      sub_pd,new_pd_hdr

end

module Compute = struct

  (* Make a parsing function from a computed value and gen_pd function. 
     pads argument is ignored. It is there so that function has an ('a,'b) parser__ type.
  *)
  (* FIX: Why do we need this wrapper? Why does a compute function need a parser type? *)
  let generate_parser gen_val genpd_fn pads = gen_val, genpd_fn gen_val 

end

module Where = struct

  let gen_pd elt_pd b = 
    let elt_hdr = get_pd_hdr elt_pd in
      if b && elt_hdr.nerr = 0 then 
	({state      = ps_init;
	  nerr       = 0; 
	  error_code = Good; 
	  error_info = No_info;
	  span       = elt_hdr.span},
	 elt_pd) 
      else (* There is some error. *)
	if elt_hdr.nerr = 0 then (* so b = false *)
	  ({state      = elt_hdr.state;
	    nerr       = 1; 
	    error_code = Sem; 
	    error_info = No_info;
	    span       = elt_hdr.span},
	   elt_pd)
	else 
	  ({state      = elt_hdr.state;
	    nerr       = 1; 
	    error_code =
	       (match elt_hdr.error_code with
		   Nest sp -> Nest sp
		 | Syn | Sem -> Nest elt_hdr.span
		 | Good | Maybe -> 
		     (* These codes shouldn't be listed if nerr > 0 *)
		     raise (Runtime_error ("Pads.Where.gen_pd", None,
					  "Invariant violated: nerr > 0, but error code is Good or Maybe"))); 
	    error_info = No_info;
	    span       = elt_hdr.span},
	   elt_pd)

  let make_pd elt_pd b pads = 
    let elt_hdr = get_pd_hdr elt_pd in
      if b && elt_hdr.nerr = 0 then 
	({state      = ps_init;
	  nerr       = 0; 
	  error_code = Good; 
	  error_info = No_info;
	  span       = elt_hdr.span},
	 elt_pd) 
      else (* There is some error. *)
	if IO.is_speculative pads then
	  raise Speculation_failure
	else 
	  if elt_hdr.nerr = 0 then (* so b = false *)
	    ({state      = elt_hdr.state;
	      nerr       = 1; 
	      error_code = Sem; 
	      error_info = No_info;
	      span       = elt_hdr.span},
	     elt_pd)
	  else 
	    ({state      = elt_hdr.state;
	      nerr       = 1; 
	      error_code =
		 (match elt_hdr.error_code with
		      Nest sp -> Nest sp
		    | Syn | Sem -> Nest elt_hdr.span
		    | Good | Maybe -> 
			(* These codes shouldn't be listed if nerr > 0 *)
			Log.report_fatal "Pads.Where.make_pd" None (Padsc_error_code Padsc.P_SYS_ERR) 
			  (Some "Invariant violated: nerr > 0, but error code is Good or Maybe") pads); 
	      error_info = No_info;
	      span       = elt_hdr.span},
	     elt_pd)

  let parse_underlying parse_fn pred pads = 
    let (r, pd) = parse_fn pads in
    let c = pred r in 
      (r, make_pd pd c pads)

end

module Datatype = struct
  let parse_variant parse_fn pads =
    IO.checkpoint true pads;
    let spec_level = IO.get_spec_level pads in
      try 
	let r,p = parse_fn pads in
	if spec_level = (IO.get_spec_level pads) then (* no nested commit. *)
	  if pd_is_ok p then
	    (IO.commit pads; Some(r,p))
	  else
	    (IO.restore pads; None) (* an error occurred without raising 
				       an exception (e.g. in ptry). *)
	else (* a nested element committed. *)
	  Some (r,p)
      with Speculation_failure when spec_level = (IO.get_spec_level pads) ->
	(* An error occured while speculating for _this_ variant. *)
	(IO.restore(pads); None)
	  (* If "when" condition is not met, then the exception was raised
	     because we are still speculating for some upper-level
	     construct. *)

  let absorb_variant parse_fn pads =
    IO.checkpoint true pads;
    let spec_level = IO.get_spec_level pads in
      try 
	let r,((h,_) as p) = parse_fn pads in
	  if spec_level = (IO.get_spec_level pads) then (* no nested commit. *)
	    if pd_is_ok p then
	      (IO.commit pads; Some h.span)
	    else
	      (IO.restore pads; None) (* an error occurred without raising 
					 an exception (e.g. in ptry). *)
	  else (* a nested element committed. *)
	    Some h.span
      with Speculation_failure when spec_level = (IO.get_spec_level pads) ->
	(* An error occured while speculating for _this_ variant. *)
	(IO.restore(pads); None)

  let absorb_literal_variant scan_lit ~lit pads =
    IO.checkpoint true pads;
    let begin_pos = get_current_pos pads in
    let res = scan_lit pads lit in
    let end_pos = get_prev_pos pads in
    let lit_span = make_incl_span begin_pos end_pos in      
      match res with
	  Ok 0 -> (IO.commit(pads); Some lit_span)
	| _ -> (IO.restore(pads); None)
	
  let absorb_char_variant = absorb_literal_variant p_char_lit_scan1
  let absorb_string_variant = absorb_literal_variant p_str_lit_scan1
  let absorb_int_variant = absorb_literal_variant p_int_lit_scan1
  let absorb_regexp_variant = absorb_literal_variant p_regexp_scan1
  let absorb_regexp_str_variant = absorb_literal_variant p_regexp_str_scan1
  let absorb_EOL_variant = absorb_literal_variant p_EOL_lit_scan

  let make_pd_hdr sub_hdr = make_valid_pd_hdr sub_hdr.span
      
  let make_rep r = r 

  let make_pd (sub_hdr, pd_body) = (make_pd_hdr sub_hdr, pd_body) 

  let make_err_pd pads err_pd_body =
    ({state      = ps_set_panic ps_init;
      nerr       = 1; 
      error_code = Syn; 
      error_info = No_info;
      span       = make_empty_span pads
     },
     err_pd_body)    

  let handle_error_variant pads err_pd_body = 
    if IO.is_speculative pads then
      raise Speculation_failure
    else make_err_pd pads err_pd_body

(*   let make_gen_pd pads gen_pd_body =  *)
(*     ({state= ps_init; *)
(*       nerr= 0;  *)
(*       error_code=Good;  *)
(*       error_info=No_info; *)
(*       span=make_empty_span pads}, *)
(*      gen_pd_body)     *)

  let make_absorb_pd sp body = (make_valid_pd_hdr sp, body) 
 
  let process_case_result h = 
    if h.nerr = 0 then h
    else {h with nerr = 1}

  let parse_case parse_fn rep_con pd_con pads = 
    let rep,pd = parse_fn pads in
    let hdr = process_case_result (get_pd_hdr pd) in
      rep_con rep,(hdr, pd_con pd)

  let absorb_case parse_fn rep_con pd_con pads = 
    let rep,pd = parse_fn pads in
    let hdr = process_case_result (get_pd_hdr pd) in
      rep_con,(hdr, pd_con)

  let absorb_literal_case scan_lit lit rep_con pd_con pads =
      let begin_pos = get_current_pos pads in
      let res = scan_lit pads lit in
      let end_pos = get_prev_pos pads in
      let lit_span = make_incl_span begin_pos end_pos in      
      let hdr = 
	match res with
	    Ok 0 -> make_valid_pd_hdr lit_span
	  | _ -> make_lit_error_pd_hdr lit_span
      in
	rep_con,(hdr,pd_con)	

  let absorb_char_case c = absorb_literal_case p_char_lit_scan1 c
  let absorb_string_case s = absorb_literal_case p_str_lit_scan1 s
  let absorb_int_case i = absorb_literal_case p_int_lit_scan1 i
  let absorb_regexp_case r = absorb_literal_case p_regexp_scan1 r
  let absorb_regexp_str_case rs = absorb_literal_case p_regexp_str_scan1 rs
  let absorb_EOL_case () = absorb_literal_case p_EOL_lit_scan ()

  let gen_case gen_val genpd_fn = 
    parse_case (Compute.generate_parser gen_val genpd_fn)

  let gen_pd pd pd_con = 
    let hdr = process_case_result (get_pd_hdr pd) in
      (hdr, pd_con pd)

  let gen_pd_empty pd_con = 
    let hdr = make_valid_pd_hdr None in
      (hdr, pd_con)

end


