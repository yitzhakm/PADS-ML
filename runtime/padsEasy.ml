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
exception Failure of string

let handle_res err_msg = function
    Pads.Ok p -> p
  | Pads.Error -> raise (Failure err_msg)

let parse_source parse_fn file_name has_rec = 
  let pads = handle_res "Failed to open pads handle." (
      if has_rec then Pads.open_handle () 
      else Pads.open_handle_norec ()
    ) 
  in    
  handle_res "Failed to open input file." (Pads.IO.open_in_file pads file_name);
  let x = parse_fn pads in
  handle_res "Failed to close input file." (Pads.IO.close_in_file pads);
  handle_res "Failed to close pads handle." (Pads.close_handle pads);
  x

let parse_source_debug parse_fn file_name has_rec = 
  let pads = handle_res "Failed to open pads handle." (
      if has_rec then Pads.open_handle () 
      else Pads.open_handle_norec ()
    ) 
  in
  Pads.Discipline.set_error_reporting pads Padsc.PerrorRep_Max;
  handle_res "Failed to open input file." (Pads.IO.open_in_file pads file_name);
  let x = parse_fn pads in
  handle_res "Failed to close input file." (Pads.IO.close_in_file pads);
  handle_res "Failed to close pads handle." (Pads.close_handle pads);
  x

let parse_fd parse_fn fd has_rec = 
  let pads = handle_res "Failed to open pads handle." (
      if has_rec then Pads.open_handle () 
      else Pads.open_handle_norec ()
    ) 
  in    
  handle_res "Failed to set input file descriptor." (Pads.IO.set_in_to_fd pads fd);
  let x = parse_fn pads in
  handle_res "Failed to clean up input state." (Pads.IO.close_in pads);
  handle_res "Failed to close pads handle." (Pads.close_handle pads);
  x

let parse_process cmd parse_in in_has_rec =
  let pads_in = handle_res "Failed to open pads handle for input." (
      if in_has_rec then Pads.open_handle_with_io_disc (Padsc.p_ctrec_noseek_make (int_of_char '\n') 0) 
      else Pads.open_handle_with_io_disc (Padsc.p_norec_noseek_make 0)
    ) 
  in    
  let process_sfio = handle_res ("Failed to launch command: \"" ^ cmd ^ "\"") 
    (Pads.IO.open_process_read pads_in cmd) 
  in
  let x = parse_in pads_in in
  handle_res "Failed to clean up input state." (Pads.IO.close_in pads_in);
  let exit_code = Padsc.sfclose process_sfio in (* will block until process ends. *)
  handle_res "Failed to close pads handle." (Pads.close_handle pads_in);
  x, exit_code

let parse_with_from_file parse_fn file_name = parse_source parse_fn file_name true

let parse_with parse_fn = parse_source parse_fn "/dev/stdin" true

let parse_with_norec parse_fn = parse_source parse_fn "/dev/stdin" false

module Parse (D:Type.Description) =
struct
  let in_stream =     
    if Array.length Sys.argv > 1 
    then Sys.argv.(1) 
    else "/dev/stdin"

  let print_pd_hdr h = print_endline (Pads.string_of_pd_hdr h)

  let get_error_span = function
      {Pads.error_code=Pads.Nest s} -> s
    | {Pads.span=s} -> s

  let (r,pd) = parse_source D.Source.parse in_stream D.__PML__has_records
  ;;
  if Pads.pd_is_ok pd then
    print_endline "Parse succeeded."
  else
    begin
      let h = fst pd in
      let s_msg = Pads.location_msg_of_span (get_error_span h) in
      print_endline ("Parse error at " ^ s_msg  ^ ".");
      print_pd_hdr h;
    end
  
end

(******************************************************************************************)
(* Printing Code                                                                          *)
(******************************************************************************************)
(** internal use only. *)
let print_clean_source_any fopen print_fun out_file has_rec x =
  let pads = handle_res "Failed to open pads handle." (
      if has_rec then Pads.open_handle () 
      else Pads.open_handle_norec ()
    ) 
  in    
  handle_res "Failed to open output file." (fopen pads out_file);
  print_fun x pads;
  (* close the pads handle. *)
  handle_res "Failed to close pads handle." (Pads.close_handle pads);
  (* close the output stream. *)
  handle_res "Failed to close output file." (Pads.IO.close_out_file pads)

(** internal use only. *)
let print_clean_source_any_to_string fopen print_fun has_rec x =
  let pads = handle_res "Failed to open pads handle." (
      if has_rec then Pads.open_handle () 
      else Pads.open_handle_norec ()
    ) 
  in    
  handle_res "Failed to open string for output." (fopen pads);
  print_fun x pads;
  (* close the pads handle. *)
  handle_res "Failed to close pads handle." (Pads.close_handle pads);
  (* close the output stream. *)
  handle_res "Failed to close output string." (Pads.IO.close_out_string None pads)

(** internal use only. *)
let print_clean_source_any_to_fd fd_assoc fd print_fun has_rec x =
  let pads = handle_res "Failed to open pads handle." (
      if has_rec then Pads.open_handle () 
      else Pads.open_handle_norec ()
    ) 
  in    
  handle_res "Failed to use file descriptor for output." (fd_assoc pads fd);
  print_fun x pads;
  (* close the pads handle. *)
  handle_res "Failed to close pads handle." (Pads.close_handle pads);
  (* close the output stream. *)
  handle_res "Failed to close output buffer." 
    (match Pads.IO.close_out pads with 0 -> Pads.Ok () | _ -> Pads.Error)


let print_clean_source print_fun out_file has_rec x = 
  print_clean_source_any Pads.IO.open_out_file print_fun out_file has_rec x 

let print_clean_source_to_string print_fun has_rec x = 
  print_clean_source_any_to_string Pads.IO.open_out_string print_fun has_rec x 

let print_clean_source_to_fd fd print_fun has_rec x = 
  print_clean_source_any_to_fd Pads.IO.set_out_to_fd fd print_fun has_rec x


let print_clean_source_mode mode print_fun out_file has_rec x =
  print_clean_source_any (Pads.IO.open_out_file_mode mode) print_fun out_file has_rec x

let print_source_mode mode print_fun out_file has_rec x x_pd =
  print_clean_source_mode mode (fun (x,x_pd) pads -> print_fun x x_pd pads) out_file has_rec (x,x_pd)


let print_source print_fun out_file has_rec x x_pd =
  print_clean_source (fun (x,x_pd) pads -> print_fun x x_pd pads) out_file has_rec (x,x_pd)

let print_source_to_string print_fun has_rec x x_pd =
  print_clean_source_to_string (fun (x,x_pd) pads -> print_fun x x_pd pads) has_rec (x,x_pd)

let print_source_to_fd fd print_fun has_rec x x_pd =
  print_clean_source_to_fd fd (fun (x,x_pd) pads -> print_fun x x_pd pads) has_rec (x,x_pd)

(******************************************************************************************)
(* End Printing Code                                                                      *)
(******************************************************************************************)

(******************************************************************************************)
(* Hybrid Functions                                                                       *)
(******************************************************************************************)
let print_process cmd print_out out_has_rec output_data =
  let pads_out = handle_res "Failed to open pads handle for output." (
      if out_has_rec then Pads.open_handle () 
      else Pads.open_handle_norec ()
    ) 
  in    

(*   Pads.Log.report_info "print_process" None Pads.No_info (Some "launching process") pads_out; *)
  let process_sfio = handle_res ("Failed to launch command: \"" ^ cmd ^ "\"") 
    (Pads.IO.open_process cmd Pads.IO.Write)
  in

(*   Pads.Log.report_info ~fun_name:"print_process" None Pads.No_info ~msg:(Some "printing out") pads_out; *)
  Pads.IO.set_out pads_out process_sfio;
  print_out output_data pads_out;

(*   Pads.Log.report_info ~fun_name:"print_process" None Pads.No_info ~msg:(Some "closing up") pads_out; *)
  let exit_code = Padsc.sfclose process_sfio in (* will block until process ends. *)
  handle_res "Failed to close output pads handle." (Pads.close_handle pads_out);

  exit_code

(** UNSAFE CAST *)
let fd_of_descr (descr : Unix.file_descr) : int = (Obj.magic descr)

let interact_with_process cmd
                          print_out out_has_rec output_data
                          parse_in in_has_rec =
  let pads_out = handle_res "Failed to open pads handle for output." (
      if out_has_rec then Pads.open_handle ()
      else Pads.open_handle_norec ()
    )
  in
  let pads_in = handle_res "Failed to open pads handle for input." (
      if in_has_rec then Pads.open_handle ()
      else Pads.open_handle_norec ()
    )
  in

(*   Pads.Log.report_info ~fun_name:"interact_with_process" None Pads.No_info ~msg:(Some "launching process") pads_out; *)
  let in_channel, out_channel = Unix.open_process cmd in
  let read_fd = fd_of_descr (Unix.descr_of_in_channel in_channel) in
  let write_fd = fd_of_descr (Unix.descr_of_out_channel out_channel) in
(*   let read_fd = 10 in *)
(*   let write_fd = 11 in *)

(*   Pads.Log.report_info ~fun_name:"interact_with_process" None Pads.No_info ~msg:(Some "printing out") pads_out; *)
  Pads.IO.set_out_to_fd pads_out write_fd;
  print_out output_data pads_out;

(*   Pads.Log.report_info ~fun_name:"interact_with_process" None Pads.No_info ~msg:(Some "closing pads output state") pads_out; *)
  Pads.IO.close_out pads_out;
(*   close_out out_channel; *)
  handle_res "Failed to close output pads handle." (Pads.close_handle pads_out);
 
(*   Pads.Log.report_info ~fun_name:"interact_with_process" None Pads.No_info ~msg:(Some "setting in"); *)
  handle_res "Failed to set input io stream." (Pads.IO.set_in_to_fd pads_in read_fd);
(*   Pads.Log.report_info ~fun_name:"interact_with_process" None Pads.No_info ~msg:(Some "parsing in"); *)
  let x = parse_in pads_in in

(*   Pads.Log.report_info ~fun_name:"interact_with_process" None Pads.No_info ~msg:(Some "closing pads input state") pads_out; *)
  handle_res "Failed to clean up input state." (Pads.IO.close_in pads_in);
  close_in in_channel;
  handle_res "Failed to close input pads handle." (Pads.close_handle pads_in);

  x, 0

(* *** Non-working, sfio version: *** *)
(* let interact_with_process cmd *)
(*                           print_out out_has_rec output_data *)
(*                           parse_in in_has_rec = *)
(*   let pads_out = handle_res "Failed to open pads handle for output." ( *)
(*       if out_has_rec then Pads.open_handle () *)
(*       else Pads.open_handle_norec () *)
(*     ) *)
(*   in *)
(*   let pads_in = handle_res "Failed to open pads handle for input." ( *)
(*       (\* In either case, specify a block size of 1, so pads won't block waiting for input. *)
(* 	 Hardly very efficient, but it works. *)
(* 	 TODO: find efficient alternative. *)
(*       *\) *)
(*       if in_has_rec then Pads.open_handle_with_io_disc (Padsc.p_ctrec_make (int_of_char '\n') 1) *)
(*       else Pads.open_handle_with_io_disc (Padsc.p_norec_make 1) *)
(*     ) *)
(*   in *)

(* (\*   print_endline "launching process"; *\) *)
(*   let process_sfio = handle_res ("Failed to launch command: \"" ^ cmd ^ "\"") *)
(*     (Pads.IO.open_process cmd Pads.IO.Read_write) *)
(*   in *)

(* (\*   print_endline "printing out"; *\) *)
(*   Pads.IO.set_out pads_out process_sfio; *)
(*   print_out output_data pads_out; *)
(*   (if Padsc.sfclose_writefd process_sfio < 0 then print_endline "sfclose_writefd: error"); *)
(* (\*   print_endline "closing pads output handle"; *\) *)
(*   handle_res "Failed to close output pads handle." (Pads.close_handle pads_out); *)
 
(* (\*   print_endline "setting in"; *\) *)
(*   handle_res "Failed to set input io stream." (Pads.IO.set_in pads_in process_sfio); *)
(*   print_endline "parsing in"; *)
(*   let x = parse_in pads_in in *)

(* (\*   print_endline "closing up"; *\) *)
(*   handle_res "Failed to clean up input state." (Pads.IO.close_in pads_in); *)
(*   let exit_code = Padsc.sfclose process_sfio in (\* will block until process ends. *\) *)
(*   handle_res "Failed to close input pads handle." (Pads.close_handle pads_in); *)

(* (\*   print_endline "closed up"; *\) *)
(*   x, exit_code *)
