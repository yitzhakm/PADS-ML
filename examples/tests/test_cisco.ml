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
module R = Type.Convert_type(Cisco.T_Config_members)
module TXml = R.Traverse(Cisco_xml_formatter)
(* module TXml = R.Traverse(Xml_formatter) *)
module TDbg = R.Traverse(Debug_tool)

exception Failure

let handle_res = function
    Pads.Ok p -> p 
  | Pads.Error -> raise Failure

let string_of_nerr (h,_) = string_of_int h.Pads.nerr

let parse_all_toXML pads = 
  let rec _parse prev_pos =
    if (Padsc.p_io_at_eof (Pads.get_padsc_handle pads) = 1) then 
      ()
    else 
      let r,pd = R.parse pads in
      let sXml = TXml.init () in
      let xml = TXml.traverse  r pd sXml in
      let print_xml () x =
	print_endline (Xml.to_string_fmt x)
      in
      let _ = List.fold_left print_xml () xml in
      let current_pos = Pads.get_current_pos pads in
	if Pads.eq_pos prev_pos current_pos then 
	  prerr_endline "No progress"
	else
	  _parse current_pos
  in
    _parse (Pads.get_current_pos pads)

let parse_all_debug pads = 
  let rec _parse prev_pos =    
    if Padsc.p_io_at_eof (Pads.get_padsc_handle pads) = 1 then 
      ()
    else 
      let r,pd = R.parse pads in
      let sDbg = TDbg.init () in
      let _ = TDbg.traverse  r pd sDbg in
      let current_pos = Pads.get_current_pos pads in
      if Pads.eq_pos prev_pos current_pos then 
	()
      else
	_parse current_pos
  in
    _parse (Pads.get_current_pos pads)
      
let parseSource pads = 
  begin
    print_endline "<Config>";
    parse_all_toXML pads;
(*     parse_all_debug pads; *)
    print_endline "</Config>"
  end

let () = 
  if Array.length Sys.argv > 1 then 
    PadsEasy.parse_source parseSource Sys.argv.(1) true
  else
    PadsEasy.parse_with parseSource
				
(* let sXml = TXml.init () *)
(* let xml = Xml_formatter.wrap (TXml.traverse  r pd sXml) "Config" *)
(* let _ = print_endline (Xml.to_string_fmt xml) *)

(* let sDbg = TDbg.init () *)
(* ;; *)
(* TDbg.traverse  r pd sDbg *)

(* let print_to_stdout x x_pd = *)
(*   let pads = handle_res (Pads.open_handle ()) in *)
(*     (\*   let _ = handle_res (Pads.IO.open_out_file pads file_name) in      *\) *)
(*   let _ = S.print x x_pd pads in *)
(*   handle_res (Pads.close_handle pads) *)

(* let _ = print_to_stdout r pd *)


