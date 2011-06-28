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
open Pads
module Source = Type.Convert_type (Intlist.Source)

exception Failure
let handle_res = function
    Pads.Ok p -> p
  | Pads.Error -> raise Failure

let (rep,pd) =
  if Array.length Sys.argv > 1 then 
    PadsEasy.parse_source Source.parse Sys.argv.(1) false
  else
    PadsEasy.parse_with_norec Source.parse

let rec get_sum sum len = function
  [] -> sum,len
| i::is -> get_sum (sum + i) (len + 1) is
  
let (sum,len) = match get_pd_hdr pd with
            {error_code = Good} -> get_sum 0 0 rep
          | _ -> 0,1

let avg = sum/len

let print_to_stdout x x_pd =
  let pads = handle_res (Pads.open_handle_norec ()) in
    (*   let _ = handle_res (Pads.IO.open_out_file pads file_name) in      *)
  let _ = Source.print x x_pd pads in
  handle_res (Pads.close_handle pads)

let _ = print_to_stdout rep pd

let _ = print_endline ("List average is: " ^ string_of_int avg)

