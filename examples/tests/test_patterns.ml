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
open Patterns

exception Failure

let handle_res = function
    Pads.Ok p -> p 
  | Pads.Error -> raise Failure

let (labels,r),(labels_pd,pd) = 
  if Array.length Sys.argv > 1 then 
    PadsEasy.parse_source Source.parse Sys.argv.(1) true
  else
    PadsEasy.parse_with Source.parse

(* let sDbg = TDbg.init () *)
(* let _ = TDbg.traverse  r pd sDbg *)

let permute_row r = 
  { n_pattern_id = r.pattern_id; 
    n_parser_id = r.parser_id; 
    n_seq_num = r.seq_num; 
    n_parent = r.parent; 
    n_parent_op = r.parent_op;
    n_data_type = r.data_type; 
    n_container = r.container; 
    n_post_pop = r.post_pop; 
    n_last = r.last; 
    n_pattern = r.pattern;}

let filter_row r = r.pattern

let r'  = List.map permute_row r
let pd' = NewSource.gen_pd r'

let print_to_stdout print_fun x x_pd =
  let pads = handle_res (Pads.open_handle ()) in
    (*   let _ = handle_res (Pads.IO.open_out_file pads file_name) in      *)
  let _ = print_fun x x_pd pads in
    handle_res (Pads.close_handle pads)

let _ = print_to_stdout NewSource.print r' pd'

(* let r2  = List.map filter_row r *)
(* let pd2 = PatternSource.gen_pd r2 *)

(* let _ = print_to_stdout PatternSource.print r2 pd2 *)

