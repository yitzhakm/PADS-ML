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
module MyTerm = Plib.MyTerm
module MyTermOpt = Plib.Popt(Plib.MyTerm)
module MyTermNot = Plib.Pnot(Plib.MyTerm)
module MyTermPeek = Plib.Ppeek(Plib.MyTerm)
module MyIntList = Plib.MyIntList

module Test = Plib.MyIntList


let string_of_nerr (h,_) = string_of_int h.Pads.nerr

let rep,pd = PadsEasy.parse_with Test.parse	    
let _ = print_endline (string_of_nerr pd)

(* let _ = match rep with *)
(*   MyTermOpt.Something _ -> print_endline "Something" *)
(* | MyTermOpt.Nothing _ ->  print_endline "Nothing" *)
(* | _ ->  print_endline "Error" *)

(* let _ = match rep with *)
(*   MyTermPeek.Notfound _ -> print_endline "Notfound" *)
(* | MyTermPeek.Found _ ->  print_endline "Found" *)
(* | _ ->  print_endline "Error" *)

(* let (_,(sum_pd,orders_pd)) = pd  *)
(* let _ = print_endline (string_of_nerr sum_pd) *)
(* let _ = print_endline (string_of_nerr orders_pd) *)

(* let (_,{Sirius.Order_header.order_num_pd=order_num_pd; *)
(*      Sirius.Order_header.att_order_num_pd=att_order_num_pd; *)
(*      Sirius.Order_header.ord_version_pd=ord_version_pd; *)
(*      Sirius.Order_header.service_tn_pd=service_tn_pd; *)
(*      Sirius.Order_header.billing_tn_pd=billing_tn_pd; *)
(*      Sirius.Order_header.nlp_service_tn_pd=nlp_service_tn_pd; *)
(*      Sirius.Order_header.zip_code_pd=zip_code_pd; *)
(*      Sirius.Order_header.ramp_pd=ramp_pd; *)
(*      Sirius.Order_header.order_sort_pd=order_sort_pd; *)
(*      Sirius.Order_header.order_details_pd=order_details_pd; *)
(*      Sirius.Order_header.unused_pd=unused_pd; *)
(*      Sirius.Order_header.stream_pd=stream_pd *)
(* }) = orders_pd *)

(* let _ = print_endline (string_of_nerr order_num_pd) *)

(* let _ = print_endline (string_of_nerr att_order_num_pd) *)

(* let _ = print_endline (string_of_nerr ord_version_pd) *)

(* let _ = print_endline (string_of_nerr service_tn_pd) *)
(* let _ = match order_hdr.Sirius.Order_header.service_tn with *)
(*     Sirius.Pn_t_opt.Something i -> print_endline ("service_tn: " ^ (string_of_int i)) *)
(*   | Sirius.Pn_t_opt.Nothing () -> print_endline "service_tn: nothing" *)

(* let _ = print_endline (string_of_nerr billing_tn_pd) *)

(* let _ = print_endline (string_of_nerr nlp_service_tn_pd) *)

(* let _ = print_endline (string_of_nerr zip_code_pd) *)

(* let _ = print_endline (string_of_nerr ramp_pd) *)

(* let _ = print_endline (string_of_nerr order_sort_pd) *)

(* let _ = print_endline (string_of_nerr order_details_pd) *)

(* let _ = print_endline (string_of_nerr unused_pd) *)

(* let _ = print_endline (string_of_nerr stream_pd) *)


