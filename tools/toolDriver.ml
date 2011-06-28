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
module Debug_test (Ty:Type.S) =
struct
  module Source = Type.Convert_type (Ty)
  module TDebug = Source.Traverse(Debug_tool)
    
  let in_stream =     
    if Array.length Sys.argv > 1 
    then Sys.argv.(1) 
    else "/dev/stdin"

  let (r,pd) = PadsEasy.parse_source_debug Source.parse in_stream true
  let _ = print_endline "Debug output:"
  let sDebug = TDebug.init ()
  let _ = TDebug.traverse r pd sDebug
end

module XML_test (Ty:Type.S) =
struct
  module Source = Type.Convert_type (Ty)
  module T = Source.Traverse(Xml_formatter)
    
  let in_stream =     
    if Array.length Sys.argv > 1 
    then Sys.argv.(1) 
    else "/dev/stdin"

  let (r,pd) = PadsEasy.parse_source Source.parse in_stream true
  let s = T.init ()
  let xml = T.traverse r pd s
  let print_xml x =
    print_endline (Xml.to_string_fmt x)
  ;;
    List.iter print_xml xml
end

module XML_test_norec (Ty:Type.S) =
struct
  module Source = Type.Convert_type (Ty)
  module T = Source.Traverse(Xml_formatter)
    
  let in_stream =     
    if Array.length Sys.argv > 1 
    then Sys.argv.(1) 
    else "/dev/stdin"

  let (r,pd) = PadsEasy.parse_source Source.parse in_stream false
  let s = T.init ()
  let xml = T.traverse r pd s
  let print_xml () x =
    print_endline (Xml.to_string_fmt x)
  ;;
    List.fold_left print_xml () xml
end

module Debug_test_norec (Ty:Type.S) =
struct
  module Source = Type.Convert_type (Ty)
  module TDebug = Source.Traverse(Debug_tool)
    
  let in_stream =     
    if Array.length Sys.argv > 1 
    then Sys.argv.(1) 
    else "/dev/stdin"

  let (r,pd) = PadsEasy.parse_source Source.parse in_stream false
  let _ = print_endline "Debug output:"
  let sDebug = TDebug.init ()
  let _ = TDebug.traverse r pd sDebug
end

module Parse_test (D:Type.Description) =
struct
  let in_stream =     
    if Array.length Sys.argv > 1 
    then Sys.argv.(1) 
    else "/dev/stdin"

  let (r,pd) = PadsEasy.parse_source D.Source.parse in_stream D.__PML__has_records

  ;;
  if Pads.pd_is_ok pd then print_endline "Success" 
  else print_endline ("Failure:\n" ^ Pads.string_of_pd_hdr (Pads.get_pd_hdr pd))
end
