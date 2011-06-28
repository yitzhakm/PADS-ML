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
(** Parse the source and print it with the Debug tool. 
    The name of the source file is optionally specified on the command line as the first argument.
    In the absence of a first argument, reads from stdin.

    Assumes that the format uses a newline record discipline.

    Returns nothing. *)
module Debug_test (Ty:Type.S) : sig end

(** Parse the source and print it with the Debug tool. 
    The name of the source file is optionally specified on the command line as the first argument.
    In the absence of a first argument, reads from stdin.

    Assumes that the format uses no record discipline.

    Returns nothing. *)
module Debug_test_norec (Ty:Type.S) : sig end

(** Parse the source and convert it to XML.
    The name of the source file is optionally specified on the command line as the first argument.
    In the absence of a first argument, reads from stdin.

    Assumes that the format uses a newline record discipline.

    Returns nothing. *)
module XML_test (Ty:Type.S) : sig end

(** Parse the source and convert it to XML.
    The name of the source file is optionally specified on the command line as the first argument.
    In the absence of a first argument, reads from stdin.

    Assumes that the format uses no record discipline.

    Returns nothing. *)
module XML_test_norec (Ty:Type.S) : sig end

(** Attempt to parse the source and print result. *)
module Parse_test (D:Type.Description) : sig end
