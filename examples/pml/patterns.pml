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
*                 David Walker <dpw@cs.princeton.edu>                  *
*              Kathleen Fisher <kfisher@research.att.com>              *
*                  Kenny Zhu <kzhu@cs.princeton.edu>                   *
*                                                                      *
***********************************************************************)
open Built_ins

ptype field_string = pstring_ME("/[^|]+/")

ptype row = {
    pattern_id: pint32;
    '|'; parser_id : pint32;
    '|'; seq_num : pint32;
    '|'; pattern: pstring_SE("/\\|\\w*\\|\\w*\\|\\w*\\|\\w*\\|\\w*\\|\\w*\\|\\w*\\|[tf]?\\|[tf]?\\|[tf]?$/");
    '|'; parent: pstring('|');
    '|'; parent_op: pstring('|');
    '|'; data_type: pstring('|');
    '|'; container: field_string popt;
    '|'; post_pop: field_string popt;
    '|'; last: pstring_ME("/\\w*\\|\\w*\\|[tf]?\\|[tf]?\\|[tf]?$/")
  }

ptype rowRecord = row precord

ptype columnHeading  = pstring_SE("/\\||$/") 
ptype columnHeadings = columnHeading plist_re("/\\|/",peor)

ptype source = (columnHeadings precord) * (rowRecord plist_np)

ptype newRow = {
    n_pattern_id: pint32;
    '|'; n_parser_id : pint32;
    '|'; n_seq_num : pint32;
    '|'; n_parent: pstring('|');
    '|'; n_parent_op: pstring('|');
    '|'; n_data_type: pstring('|');
    '|'; n_container: field_string popt;
    '|'; n_post_pop: field_string popt;
    '|'; n_last: pstring_ME("/\\w*\\|\\w*\\|[tf]?\\|[tf]?\\|[tf]?/");
    '|'; n_pattern: pstring_SE(peor)
  }

ptype newRowRecord = newRow precord

ptype newSource = newRowRecord plist_np

ptype patternRow = pstring_SE(peor)
ptype patternSource = (patternRow precord) plist_np
