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

ptype field_string =  pstring_ME("/[^|]+/")

ptype arg_info = {
    pattern_id : pint;
   '|'; seq_num    : pint;
   '|'; pattern_arg: pint;
   '|'; xml_name   : pstring('|');
   '|'; container  : pstring('|');
   '|'; fixed_value: field_string popt;
   '|'; post_pop   : field_string popt;
   '|'; delimiter  : field_string popt;
   '|'; inc_empty  : pchar
  }

ptype arg_info_record = arg_info precord

ptype columnHeading  = pstring_SE("/\\||$/") 
ptype columnHeadings = columnHeading plist_re("/\\|/",peor)

ptype map = arg_info_record plist_np
ptype source = columnHeadings precord * map

