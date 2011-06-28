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

ptype arg_token = 
     String_arg of '"' * pstring('"') * '"' 
   | IP_arg of pIP
   | Any_arg of pstring_SE("/[;\\s]/")

(* let ws_sep = Regexp_sep "/[[:space:]]+/" *)
(* let arg_sep = ws_sep *)
let space_sep = Char_sep ' '
let arg_sep = space_sep
let re_EOL =  "/\\s*\\n/"
let sep_EOL = Regexp_sep re_EOL

ptype comment = " ## " * pstring('\n')

ptype command = { text: pstring_SE("/;|\\n/"); ';'; comment: comment poption }

ptype (section) element = 
    Command of command
  | Section of section

ptype section = {
    name : pstring(' '); ' ';
    arguments :  (pstring(' ') * ' ') plist(No_sep, Char_term '{');
    '{'; pcommit;  pre re_EOL;
    (* We use the suffix "__members" so that the special XML formatter
       will strip the tags for this field.  *)
    section__anon : section element plist(sep_EOL, String_term "\n}");
    "\n}"
  }

ptype juniper_rec = section element * pre re_EOL
  
ptype source = section element plist(sep_EOL, No_term)
