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

(* XXX: Hacks until I add to the compiler support for nested types. *)

ptype l_bracket = '['
ptype r_bracket = ']'
ptype l_paren   = '('
ptype r_paren   = ')'
ptype q_mark    = '?'
ptype lb_c      = "[^"
ptype comma     = ','

(*************  SINGLETON REGEXPS *************)

ptype escaped_regexp =
    Tab of "\\t"
  | Nl of "\\n"
  | Cr of "\\r"
  | Ff of "\\f"
      (*|   alarm of "\\a" *)
  | Word_char of "\\w"
  | Non_word of "\\W"
  | Ws of "\\s"
  | Non_ws of "\\S"
  | Digit of "\\d"
  | Non_digit of "\\D"
  | Other of '\\' * pchar
      
(* Only have to avoid these characters, as others will always be
   consumed by fields/types with higher precedence.*)
let not_special c =  not (c = '|' || c = ')' || c = ']')

ptype simple_regexp = 
    BeginLine of '^'
  | EndLine   of '$'
  | Any       of '.'
  | Escaped      of escaped_regexp
  | Literal_char of [c: pchar | not_special c]

(************** CHARACTER CLASS REGEXPS *************)

ptype char_class_alt =
    Range of pchar * '-' * pchar
  | Lit of pchar

ptype char_class = {
  rightBracket : r_bracket popt;
  alternatives : char_class_alt plist_st("","]")
}

ptype char_class_regexp =
    Inv_cc of lb_c pcommit * char_class * ']'
  | Cc     of l_bracket pcommit * char_class * ']'

(************* QUALIFIED REGEXPS *************)

ptype numeric_qualifier_limit = comma pcommit * pint32

ptype numeric_qualifier = {
  '{'; min : pint32;
       lim : numeric_qualifier_limit popt;
  '}'; 
  not_greedy: q_mark popt
}

ptype regexp_qualifier =
    Kleene   of "*?"
  | Plus     of "+?"
  | Opt      of "??"
  | G_kleene of '*'
  | G_plus   of '+'
  | G_opt    of '?'
  | N_qual  of numeric_qualifier

ptype perl_regexp_unqualified = 
      Grouped    of l_paren pcommit * perl_regexp * ')'
    | Char_class of char_class_regexp
    with pdefault Simple of simple_regexp

and qualified_regexp = {
  base : perl_regexp_unqualified;
  qual : regexp_qualifier popt
}

(************* CONCAT REGEXP *************)
 
(* "/ ??/" is a hack for no separator. *)
and concat_regexp =
  [l : qualified_regexp plist_re("/ ??/","/\\)|\\||$/") | not (l = [])]

(************* ALTERNATIVES REGEXP *************)

and perl_regexp =
  [l : concat_regexp plist_nosep("|") | not (l = [])]

(************* TOP-LEVEL REGEXPS *************)


ptype source = (perl_regexp precord) plist_np
