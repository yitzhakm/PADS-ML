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

(* ptype l_bracket = '[' *)
(* ptype r_bracket = ']' *)
(* ptype l_paren   = '(' *)
(* ptype r_paren   = ')' *)
(* ptype q_mark    = '?' *)
(* ptype lb_c      = "[^" *)
(* ptype comma     = ',' *)

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
  Begin_line of '^'
  | End_line   of '$'
  | Any       of '.'
  | Bang      of '!'
  | Escaped      of escaped_regexp
  | Literal_char of [c: pchar | not_special c]

(************** CHARACTER CLASS REGEXPS *************)

(* ptype char_class_alt = *)
(*     Range of pchar * '-' * pchar *)
(*   | Lit of pchar *)

(* ptype char_class = { *)
(*   rightBracket : r_bracket popt; *)
(*   alternatives : char_class_alt plist_st("","]") *)
(* } *)

(* ptype char_class_regexp = *)
(*     Inv_cc of "[^" * pcommit * char_class * ']' *)
(*   | Cc     of '[' * pcommit * char_class * ']' *)

ptype char_class_regexp = '[' * pcommit * pstring_ME("/^?\\]?.*?\\]/")

(************* QUALIFIED REGEXPS *************)

ptype numeric_qualifier_limit = '[' * pcommit * pint32

ptype numeric_qualifier = {
  '{'; min : pint32;
       lim : numeric_qualifier_limit popt;
  '}'; 
  not_greedy: '?' popt
}

ptype regexp_qualifier =
    Kleene   of "*?"
  | Plus     of "+?"
  | Opt      of "??"
  | G_kleene of '*'
  | G_plus   of '+'
  | G_opt    of '?'
  | N_qual  of numeric_qualifier

ptype simple_unqualified_regexp = 
      SChar_class of char_class_regexp
    | SSimple     of simple_regexp

ptype (nested_re) unqualified_regexp = 
      Grouped    of '(' * pcommit * nested_re * ')'
    | Char_class of char_class_regexp
    | Simple     of simple_regexp

ptype (unqualified_re) qualified_regexp = {
  base : unqualified_re;
  qual : regexp_qualifier popt
}

(************* CONCAT REGEXP *************)
 
(* "/ ??/" is a hack for no separator. *)
ptype (unqualified_re) concat_regexp =
  [l : unqualified_re qualified_regexp plist_re("/ ??/","/\\)|\\||$/") | not (l = [])]

(************* ALTERNATIVES REGEXP *************)

ptype (unqualified_re) perl_regexp =
  [l : unqualified_re concat_regexp plist_nosep("|") | not (l = [])]

(************* NESTED REGEXPS *************)

ptype nested2_regexp = simple_unqualified_regexp perl_regexp

ptype nested1_regexp = nested2_regexp unqualified_regexp perl_regexp

ptype nested0_regexp = nested1_regexp unqualified_regexp perl_regexp

(************* TOP-LEVEL TYPES *************)

ptype word_string = pstring_ME("/[[:word:]]+/")

(* beginning of line marker *)
ptype bol = [c:pchar | c = '^']

ptype record = {
    pattern_id: pint;
    '|'; parser_id : pint;
    '|'; seq_num : pint32;
    '|'; parent: pstring('|');
    '|'; parent_op: pstring('|');
    '|'; data_type: pstring('|');
    '|'; container: word_string popt;
    '|'; post_pop: word_string popt;
    '|'; last: pstring_ME("/\\w*\\|\\w*\\|[tf]?\\|[tf]?\\|[tf]?/");
    '|'; bol_anchor_opt: bol popt; pattern: nested0_regexp
}

ptype source = (record precord) plist_np
