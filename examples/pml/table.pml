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

ptype name_t = {
  first: pstring (' ');
  " ";
  last: pstring ('|')
}
ptype item = {
  name:      name_t;
  "|"; dept:    pstring('|'); 
  "|"; salary: pint
}

(* tables generate modules with functions for manipulating a hash table of 
   their items.  Parsing semantics is identical to plist.

   meaning of key field => generate a function called "name" that when given 
   a domain name (ie the type of item.name) will lookup the item in the 
   table.  there is a semantic error if the table has more than 1 element
   with the same key.  there may be multiple keys. (each key needs to have a
   different name).

   meaning of index field => generate a function called "site" that when
   given a string will return a list of the table items.  (indexes don't
   need to have the uniqueness property of keys).  there may be multiple
   indices.  (each index needs to have a different name).
*)

ptype source = 
  table item of
    sep = Char_sep '\n';    
    term = No_term;
    key = name : name_t;   
  end                          
