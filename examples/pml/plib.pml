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
ptype Pcharlit(c:char) = [cc:Pchar | cc = c]

pdatatype (Alpha) Popt =
  Something of Alpha 
  with pdefault Nothing of Punit

let fst(x,y) = x
let snd(x,y) = y

ptype (Alpha) Pnot = 
  [x_opt : Alpha Popt |
   let module M = Popt(Alpha) in	
   match x_opt with 
     M.Something(_) -> false 
   | _ -> true]

ptype Pfalse = [u:Punit|false]

pdatatype (Alpha,Beta) Por =
  IsAlpha of Alpha
| IsBeta of Beta 
with pdefault of Pfalse

(* Twisted, eh? I don't use it, just threw it in for fun.*)
ptype (Alpha) Pnot_not = (Alpha Pnot) Pnot

(* Lookahead type. Guaranteed not to consume any data from the stream. 
   Never returns error.*)
pdatatype (Alpha) Ppeek = 
    Notfound of Alpha Pnot
  | Found of Punit

(* Lookahead type. Guaranteed not to consume any data from the stream. 
   Succeeds when char is found, returns error otherwise.*)
ptype (Alpha) Pmatch_tp =
  [peek : Alpha Ppeek |
   let module M = Ppeek(Alpha) in	
     match peek with
        M.Notfound _ -> false
     |   _ -> true]

pdatatype (Alpha,SepTp,TermTp) Plist_tail =
 Nil of omit TermTp Pmatch_tp
with pdefault Cons of SepTp * Alpha * (Alpha,SepTp,TermTp) Plist_tail

pdatatype (Alpha,SepTp,TermTp) Plist =
 Nil of omit TermTp Pmatch_tp
with pdefault Cons of Alpha * (Alpha,SepTp,TermTp) Plist_tail

(* Example: *)

ptype MySep = Pcharlit('|')
ptype MyTerm = Pcharlit('$')
ptype MyIntList = (Pint,MySep,MyTerm) Plist
ptype TestMatch = MyTerm Pmatch_tp * MyTerm

(*
pdatatype (Alpha) Plist (sep_term: char * char) =
  More of Alpha * Pcharlit(fst sep_term) * Plist (sep_term)
| One  of Alpha
| Done of omit Pcharlit(snd sep_term)
*)
