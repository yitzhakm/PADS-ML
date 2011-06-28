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
type pos = (int*int) * (int*int)   (* beginning and end position in source code *)
type id = PadscId.id                    (* possibly implemented as a string * int *)

let make_pos start finish = (start,finish)

let string_of_pos ((l1,c1),(l2,c2)) = 
(string_of_int l1) ^ "."   ^ 
(string_of_int c1) ^ " - " ^ 
(string_of_int l2) ^ "."   ^ 
(string_of_int c2)

let start_pos = make_pos (0,0) (0,0)
    
type exp = HostLanguage.expr
    
type tp = TidTp of id 
	  | TupleTp of comp_tp list
	  | RecordTp of field list
	  | TpAppTp of tp list * id  (* Application to a type. *)
	  | ValAppTp of tp * exp     (* Applicaiton to a value. *)
	  | WhereTp of id * tp * exp 
	  | ArrayTp of tp * array_comp list
	  | TableTp of id * exp * exp * id * id
	  | SingletonTp of exp
	  | HostTp of HostLanguage.tp

and  field = AbsorbField of comp_tp 
             | FullField of (id * tp)
	     | GenField of (id * tp * exp)
	     | LetField of (id * exp)
		 
and array_comp = ArrSep of exp | ArrTerm of term_comp | ArrPred of exp
and term_comp = ArrNosep | ArrTermExp of exp
and table_comp = TableSep of exp | TableTerm of exp | TableKeyPath of id * id
  
and comp_tp = Type of tp | Exp of exp
  
type val_param = id * HostLanguage.tp
    
type case_pat = HostLanguage.patt		      
		      
type dt_case = 
    AbsorbCase of case_pat * id * comp_tp
  | FullCase of case_pat * id * tp
  | GenCase of case_pat * id * tp * exp

type dt_variant = 
    AbsorbVar of id * comp_tp
  | FullVar of id * tp

type dt_default_variant = 
    FullDefault of id option * tp
  | GenDefault of id option * tp * exp

type dt_body = ImplicitDT of dt_variant list * dt_default_variant option
	       | CaseDT of exp * dt_case list
		   
type tp_def = TpDef of tp
              | DtDef of dt_body

type tp_equation = id option * id  (* optional module id, type id *)

type decl = id list * id * val_param option 
    * tp_equation option  (* alias to an existing type. *)
    * tp_def   (* external declarations don't have a definition. *)

type pml_item = Ptype of MLast.loc * decl list
				 | Pextern of MLast.loc * (PadscId.id list * id * val_param option)
				 | Ocaml of HostLanguage.item

type pml_ast = pml_item list

