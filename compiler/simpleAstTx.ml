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
module C = Common
module D = Description
module N = Names

open SimpleAst


(********************)
(* Helper functions *)
(********************)

let gen_dt_var id ty = (id,Some ty)

let gen_dt_var_empty con = (con,None)


(********************)

let phase_name = "SimpleAstGen"

let gen_where_f sub_ty id exp = Constraint (sub_ty, id, exp)

let gen_record_abs_type f tp n = []

let gen_record_abs_lit exp n = []

let gen_record_full id f n = [f]

let gen_record_comp id ty exp n = [ty]

let gen_record_let id exp n = []

let gen_record_compose is_tuple names processed_fields = 
  if is_tuple then 
    Tuple processed_fields
  else
    Record (List.combine names processed_fields)
      
let gen_dt_full_var vt_ty id cases = (gen_dt_var id vt_ty) :: cases

let gen_dt_abs_type_var vt_ty tp id cases = (gen_dt_var_empty id) :: cases

let gen_dt_abs_lit_var e id cases = (gen_dt_var_empty id) :: cases

(* computed default *)
let gen_dt_comp_def ty e id = [gen_dt_var id ty]

(* full default *)
let gen_dt_full_def vt_ty id = [gen_dt_var id vt_ty]

(* no default *)
let gen_dt_no_def id = [gen_dt_var_empty id]

(* compose the generated code for each variant into a simple_type. *)
let gen_dt_compose cases = Datatype cases

let gen_dt_full_case pat case_ty id = gen_dt_var id case_ty

let gen_dt_abs_type_case pat case_ty tp id = gen_dt_var_empty id
    
let gen_dt_abs_lit_case pat e id = gen_dt_var_empty id

let gen_dt_comp_case pat ty e id = gen_dt_var id ty

let gen_dt_def_case id = gen_dt_var_empty id

let gen_dt_match_compose descr cases = Datatype cases

(*********************************)

(* Import the metadata type so as not to have to prefix each field
   name with "Description." *)
type metadata = Description.metadata = {
  name : PadscId.id; (** The (visible) name of the description. *)
  kind : D.kind;
  tyclass : D.tyclass;
}

(* Abbreviations *)
let rid = N.mk_rep_str
let pid = N.mk_pd_str

let rec gen_record_f dt tc loc fields is_tuple = 
  (** list of field names in record. *)
  let names = 
    C.munge_fields
      (fun ctp -> [])
      (fun (id,_) -> [id])
      (fun (id,_,_) -> [id])
      (fun _ -> [])
      fields
  in

  let processed_fields = 
    C.munge_fields_state
      
      (* Absorb field *)
      (fun ctp n -> 
         (match ctp with 
             Ast.Type tp -> gen_record_abs_type (gen_f dt tc loc tp) tp n
           | Ast.Exp exp -> gen_record_abs_lit exp n),
         n+1)
      
      (* Normal field *)
      (fun (id,tp) n ->  
         gen_record_full id (gen_f dt tc loc tp) n,
         n+1)
      
      (* Compute field *)
      (fun (id,tp,exp) n -> gen_record_comp id (gen_f dt tc loc tp) exp n,n)
      
      (* Let field *)
      (fun (id,exp) n -> gen_record_let id exp n,n)
      
      1 fields
  in
    gen_record_compose is_tuple names processed_fields

and gen_table_f id sep term keytype path =
  Table (id, sep, term, keytype, path)

and gen_f dt tc loc = function 

    Ast.TupleTp ctps ->
      let fresh_rep = (PadscId.id2string (PadscId.freshid "r")) ^ "_" in
      (*let fresh_pd = (PadscId.id2string (PadscId.freshid "p")) ^ "_" in *)
      let rid i = fresh_rep ^ (string_of_int i) in
      (*let pid i = fresh_pd ^ (string_of_int i) in*)

      (* Convert tuple elements into record fields *)
      let convert (i,fds) = function
          Ast.Type tp -> i+1,Ast.FullField(PadscId.makeid (rid i),tp)::fds
        | (Ast.Exp exp) as a -> i,(Ast.AbsorbField a)::fds
      in
        
      (* Use fold left so that fields will be numbered from 1...n,
         rather than n...1 *)
      let _,fields_rev = List.fold_left convert (1,[]) ctps in
        gen_record_f dt tc loc (List.rev fields_rev) true           
          
  | Ast.TidTp tid ->
      (match D.lookup_descr dt tid with
          None -> (PadscError.report_warning loc ("simpleAst: Type " ^ (N.mk_top_level_rep_str tid) ^ " is an Ocaml type.");
      		   TyId tid)
        | Some d -> TyId tid)
  | Ast.ValAppTp (tp_fun,exp) -> ValApp (gen_f dt tc loc tp_fun, exp)

  | Ast.TpAppTp (tp_args,tp_fun) -> 
      let gen_ty = gen_f dt tc loc in
	TyApp (tp_fun, List.map gen_ty tp_args)
      
  | Ast.WhereTp (id,tp,exp) ->  
      let sub_f = gen_f dt tc loc tp in
        gen_where_f sub_f id exp

  | Ast.HostTp (t) -> HostTp t

  | _ -> PadscError.report_error loc "unsupported feature"
              
let gen_tp_f_body dt tc loc = function    
    (* We single out records here, because records cannot be anonymous
       (nor, therefore, nested) and so are only allowed at top
       level. 

       XXX: doesn't this belong in the type checker? *)
    Ast.RecordTp fields -> 
      gen_record_f dt tc loc fields false
    (*TableTp can appear only at top level as well*)
  | Ast.TableTp (id, sep, term, keytype, path) ->
      gen_table_f id sep term keytype path
  | b -> gen_f dt tc loc b      
    
(**
   returns: strings rep_c,pd_c
   rep_c : the rep data constructor 
   pd_c : the pd data constructor *)
let mk_cons id = 
  N.mk_rep_c_str id, N.mk_pd_c_str id

let gen_dtp_f_body dt tc loc = function
    Ast.ImplicitDT (vs,def_opt) -> 

      (* mkmatch - generate the match expression for a particular variant.
         var    : the given variant
         acc : the accumulator for the fold. *)
      let mk_case var acc =
        match var with
          Ast.FullVar(id,tp) -> 
            (* vt_f : the function for the variant. *)
            let vt_f = gen_f dt tc loc tp in
              gen_dt_full_var vt_f id acc

        | Ast.AbsorbVar(id,Ast.Type tp) -> 
            (* vt_f : the function for the variant. *)
            let vt_f = gen_f dt tc loc tp in
              gen_dt_abs_type_var vt_f tp id acc

        | Ast.AbsorbVar(id,Ast.Exp e) ->  gen_dt_abs_lit_var e id acc
              (* XXX: Why limited to literals? *)
              
      in

        (* Make the default constructors from the optional id. *)
      let mk_def_cons id_opt =
        (* Choose the right string for the constructor names. *)
        match id_opt with
              None -> N.def_vt, N.def_pd_vt
            | Some id -> N.mk_rep_c_str id, N.mk_pd_c_str id
      in

      let def_vt = match def_opt with
          Some (Ast.GenDefault(id_opt,tp,e)) -> 
            let (def_cons,_) = mk_def_cons id_opt in
              gen_dt_comp_def (gen_f dt tc loc tp) e (PadscId.makeid def_cons)

        | Some (Ast.FullDefault(id_opt,tp)) ->
            let def_f = gen_f dt tc loc tp in
            let (def_cons, _) = mk_def_cons id_opt in
              gen_dt_full_def def_f (PadscId.makeid def_cons)

        | None -> gen_dt_no_def (PadscId.makeid N.err_vt)
      in

      let cases = List.fold_right mk_case vs def_vt in
        gen_dt_compose cases
          
  | Ast.CaseDT (descr,cases) -> 
      let mk_case = function
          Ast.FullCase(pat,id,tp) -> 
            (* case_f: the function for the case. *)
            let case_f = gen_f dt tc loc tp in
              gen_dt_full_case pat case_f id

        | Ast.AbsorbCase(pat,id,Ast.Type tp) -> 
            (* case_f: the function for the case. *)
            let case_f = gen_f dt tc loc tp in
              gen_dt_abs_type_case pat case_f tp id

        | Ast.AbsorbCase(pat,id,Ast.Exp e) -> 
              gen_dt_abs_lit_case pat e id

        | Ast.GenCase(pat,id,tp,e) -> 
              gen_dt_comp_case pat (gen_f dt tc loc tp) e id
 
     in
        (* Default error case, in case nothing else matches. *)
      let def_case = gen_dt_def_case (PadscId.makeid N.err_vt) in
        (* cs is the list of processed cases. *)
      let cs = (List.map mk_case cases) @ [def_case] in
        gen_dt_match_compose descr cs
      
let trans dt current_descr tc loc = function 
    Ast.TpDef tp -> gen_tp_f_body dt tc loc tp
  | Ast.DtDef dtp_body -> gen_dtp_f_body dt tc loc dtp_body
	  
