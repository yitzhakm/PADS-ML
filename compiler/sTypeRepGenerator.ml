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
module HL = HostLanguage
  
(* Import the metadata type so as not to have to prefix each field
   name with "Description." *)
type metadata = Description.metadata = {
  name : PadscId.id; (** The (visible) name of the description. *)
  kind : D.kind;
  tyclass : D.tyclass;
}

(** Dummy location provided as default of quotations. Quotations that
    want to specify their own location should just locally rebind
    loc. The use of a dummy loc is ok as all of the locs in the ast
    are replaced by camlp4 once the ast is returned at the top
    level.*)
let loc = Ploc.dummy

let tool = <:expr<$lid:N.tyrep_tool_rec$>>
let tool_pre = <:expr<$lid:N.tyrep_tool_rec$.$uid:N.tyrep_unit_gftys$>>

let tyrep = <:expr<$lid:N.tyrep$>>

let tuple_to_fields tys =
  let rid i = "elt"^(string_of_int i) in
  (* Convert a tuple-element AST node to a record-field AST node. *)
  let convert (i,fds) ty = i+1,(PadscId.makeid (rid i),ty)::fds in
  let _,fields_rev = List.fold_left convert (1,[]) tys in
    List.rev fields_rev

let gen_single_record_tyrep gt field is_tuple =
  (* for a record { f1 : t1 } or degenerate tuple (t1), we want to generate:

	   RECORD:
     let to_rep { f1 = v } = (v, ()) in 
	 let from_rep (v, ()) = { f1 = v } in
	 let to_pd { f1_pd = pd } = (pd, (Pads.spanless_pd_hdr, ())) in
	 let from_pd (pd, _) = { f1_pd = pd } in

	   TUPLE:
	 let to_rep v = (v, ()) in
	 let from_rep (v, ()) = v in
	 let to_pd pd = (pd, (Pads.spanless_pd_hdr, ())) in
	 let from_pd (pd, _) = pd in

	 let sub_tyrep tool =
	   tool.UnitGFTys.tuple Singleton "f1" t1.tyrep Punit.tyrep 
     in
       tool.UnitGFTys.datatype sub_tyrep (to_rep, from_rep) (to_pd, from_pd)
  *)
  let (name, tp) = field in
  let name = PadscId.id2string name in

  let rep_conv e = 
	if is_tuple
	then 
	  <:expr<let to_rep v = (v, ()) in 
	         let from_rep (v, ()) = v in 
			 let to_pd pd = (pd, (Pads.spanless_pd_hdr, ())) in
			 let from_pd (pd, _) = pd in   
			   $e$>>
	else 
	  let pd_name = name ^ "_pd" in
	  <:expr<let to_rep { $lid:name$ = v } = (v, ()) in 
	         let from_rep (v, ()) = { $lid:name$ = v } in
			 let to_pd { $lid:pd_name$ = pd } = (pd, (Pads.spanless_pd_hdr, ())) in
			 let from_pd (pd, _) = { $lid:pd_name$ = pd } in
			   $e$>>
  in
  let tool_calls = 
	<:expr<
		   let $lid:N.sub_tyrep$ = {UnitGFTys.trep = fun $lid:N.tyrep_tool_rec$ ->
			 $tool_pre$.tuple 
			   GenFunTys.Singleton $str:name$
			   $gt tp$
			   Punit.$tyrep$}
		   in
			 $tool_pre$.datatype $lid:N.sub_tyrep$ (to_rep, from_rep) (to_pd, from_pd)>>
  in
	rep_conv tool_calls

let gen_multi_record_tyrep gt fields is_tuple =
  (* what we want to generate for a record { f1 : t1; f2 : t2; ... ; fn : tn }
	 or tuple (t1, t2, ..., tn) is:

	   RECORD:
	 let to_rep { f1 = f1; f2 = f2; ... ; fn = fn } = (f1, (f2, (..., fn))) in
	 let from_rep (f1, (f2, (..., fn))) = { f1 = f1; f2 = f2; ... ; fn = fn } in

	 let to_pd { f1_pd = f1_pd; f2_pd = f2_pd; ... ; fn_pd = fn_pd} = 
	   (f1_pd, (Pads.spanless_pd_hdr, (f2_pd, (Pads.spanless_pd_hdr, (..., fn_pd)))))
	 in
	 let from_pd (f1_pd, (_, (f2_pd, (_, (..., fn_pd))))) = 
	   { f1_pd = f1_pd; f2_pd = f2_pd; ... ; fn_pd = fn_pd } 
	 in 
	   ...

	   TUPLE:
	 let to_rep (f1, f2, ..., fn) = (f1, (f2, (..., fn))) in
	 let from_rep (f1, (f2, (..., fn))) = (f1, f2, ..., fn) in
	 let to_pd (f1_pd, f2_pd, ..., fn_pd) =
	   (f1_pd, (Pads.spanless_pd_hdr, (f2_pd, (Pads.spanless_pd_hdr, (..., fn_pd)))))
	 in
	 let from_pd (f1_pd, (_, (f2_pd, (_, (..., fn_pd))))) = 
	   (f1_pd, f2_pd, ..., fn_pd)
     in
	   ...


	 let sub_tyrep tool = 
	   tool.UnitGFTys.tuple (Last "fn") "f(n-1)" tn.tyrep t(n-1).tyrep
	 in
	 ...
	 let sub_tyrep tool = 
	   tool.UnitGFTys.tuple Middle "f2" t2.tyrep sub_tyrep
	 in
	 let sub_tyrep tool =
	   tool.UnitGFTys.tuple First "f1" t1.tyrep sub_tyrep
	 in
	   tool.UnitGFTys.datatype sub_tyrep (to_rep, from_rep) (to_pd, from_pd)
  *)

  (* build up rep assignments and pd_assignment: 
	 { f1 = f1'; ... } or (f1', ...) 

	 and

	 { f1_pd = f1_pd; ... } or (f1_pd, ...)

	 TODO fix this, fscking camlp4 lists
  *)
  let rep_assignments, pd_assignments = 
	List.split 
	  (List.map
		  (fun (name, tp) -> 
			let name = PadscId.id2string name in 
			let pd = name ^ "_pd" in
			  
			let rep_a = (<:patt<$lid:name$>>, <:expr<$lid:name$>>) in
			let pd_a = (<:patt<$lid:pd$>>, <:expr<$lid:pd$>>) in
			  (rep_a, pd_a))
		  fields)
  in
	
  let rec nested_tuples names = 
	let (pl, el)::(pntl, entl)::names' = List.rev names in
	let names = List.rev names' in
	  List.fold_right
		(fun (p, e) (ps, es) ->
		  let ps' = <:patt<($p$, $ps$)>> in
		  let es' = <:expr<($e$, $es$)>> in
			(ps', es'))
		names
		(<:patt<($pntl$, $pl$)>>, <:expr<($entl$, $el$)>>)
  in

  (* build (f1, (f2, (..., fn))) as an expression and a pattern *)
  let rep_tuple_p, rep_tuple_e = nested_tuples rep_assignments in

  (* build (f1_pd, (hdr, (f2_pd, (hdr, (..., fn_pd))) as an expression
	 and a pattern, where the hdr is _ in the pattern and
	 Pads.spanless_pd_hdr in the expression *)
  let pd_tuple_p, pd_tuple_e = 
	let (pl, el)::(pntl, entl)::names' = List.rev pd_assignments in
	let names = List.rev names' in
	  List.fold_right
		(fun (p, e) (ps, es) ->
		  let ps' = <:patt<($p$, (_, $ps$))>> in
		  let es' = <:expr<($e$, (Pads.spanless_pd_hdr, $es$))>> in
			(ps', es'))
		names
		(<:patt<($pntl$, $pl$)>>, <:expr<($entl$, $el$)>>)
  in

  let iso e =
	if is_tuple
	then 
	  (*
		let to_rep (f1, f2, ..., fn) = (f1, (f2, (..., fn))) in
		let from_rep (f1, (f2, (..., fn))) = (f1, f2, ..., fn) in
		let to_pd (f1_pd, f2_pd, ..., fn_pd) =
		  (f1_pd, (Pads.spanless_pd_hdr, (f2_pd, (Pads.spanless_pd_hdr, (..., fn_pd)))))
	    in
		let from_pd (f1_pd, (_, (f2_pd, (_, (..., fn_pd))))) = 
		  (f1_pd, f2_pd, ..., fn_pd)
		  ...
	  *)
	  let rep_p, rep_e = List.split rep_assignments in
	  let pd_p, pd_e = List.split pd_assignments in
	  <:expr<let to_rep ($list:rep_p$) = $rep_tuple_e$ in
	         let from_rep $rep_tuple_p$ = ($list:rep_e$) in
			 let to_pd ($list:pd_p$) = $pd_tuple_e$ in
			 let from_pd $pd_tuple_p$ = ($list:pd_e$) in
			   $e$>>
	else
	  (* 
		 let to_rep { f1 = f1; f2 = f2; ... ; fn = fn } = (f1, (f2, (..., fn))) in
		 let from_rep (f1, (f2, (..., fn))) = { f1 = f1; f2 = f2; ... ; fn = fn } in
		 let to_pd { f1_pd = f1_pd; f2_pd = f2_pd; ... ; fn_pd = fn_pd} = 
		 (f1_pd, (Pads.spanless_pd_hdr, (f2_pd, (Pads.spanless_pd_hdr, (..., fn_pd)))))
		 in
		 let from_pd (f1_pd, (_, (f2_pd, (_, (..., fn_pd))))) = 
		 { f1_pd = f1_pd; f2_pd = f2_pd; ... ; fn_pd = fn_pd } 
		 in 
		   ...
	  *)
	  let assign_to_patt assignments = List.map (fun (patt, _) -> (patt, patt)) assignments in
	  let rep_patt = assign_to_patt rep_assignments in
	  let pd_patt = assign_to_patt pd_assignments in
	  <:expr<let to_rep { $list:rep_patt$ } = $rep_tuple_e$ in
	         let from_rep $rep_tuple_p$ = { $list:rep_assignments$ } in
			 let to_pd { $list:pd_patt$ } = $pd_tuple_e$ in
	         let from_pd $pd_tuple_p$ = { $list:pd_assignments$ } in
			   $e$>>
  in
	(* 
	   In general, we want to generate the following:
	   
	     let sub_tyrep tool = 
	     tool.UnitGFTys.tuple (Last "fn") "f(n-1)" tn.tyrep t(n-1).tyrep
	     in
	     ...
	     let sub_tyrep tool = 
	     tool.UnitGFTys.tuple Middle "f2" t2.tyrep sub_tyrep
	     in
	     let sub_tyrep tool =
	     tool.UnitGFTys.tuple First "f1" t1.tyrep sub_tyrep
	     in
	     ...

	   There are a few edge cases. We know we have at least two
	   fields, since the zero record case is delegated to punit and
	   the single record case is delegated to
	   gen_single_record_tyrep.

	   If we have only two fields, then we want to generate a
	   First_last sub_tyrep, with no middle or first.
	*)
  let sub_tyreps e =
	let mk_first (f1, t1) e =
	  <:expr<
		let $lid:N.sub_tyrep$ =
			  {UnitGFTys.trep = fun $lid:N.tyrep_tool_rec$ ->
				$tool_pre$.tuple
				  GenFunTys.First 
				  $str:PadscId.id2string f1$
				  $gt t1$
				  $lid:N.sub_tyrep$}
			in
			  $e$>>
	in
	let mk_middle (f, t) e =
	  <:expr<
		let $lid:N.sub_tyrep$ =
		  {UnitGFTys.trep = fun $lid:N.tyrep_tool_rec$ -> 
			$tool_pre$.tuple 
			  GenFunTys.Middle
			  $str:PadscId.id2string f$
			  $gt t$
			  $lid:N.sub_tyrep$}
		in
		  $e$>>
	in
	let mk_last (fntl, tntl) (fl, tl) e =
	  <:expr<
		let $lid:N.sub_tyrep$ = 
		  {UnitGFTys.trep = fun $lid:N.tyrep_tool_rec$ -> 
			$tool_pre$.tuple 
			  (GenFunTys.Last $str:PadscId.id2string fl$)
			  $str:PadscId.id2string fntl$
			  $gt tntl$
			  $gt tl$}
		in
		  $e$>>
	in
	match fields with
		[(f1, t1); (f2, t2)] ->
		  <:expr<
			let $lid:N.sub_tyrep$ = 
			  {UnitGFTys.trep = fun $lid:N.tyrep_tool_rec$ -> 
				$tool_pre$.tuple 
				  (GenFunTys.First_last $str:PadscId.id2string f2$)
				  $str:PadscId.id2string f1$
				  $gt t1$
				  $gt t2$}
			in
			  $e$>>
	  | first::rest ->
		  let last::next_to_last::rest = List.rev rest in
			mk_last next_to_last last 
			  (List.fold_right
				  mk_middle
				  rest
				  (mk_first first e))
  in
	(* we have the isomorphism between the record or tuple and the
	   nested tuple type, we've written the subtyreps -- it's time to
	   call datatype and return:

	   tool.UnitGFTys.datatype sub_tyrep (to_rep, from_rep) (to_pd, from_pd)
	*)
  let datatype_call = 
	<:expr<$tool_pre$.datatype $lid:N.sub_tyrep$ (to_rep, from_rep) (to_pd, from_pd)>>
  in
	iso (sub_tyreps datatype_call)

(* a wrapper around the record producer to convert empty tuples and records to punit *)
let gen_record_tyrep gp fields is_tuple =
  match fields with
	(* if there's just one, we treat that specially *)
	[field] -> gen_single_record_tyrep gp field is_tuple
	  
	(* other size records *)
	| fields -> gen_multi_record_tyrep gp fields is_tuple

let gen_datatype_tyrep gt variants =
  (* for a datatype Bar | Foo a | ... | Quux | DtErr we want to generate the following

	 let to_rep = function
	   | Bar -> GenFunTys.Left ()
	   | Foo v -> GenFunTys.Right (GenFunTys.Left v)
	   | ...
	   | Quux -> GenFunTys.Right (GenFunTys.Right (... (GenFunTys.Left ())))
	   | DtErr -> GenFunTys.Right (GenFunTys.Right (... (GenFunTys.Right ())))
	 in
	 let from_rep = function
	   | GenFunTys.Left () -> Bar
	   | GenFunTys.Right (GenFunTys.Left v) -> Foo v
	   | ...
	   | GenFunTys.Right (GenFunTys.Right (... (GenFunTys.Left ()))) -> Quux
	   | GenFunTys.Right (GenFunTys.Right (... (GenFunTys.Right ()))) -> DtErr
	 in
	 
	 let to_pd = function
	   | Bar -> GenFunTys.Left (Pads.spanless_pd_hdr, ())
	   | Foo pd -> GenFunTys.Right (Pads.spanless_pd_hdr, (GenFunTys.Left pd))
	   | ...
	   | Quux -> GenFunTys.Right (Pads.spanless_pd_hdr, (GenFunTys.Right (... (Pads.spanless_pd_hdr, (GenFunTys.Left (Pads.spanless_pd_hdr, ()))))))
	   | DtErr -> GenFunTys.Right (Pads.spanless_pd_hdr, (GenFunTys.Right (... (Pads.spanless_pd_hdr, (GenFunTys.Right (Pads.spanless_pd_hdr, ()))))))
	 in
	 let from_pd = function
	   | GenFunTys.Left _ -> Bar_pd
	   | GenFunTys.Right (_, (GenFunTys.Left pd)) -> Foo_pd pd
	   | ...
	   | GenFunTys.Right (_, (GenFunTys.Right (... (_, GenFunTys.Left _)))) -> Quux_pd
	   | GenFunTys.Right (_, (GenFunTys.Right (... (_, GenFunTys.Right _)))) -> DtErr_pd
	 in

	 let sub_tyrep tool =
	   tool.UnitGFTys.sum (GenFunTys.Last "DtErr")
	     "Quux"
         Punit.tyrep true
	     Punit.tyrep true
	 in
	 ...
	 let sub_tyrep tool =
	   tool.UnitGFTys.sum GenFunTys.Middle
	     "Foo"
         a.tyrep false
         sub_tyrep false
	 in
	 let sub_tyrep tool =
	   tool.UnitGFTys.sum GenFunTys.First
	     "Bar"
	     Punit.tyrep true
	     sub_tyrep false
	 in
	   tool.UnitGFTys.datatype sub_tyrep (to_rep, from_rep) (to_pd, from_pd)

  *)

  (* puts depth (f (GenFunTys.Right ...)) around e *)
  let rec pack_e f e depth last =
	match depth, last with
	    (0, true)
	  | (1, true) -> <:expr<GenFunTys.Right $e$>>
	  | (0, false) -> <:expr<GenFunTys.Left $e$>>
	  | _ -> <:expr<GenFunTys.Right $f (pack_e f e (depth - 1) last)$>>
  in

  (* same as pack_e, only for patterns *)
  let rec pack_p f p depth last =
	match depth, last with
	    (0, true)
	  | (1, true) -> <:patt<GenFunTys.Right $p$>>
	  | (0, false) -> <:patt<GenFunTys.Left $p$>>
	  | _ -> <:patt<GenFunTys.Right $f (pack_p f p (depth - 1) last)$>>
  in

  (*
	We're building clauses in the following matches:

	 let to_rep = function
	   | Bar -> GenFunTys.Left ()
	   | Foo v -> GenFunTys.Right (GenFunTys.Left v)
	   | ...
	   | Quux -> GenFunTys.Right (GenFunTys.Right (... (GenFunTys.Left ())))
	   | DtErr -> GenFunTys.Right (GenFunTys.Right (... (GenFunTys.Right ())))
	 in
	 let from_rep = function
	   | GenFunTys.Left () -> Bar
	   | GenFunTys.Right (GenFunTys.Left v) -> Foo v
	   | ...
	   | GenFunTys.Right (GenFunTys.Right (... (GenFunTys.Left ()))) -> Quux
	   | GenFunTys.Right (GenFunTys.Right (... (GenFunTys.Right ()))) -> DtErr
	 in
  *)
  let mk_rep_clause (v, tp) depth last = 
        let v_s = PadscId.id2string v in
	let packed_with pack sub = pack (fun x -> x) sub depth last in

	(* Foo v -> OR Bar -> *)
	let to_rep_p = 
	  match tp with
		  Some tp -> <:patt<$uid:v_s$ v>>
		| None -> <:patt<$uid:v_s$>>
	in

	(* R (R (R (R (L ...)))), where ... is v or () *)
	let to_rep_e =
	  packed_with 
		pack_e
		(match tp with
			Some tp -> <:expr<v>>
		  | None -> <:expr<()>>)
	in

	(* R (R (R (R (L ...)))) as a pattern *)
	let from_rep_p = 
	  packed_with
		pack_p
		(match tp with
			Some tp -> <:patt<v>>
		  | None -> <:patt<_>>)
	in

	(* Foo v or Bar *)
	let from_rep_e =
	  match tp with
		  Some tp -> <:expr<$uid:v_s$ v>>
		| None -> <:expr<$uid:v_s$>>
	in
	  (* return pattern matching clauses for the type in both to_rep
		 and from_rep *)
	  (to_rep_p, None, to_rep_e),
	  (from_rep_p, None, from_rep_e)
  in
	
  (* 
	 we're building clauses in the following matches:

	 let to_pd = function
	   | Bar -> GenFunTys.Left (Pads.spanless_pd_hdr, ())
	   | Foo pd -> GenFunTys.Right (Pads.spanless_pd_hdr, (GenFunTys.Left pd))
	   | ...
	   | Quux -> GenFunTys.Right (Pads.spanless_pd_hdr, (GenFunTys.Right (... (Pads.spanless_pd_hdr, (GenFunTys.Left (Pads.spanless_pd_hdr, ()))))))
	   | DtErr -> GenFunTys.Right (Pads.spanless_pd_hdr, (GenFunTys.Right (... (Pads.spanless_pd_hdr, (GenFunTys.Right (Pads.spanless_pd_hdr, ()))))))
	 in
	 let from_pd = function
	   | GenFunTys.Left _ -> Bar_pd
	   | GenFunTys.Right (_, (GenFunTys.Left pd)) -> Foo_pd pd
	   | ...
	   | GenFunTys.Right (_, (GenFunTys.Right (... (_, GenFunTys.Left _)))) -> Quux_pd
	   | GenFunTys.Right (_, (GenFunTys.Right (... (_, GenFunTys.Right _)))) -> DtErr_pd
	 in
 
  *)
  let mk_pd_clause (v, tp) depth last = 
	let v_s = (PadscId.id2string v) ^ "_pd" in

	(* Foo_pd pd -> OR Bar_pd -> *)
	let to_pd_p = 
	  match tp with
		  Some tp -> <:patt<$uid:v_s$ pd>>
		| None -> <:patt<$uid:v_s$>>
	in

	(* R (hdr, (R (hdr, (L ...)))), where ... is pd or (hdr, ()) *)
	let to_pd_e =
	  pack_e
		(fun e -> <:expr<(Pads.spanless_pd_hdr, $e$)>>)
		(match tp with
			Some tp -> <:expr<pd>>
		  | None -> <:expr<(Pads.spanless_pd_hdr, ())>>)
		depth last
	in

	(* R (_, (R (_, (L ...)))), where ... is pd or _ *)
	let from_pd_p = 
	  pack_p
		(fun p -> <:patt<(_, $p$)>>)
		(match tp with
			Some tp -> <:patt<pd>>
		  | None -> <:patt<_>>)
		depth last
	in

	(* Foo v or Bar *)
	let from_pd_e =
	  match tp with
		  Some tp -> <:expr<$uid:v_s$ pd>>
		| None -> <:expr<$uid:v_s$>>
	in
	  (* return pattern matching clauses for the type in both to_pd
		 and from_pd *)
	  (to_pd_p, None, to_pd_e),
	  (from_pd_p, None, from_pd_e)
  in
	
  (* a way to map over the set of variants while keeping a depth
	 counter and detecting the last node
  *)
  let depth_map (f : 'a -> int -> bool -> 'b) (ls : 'a list) : 'b list =
	let rec imap i ls =
	  match ls with
		  [] -> []
		| [v] -> [(f v i true)]
		| v::ls -> (f v i false)::(imap (i + 1) ls)
	in
	  imap 0 ls
  in

  (* collect all of the clauses *)
  let collect_clauses collect = List.split (depth_map collect variants) in
  let to_reps, from_reps = collect_clauses mk_rep_clause in
  let to_pds, from_pds = collect_clauses mk_pd_clause in

  (* define {to,from}_{rep,pd} *)
  let mk_fun name clauses = 
	fun e ->
	  <:expr<let $lid:name$ = fun [$list:clauses$] in $e$>> 
  in
  let to_rep   = mk_fun "to_rep"   to_reps in
  let from_rep = mk_fun "from_rep" from_reps in
  let to_pd   = mk_fun "to_pd"   to_pds in
  let from_pd = mk_fun "from_pd" from_pds in

  (* iso adds all of the isomorphism functions to an expression *)
  let iso e = to_rep (from_rep (to_pd (from_pd e))) in

  (* 
	 In general, we want to generate the following:
	 
	 let sub_tyrep tool =
	   tool.UnitGFTys.sum (GenFunTys.Last "DtErr")
	     "Quux"
         Punit.tyrep true
	     Punit.tyrep true
	 in
	 ...
	 let sub_tyrep tool =
	   tool.UnitGFTys.sum GenFunTys.Middle
	     "Foo"
         a.tyrep false
         sub_tyrep false
	 in
	 let sub_tyrep tool =
	   tool.UnitGFTys.sum GenFunTys.First
	     "Bar"
	     Punit.tyrep true
	     sub_tyrep false
	 in

	 There are a few edge cases. 

	 We have at least two variants, since the single-variant case is
	 by gen_tyrep.
	 
	 If we have only two variants, then we want to generate a
	 First_last sub_tyrep, with no middle or first.
  *)
  let sub_tyreps e =
	let gt t =
	  match t with
		  Some t -> gt t
		| None -> gt (SimpleAst.TyId (PadscId.makeid "Punit"))
	in
	let is_empty t =
	  (* why are True and False capitalized?  because camlp4 is like
		 Ocaml, but CRAZY!  they should call it crazyp4. *)
	  match t with
		  Some _ -> <:expr<False>>
		| None -> <:expr<True>>
	in
	let mk_first (v1, t1) e =
	  <:expr<
		let $lid:N.sub_tyrep$ =
		  {UnitGFTys.trep = fun $lid:N.tyrep_tool_rec$ ->
			$tool_pre$.sum
			  GenFunTys.First 
			  $str:PadscId.id2string v1$
			  $gt t1$ $is_empty t1$
			  $lid:N.sub_tyrep$ False}
		in
		  $e$>>
	in
	let mk_middle (v, t) e =
	  <:expr<
		let $lid:N.sub_tyrep$ =
		  {UnitGFTys.trep = fun $lid:N.tyrep_tool_rec$ -> 
			$tool_pre$.sum
			  GenFunTys.Middle
			  $str:PadscId.id2string v$
			  $gt t$ $is_empty t$
			  $lid:N.sub_tyrep$ False}
		in
		  $e$>>
	in
	let mk_last (vntl, tntl) (vl, tl) e =
	  <:expr<
		let $lid:N.sub_tyrep$ = 
		  {UnitGFTys.trep = fun $lid:N.tyrep_tool_rec$ -> 
			$tool_pre$.sum
			  (GenFunTys.Last $str:PadscId.id2string vl$)
			  $str:PadscId.id2string vntl$
			  $gt tntl$ $is_empty tntl$
			  $gt tl$ $is_empty tl$}
		in
		  $e$>>
	in
	  match variants with
		  [(v1, t1); (v2, t2)] ->
			<:expr<
			  let $lid:N.sub_tyrep$ = 
				{UnitGFTys.trep = fun $lid:N.tyrep_tool_rec$ -> 
				  $tool_pre$.sum
					(GenFunTys.First_last $str:PadscId.id2string v2$)
					$str:PadscId.id2string v1$
					$gt t1$ $is_empty t1$
					$gt t2$ $is_empty t2$}
			  in
				$e$>>
		| first::rest ->
			let last::next_to_last::rest = List.rev rest in
			  mk_last
				next_to_last last
				(List.fold_right
					mk_middle
					rest
					(mk_first first e))


  in

  let datatype_call = 
	<:expr<$tool_pre$.datatype $lid:N.sub_tyrep$ (to_rep, from_rep) (to_pd, from_pd)>>
  in
	iso (sub_tyreps datatype_call)

let rec gen_tyrep dt tc loc = function
	(* type application and type identifiers just need to have the
	   appropriate specialized producer built; then we can use the
	   derived 'produce' function *)
  | (SimpleAst.TyId _) 	  
  | (SimpleAst.TyApp _) as tp -> 
	  (* gen_specialize builds the appropriate tyrep; we then just pass in the tool *)
	  <:expr<$gen_specialize dt tc loc tp$.UnitGFTys.trep $lid:N.tyrep_tool_rec$>>

  | (SimpleAst.Table _) as tp ->
	  <:expr<$gen_specialize dt tc loc tp$.UnitGFTys.trep $lid:N.tyrep_tool_rec$>>
  (* we'd really, really like to handle this case, but there are a couple of
	 issues.  in particular, the type (from examples/pml/ganglia.pml):

	 ptype var_string_n (n:int) = {
	   size: [i:puint32 | (Int64.to_int i)<=n];
	   content: pstring_FW(make_mod4(Int64.to_int size)) 
	 }

	 here the value is dependent on earlier elements in the record.
	 the typerep generic functions can't deal with that -- we need a
	 dependent version of the tuple traversal.  this is absolutely
	 necessary to implement the printer and parser as generic tools.
	 future work? --mgreenberg *)
  | SimpleAst.ValApp (tp, _) -> gen_tyrep dt tc loc tp

  (* both tuples and records can be treated the same way, once tuples
	 have had their names munged 

	 empty tuples and records are treated like punit
  *)
  | SimpleAst.Tuple []
  | SimpleAst.Record [] -> gen_tyrep dt tc loc (SimpleAst.TyId (PadscId.makeid "Punit"))

  | SimpleAst.Tuple tps -> gen_record_tyrep (gen_specialize dt tc loc) (tuple_to_fields tps) true	  
  | SimpleAst.Record fields -> gen_record_tyrep (gen_specialize dt tc loc) fields false
	  
  | SimpleAst.Datatype [(v, None)] -> gen_tyrep dt tc loc (SimpleAst.TyId (PadscId.makeid "Punit"))
  | SimpleAst.Datatype [(v, Some t)] -> gen_tyrep dt tc loc t
  | SimpleAst.Datatype variants -> 

      (* filter out variants with embedded host types. *)
      let has_host_variants = function 
	  (_, Some t) -> C.contains_s_host_tp t 
	| _ -> false
      in
      if List.exists has_host_variants variants then
	PadscError.report_warning loc 
	  "STypeRepGenerator: host type used in some branch(es); will not be accessible via type rep.";
      (match List.filter (fun x -> not (has_host_variants x)) variants with
	  [(v, None)] -> gen_tyrep dt tc loc (SimpleAst.TyId (PadscId.makeid "Punit"))
	| [(v, Some t)] -> gen_tyrep dt tc loc t
	| variants -> gen_datatype_tyrep (gen_specialize dt tc loc) variants)
	  
  | SimpleAst.Constraint (tp, _, _) -> 
	  <:expr<$tool_pre$.ty_constraint $gen_specialize dt tc loc tp$>>

  | SimpleAst.HostTp _ -> PadscError.report_error loc "STypeRepGenerator: arbitrary use of host types unsupported"

(** generates the specialized producer and returns its 'produce function *)
and gen_specialize dt tc loc = function
	(* we just need to look up the type, specialize it, and return the produce function *)
  | SimpleAst.TyId tid ->
	  (match D.lookup_descr dt tid with
		  None -> PadscError.report_error loc ("SProducerGenerator: Type " ^ (PadscId.id2string tid) ^ " not found.")
		| Some d -> C.gen_TidTp loc "SProducerGenerator" tid N.tyrep_fun d)
  
  (* apply the type application recursively; eventually we'll
	 bottom out in one of the other cases and return the produce
	 function *)
  | SimpleAst.TyApp (tp_fun, tp_args) -> 
      let apply_fun f arg = <:expr<$f$ $gen_specialize dt tc loc arg$>> in
      let f = gen_specialize dt tc loc (SimpleAst.TyId tp_fun) in
		List.fold_left apply_fun f tp_args 
		  
  | SimpleAst.Table (id, _, _, keyt, _) -> 
      let apply_fun f arg = <:expr<$f$ $gen_specialize dt tc loc arg$>> in
      let f = gen_specialize dt tc loc (SimpleAst.TyId (PadscId.makeid "Ptable")) in
		List.fold_left apply_fun f [SimpleAst.TyId keyt; SimpleAst.TyId id]

  (* type reference -- just generate and use its tyrep *)	  
  | tp ->
	  <:expr<{UnitGFTys.trep = fun $lid:N.tyrep_tool_rec$ -> $gen_tyrep dt tc loc tp$}>>

let gen dt current_descr tc loc decl = 
  let (tp_params,name,val_param_opt,_, tp_def) = decl in
	
  let ty = SimpleAstTx.trans dt current_descr tc loc tp_def in
	
  let tyrep_body = 
	(* {terp = fun tool -> ... }*)
	<:expr<{UnitGFTys.trep = fun $lid:N.tyrep_tool_rec$ -> $gen_tyrep dt tc loc ty$}>>
  in

	(*
	  TODO fix sigs

	  let mk_specialize_producer_ty arg_repty arg_pdbty =
	  <:ctyp<Generic_producer.Rec_ver.t 'source -> Type.SPProducer.producer 'source $arg_repty$>>
	  in
	  
	  let poly_t   = C.package_funty mk_specialize_producer_ty tp_params mk_specialize_producer_ty in
	*)
  let poly_fun = C.package_fun tyrep_body tp_params N.tyrep_fun in
	
  let tp_pf_name = N.tyrep_fun in
	([],
	([], (* TODO fix sigs <:sig_item<value $lid:tp_pf_name$ : $poly_t$>>], *)
	 [<:str_item<open Generic>>;
	  <:str_item<module UnitGFTys = GenFunTys.Make(UnitClass)>>;
	  <:str_item<value rec $lid:tp_pf_name$ = $poly_fun$>>]),
	current_descr)
