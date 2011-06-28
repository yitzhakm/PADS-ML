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
(** Compiler from PADS/ML AST into XSchema *)

open SimpleAst

(**************)
(* NAMESPACES *)

(* The specification specific namespace is used for element, while the
   general padsns namespace is used for basetypes and parse descriptors.

   Namespace declarations are introduced in build_schema.
*)
let spec_prefix = "pml"

let pads_prefix = "padsns"
let pads_uri = "http://www.padsproj.org/pads.xsd"

(*************************)
(* INTERNAL SCHEMA TYPES *)

(* These types are the intermediary between Pads/ML's
   SimpleAst.simple_type and XML Schema.
*)

type schema_name = UQName of string (* unqualified, name only *) 
				   | QName of string * string (* qualified, namespace and name *)

let uqname_of_id id = UQName (PadscId.id2string id)

let string_of_sname sname = 
  match sname with
	  UQName n -> n
	| QName (ns, n) -> ns ^ ":" ^ n

(* the internal schema type

   simple_type_to_schema compiles SimpleAst.simple_type into
   pads_schema 

   build_xschema uses the usertype_registry, which maps top-level
   user-defined type names to pads_schema, to build an XML Schema
*)
type pads_schema = 
	(* XML elements *)
	Element of schema_name * pads_schema
	
	(* combinators on XML elements *)
	| Sequence of pads_schema list
	| Choice of pads_schema list
	| Repeat of pads_schema
	| Optional of pads_schema
	
	(* the empty schema *)
	| Empty
		
	(* an element of a given type with a given name *)
	| ElementType of schema_name * schema_name (* tag name and type name *)

	(* an extension of a given type with a given name and some new elements *)
	| ElementTypeExtension of schema_name * schema_name * pads_schema (* tag name, type name, new elements *)

	(* a reference to a global element (in XML Schema parlance), which
	   defines its own name *)
	| ElementRef of schema_name (* type name only *)

	(* a type-parameter reference, should be used only in user-defined
	   parameterized types and then substituted out before schema generation *)
	| TypeParamRef of PadscId.id

(* a pretty printer for pads_schema *)
let rec print_schema schema =
  let print_schemata schemata =
	print_string "[";
	List.iter (fun schema ->
	  print_schema schema; print_string ";")
	  schemata;
	print_string "]"
  in
  match schema with
	  Element (name, schema) ->
		print_string ("Element (" ^ (string_of_sname name) ^ ", ");
		print_schema schema;
		print_endline ")"
	| Sequence schemata ->
		print_string "Sequence ";
		print_schemata schemata;
		print_endline ""
	| Choice schemata ->
		print_string "Choice ";
		print_schemata schemata;
		print_endline ""
	| Repeat schema ->
		print_string "Repeat ";
		print_schema schema
	| Optional schema ->
		print_string "Optional ";
		print_schema schema
	| Empty ->
		print_endline "Empty"
	| ElementType (name, typename) ->
		print_endline ("Type " ^ (string_of_sname typename) ^ " as " ^ (string_of_sname name))
	| ElementRef typename ->
		print_endline ("Ref " ^ (string_of_sname typename))
	| TypeParamRef id ->
		print_endline ("TypeVar " ^ (PadscId.id2string id))

(* a reference to an optional parse descriptor *)
let optional_pd = Optional (ElementRef (QName (pads_prefix, "pd")))
let optional_semanticpd = Optional (ElementRef (QName (pads_prefix, "semanticpd")))

(*********************)
(* USERTYPE REGISTRY *)

(* maps user-defined top-level type names to their respective schema.  

   it is important to note that all top-level types produced by
   simple_type_to_ast have Element as their outermost pads_schema, but
   that the types stored in the registry store only the child
*)
type usertype_registry_entry = PadscId.id * pads_schema
let usertype_registry : usertype_registry_entry list ref = ref []

let register_usertype (name : PadscId.id) (schema : pads_schema) : unit =
  let type_schema = match schema with
	  Element (_, type_schema) -> type_schema

	(* !!! direct inclusion, breaks mutually recursive types 

	   to implement mutually recursive types, you'll need to separate
	   ElementType (name, typename) into Element (name, TypeRef typename)

	   unfortunately, this change makes the code for
	   pads_schema_to_xschema somewhat more complicated
	   
	*)
	| ElementType (_, UQName typename) -> List.assoc (PadscId.makeid typename) !usertype_registry
	| _ -> failwith ("Couldn't register non-element top-level type " ^ (PadscId.id2string name) ^ ".  simple_type_to_schema should always produce an Element pads_schema for a top-level type.")
  in
	usertype_registry := (name, type_schema)::!usertype_registry

(*********************)
(* BASETYPE REGISTRY *)

type basetype_registry_entry = PadscId.id * (PadscId.id -> pads_schema)

(* creates an entry for a basetype with a given type *)
let basetype_to_entry (name : string) (val_type : pads_schema) : basetype_registry_entry =
  let schema name = Element (uqname_of_id name,
							Sequence [Optional val_type; 
									  optional_pd]) 
  in 
	(PadscId.makeid name, schema)

(* creates an entry for a basetype with a type based on its name *)
let make_basetype name = basetype_to_entry name (ElementType (UQName "val", (QName (pads_prefix, name))))

let make_unit_basetype name = basetype_to_entry name (Element (UQName "val", Empty))

(* creates a dummy basetype *)
let basetype_TODO name = (PadscId.makeid name, fun _ -> Empty)

let basetype_registry : basetype_registry_entry list = 
  [
	make_basetype "Pint";
	make_basetype "Pint8";
	make_basetype "Pint16";
	make_basetype "Pint32";
	make_basetype "Pint64";
	make_basetype "Puint8";
	make_basetype "Puint16";
	make_basetype "Puint32";
	make_basetype "Puint64";
	make_basetype "Pint8_FW";
	make_basetype "Pint16_FW";
	make_basetype "Pint32_FW";
	make_basetype "Pint64_FW";
	make_basetype "Puint8_FW";
	make_basetype "Puint16_FW";
	make_basetype "Puint32_FW";
	make_basetype "Puint64_FW";
	make_basetype "Pchar"; 
	make_basetype "Pfloat32";
	make_basetype "Pfloat64";
	make_basetype "Pstring";
	make_basetype "Pstring_lit";
	make_basetype "Ptimestamp_explicit_FW";
	make_basetype "Pstring_FW";
	make_basetype "Pstring_ME";
	make_basetype "Pstring_SE";
	make_unit_basetype "Peof";
	make_unit_basetype "Peor";
	basetype_TODO "PIP";
	make_unit_basetype "Pcommit";
	make_unit_basetype "Punit";
  ]

(* joint lookup in the basetype and usertype registries -- it first
   checks the basetypes, then the user types; failing that, it raises an exception *)
exception No_such_type of string
let lookup_type_by_id (id : PadscId.id) = 
  (* lookup the basetype's definition *)
  try List.assoc id basetype_registry

  (* if it's not a basetype, then it must be a top-level name -- we leave a reference to the type *)
  with Not_found -> (fun name -> ElementType ((uqname_of_id name), (uqname_of_id id)))


(*******************************)
(* TYPE FUNCTIONS AND REGISTRY *)

type typefun = (PadscId.id -> simple_type list -> (PadscId.id -> simple_type -> pads_schema) -> pads_schema)
 and typefun_registry_entry = PadscId.id * typefun
 and typefun_registry = typefun_registry_entry list

let typefun_unary_ignore id [elt_type] simple_type_to_schema =
  (* precord is just a marker, and so behaves just like its single sub type *)
  simple_type_to_schema id elt_type

let typefun_Popt id [elt_type] simple_type_to_schema =
  let elt = simple_type_to_schema (PadscId.makeid "Some") elt_type in
	Element (uqname_of_id id, 
			Sequence [Choice [elt; Element (UQName "None", Empty)];
					  optional_pd])

let typefun_list id [elt_type] simple_type_to_schema =
  let elt = simple_type_to_schema (PadscId.makeid "elt") elt_type in
	Element (uqname_of_id id, 
			Sequence [Repeat elt; 
					  optional_pd])

let typefun_TODO name =
  let typefun _ _ _ = failwith ("The type function for " ^ name ^ " is unimplemented.") in
	(PadscId.makeid name, typefun)

let typefun_to_entry name typefun = (PadscId.makeid name, typefun)

let typefun_id_table : typefun_registry =
  [
	(* unary type functions *)
	typefun_to_entry "Popt" typefun_Popt;
	typefun_to_entry "Poption" typefun_Popt;
	typefun_to_entry "Plist" typefun_list;
	typefun_to_entry "Pstream" typefun_list;
	typefun_to_entry "Plist_ch" typefun_list;
	typefun_to_entry "Plist_re" typefun_list;
	typefun_to_entry "Plist_st" typefun_list;
	typefun_to_entry "Plist_nosep" typefun_list;
	typefun_to_entry "Plist_np" typefun_list;
	typefun_to_entry "Plist_longest" typefun_list;
	typefun_to_entry "Plist_longest_pred" typefun_list;
	typefun_to_entry "Ptable" typefun_list;

	(* unary type functions that don't do anything, just pass on the sub type *)
	typefun_to_entry "Precord" typefun_unary_ignore;
	typefun_to_entry "Ptry" typefun_unary_ignore;

	(* binary type functions *)
	typefun_TODO "Ptransform"
  ]

let user_typefun_id_table : typefun_registry ref =
  ref []

let substitute_type_params (typefun : pads_schema) (args : (PadscId.id * simple_type) list) simple_type_to_schema =
  let lookup var = List.assoc var args in
  let rec sub schema =
	match schema with
		(* actually susbtitute in the typeparamref; it's guaranteed to be inside
		   of an element thanks to the TyId case of simple_type_to_schema *)
		Element (UQName name, TypeParamRef var) -> 
		  simple_type_to_schema (PadscId.makeid name) (lookup var)

	    (* recursive susbtitution *)
	  | Element (name, child) -> Element (name, sub child)
	  | Sequence children -> Sequence (List.map sub children)
	  | Choice children -> Choice (List.map sub children)
	  | Repeat child -> Repeat (sub child)
	  | Optional child -> Optional (sub child)

        (* everything else can't have typeparamrefs inside them.  the only 
		   risk of this is with ElementTypeExtension, but that's only used
		   to add semanticpds to constraint schemata *)
	  | _ -> schema
  in
	sub typefun

let register_user_typefun (name : PadscId.id) (type_schema : pads_schema) (type_params : PadscId.id list) =
  let name_s = PadscId.id2string name in
  let type_schema = match type_schema with
	  Element (_, type_schema) -> type_schema
	| _ -> failwith ("Couldn't register non-element top-level type " ^ (PadscId.id2string name) ^ ".  simple_type_to_schema should always produce an Element pads_schema for a top-level type.")
  in
  let typefun id actual_params simple_type_to_schema =
	let typefun_args = List.combine type_params actual_params in
	let subbed_schema = substitute_type_params type_schema typefun_args simple_type_to_schema in
	  Element (uqname_of_id id, subbed_schema)
  in
	user_typefun_id_table := (name, typefun)::!user_typefun_id_table 

exception No_such_typefun of string
let lookup_typefun_by_id id =
  try List.assoc id typefun_id_table
  with Not_found -> 
	begin
	  try List.assoc id !user_typefun_id_table
	  with Not_found -> raise (No_such_typefun (PadscId.id2string id))
	end

(***************************************)
(* SIMPLE_TYPE -> PADS_SCHEMA COMPILER *)

(** Compiles a PADS/ML type (SimpleAst.simple_type) into a pads_schema *)
(* it is a VITAL invariant that top-level user-defined types produce Element nodes *)
let rec simple_type_to_schema (name : PadscId.id) (pml_type : simple_type) (type_params : PadscId.id list) (descr_table : Description.table) : pads_schema =
  match pml_type with
	  TyId id ->
		(* An unparameterized type -- these are merely looked up, unless 
		   they're type parameters, in which case we leave a reference for 
		   future substitution *)

		if List.mem id type_params 
		then Element (uqname_of_id name, TypeParamRef id)
		else let build_elt = lookup_type_by_id id in
			   build_elt name

	| ValApp (sub_type, _) -> simple_type_to_schema name sub_type type_params descr_table

	| TyApp (id, sub_types) -> 
		(* Type application -- we look up the definition and substitute *)

		let typefun = lookup_typefun_by_id id in
		let to_schema name simple_type = simple_type_to_schema name simple_type type_params descr_table in
		  typefun name sub_types to_schema

	| Table (id, _, _, keyt, _) -> 
		(* Type application -- we look up the definition and substitute *)

		let typefun = lookup_typefun_by_id (PadscId.makeid "Ptable") in
		let to_schema name simple_type = simple_type_to_schema name simple_type type_params descr_table in
		  (*TODO: not including keyt for now!!*)
		  typefun name [SimpleAst.TyId id] to_schema

	| Tuple comps ->
		(* A tuple type; these end up being treated like records *)
		
		let ctr = ref 1 in
		let next_elt_name () =
		  let name = "elt" ^ (string_of_int !ctr) in
			incr ctr;
			PadscId.makeid name
		in
		let comps_as_fields = List.map (fun comp -> (next_elt_name (), comp)) comps in
		  simple_type_to_schema name (Record comps_as_fields) type_params descr_table

	| Record fields -> 
		(* A record type; a sequence of children, along with a pd *)

		let children = 
		  if fields = [] 
		  then [Element (UQName "val", Empty)] (* empty records and tuples are treated like punit *)
		  else List.map 
			(fun (name, sub_type) -> simple_type_to_schema name sub_type type_params descr_table) 
			fields
		in
		  Element (uqname_of_id name, Sequence (List.append children [optional_pd]))

	| Datatype variants -> 
		(* A variant datatype; a choice of children, along with a pd *)

		let children = 
		  let child_schema (name, maybe_child) =
			match maybe_child with
				Some sub_type -> simple_type_to_schema name sub_type type_params descr_table
			  | None -> Element (uqname_of_id name, Empty)
		  in
		  List.map child_schema variants
		in
		  (* TODO fixme *)
		  Element (uqname_of_id name, Sequence [Choice children; optional_pd])

	| Constraint (sub_type, _, _) ->
		(* A constraint type; the sub type is followed by a parse descriptor *)
		
		let child = simple_type_to_schema name sub_type type_params descr_table in 
		  match child with
			  Element (name, child) -> Element (name, Sequence [child; optional_semanticpd])
			| ElementType (name, typename) -> ElementTypeExtension (name, typename, optional_semanticpd)
			| _ -> print_schema child; failwith "Unexpected constraint!"

(***********************************)
(* PADS_SCHEMA -> XSCHEMA COMPILER *)

let tag name attrs elts = Xml.Element (name, attrs, elts)

let attr name value = (name, value)

let rec pads_schema_to_xschema (schema : pads_schema) : Xml.xml list =
  let make_element_attrs name =
	match name with
		UQName name -> [attr "name" name]
	  | QName (ns, name) -> [ attr "name" name; attr "targetNamespace" ns ]
  in
  let resolve name =
	match name with
		UQName name -> QName (spec_prefix, name)
	  | QName _ -> name
  in
  let groupify schema =
	match schema with
		(* types that need wrapping *)
		Element _ 
	  | ElementType _
	  | ElementTypeExtension _
	  | ElementRef _ -> pads_schema_to_xschema (Sequence [schema])
		  (* types that don't need wrapping *)
	  | Empty -> []
	  | _ -> pads_schema_to_xschema schema
  in
  match schema with
	  Element (name, child) ->
		(* this wrapping makes sure that single-child elements still
		   get the sequence that they need and that empty types don't
		   generate empty complex types *)
		let wrapped_child = 
		  let group = groupify child in
			if group = []
			then []
			else [tag "xsd:complexType" [] group] 
		in
		  [tag "xsd:element" (make_element_attrs name) wrapped_child]
			
	| Sequence schemata ->
		let elts = List.concat (List.map pads_schema_to_xschema schemata) in
		  [tag "xsd:sequence" [] elts]
	| Choice schemata ->
		let elts = List.concat (List.map pads_schema_to_xschema schemata) in
		  [tag "xsd:choice" [] elts]
	| Repeat schema ->
		let [Xml.Element (name, attrs, elts)] = pads_schema_to_xschema schema in
		let attrs = (attr "minOccurs" "0")::(attr "maxOccurs" "unbounded")::attrs in
		  [tag name attrs elts]
	| Optional schema ->
		let [Xml.Element (name, attrs, elts)] = pads_schema_to_xschema schema in
		let attrs = (attr "minOccurs" "0")::(attr "maxOccurs" "1")::attrs in
		  [tag name attrs elts]
	| Empty -> []
	| ElementType (name, typename) -> 
		let typename = resolve typename in
		let attrs = make_element_attrs name in
		let attrs = (attr "type" (string_of_sname typename))::attrs in
		  [tag "xsd:element" attrs []]
	| ElementTypeExtension (name, typename, extension) ->
		let typename = resolve typename in
		let attrs = make_element_attrs name in
		let extension = groupify extension in
		  [tag "xsd:element" attrs
			  [tag "xsd:complexType" []
				  [tag "xsd:complexContent" []
					  [tag "xsd:extension" [attr "base" (string_of_sname typename)]
						  extension]]]]
	| ElementRef typename ->
		let attrs = [attr "ref" (string_of_sname typename)] in
		  [tag "xsd:element" attrs []]

let xschema_decl_to_type (name : string) (decl : Xml.xml) : Xml.xml =
  tag "xsd:complexType" 
	[attr "name" name] 
	[decl] (* shouldn't be xsd:element, since we generate decls from the top-level types, which strip out the top-level element *)

let top_level_element (name : string) : Xml.xml =
  tag "xsd:element"
	[attr "name" name; attr "type" (string_of_sname (QName (spec_prefix, name)))]
	[]

let build_xschema (schemata : usertype_registry_entry list) : Xml.xml =
  let process_top_level_type (id, schema) =
	let [xschema] = pads_schema_to_xschema schema in
	let name = PadscId.id2string id in
	  xschema_decl_to_type name xschema, top_level_element name
  in
  let (decls, elts) = List.split (List.map process_top_level_type !usertype_registry) in
  let schema_import = 
	tag "xsd:import"
	  [attr "namespace" pads_uri; attr "schemaLocation" "file:pads.xsd"]
	  []
  in
  let declarations = schema_import::(List.append elts decls) in
  let spec_uri = "file:" ^ !Common.global_source_file in
  let ns_attrs = 
	[
	  (* set up namespaces *)
	  attr "xmlns:xsd" "http://www.w3.org/2001/XMLSchema";
	  attr ("xmlns:" ^ pads_prefix) pads_uri;
	  attr ("xmlns:" ^ spec_prefix) spec_uri;
	  attr "targetNamespace" spec_uri;
	  (* put anonymous complexTypes into targetNamespace by default,
		 rather than the empty namespace *)
	  attr "elementFormDefault" "qualified"
	]
  in
	tag "xsd:schema" ns_attrs declarations
	  
(*************)
(* INTERFACE *)

let output_schema out =
  let xml = build_xschema !usertype_registry in
  let xml_as_string = Xml.to_string_fmt xml in
	output_string out xml_as_string;
	output_char out '\n';
	flush out

let gen dt current_descr tc loc decl =
  let (tp_params,name,val_param_opt,tp_eq_opt, tp_def) = decl in
  let ty = SimpleAstTx.trans dt current_descr tc loc tp_def in
  let type_schema = simple_type_to_schema name ty tp_params dt in
	(* register either as a basetype or typefun in the appropriate registry *)
	(if tp_params = []
	 then register_usertype name type_schema
	 else register_user_typefun name type_schema tp_params);
	([], ([], []), current_descr)
