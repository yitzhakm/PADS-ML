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
(** The initial type descriptor table *)

module D = Description
type metadata = D.metadata = {
  name : PadscId.id; (** The (visible) name of the description. *)
  kind : D.kind;
  tyclass : D.tyclass;
}

let base_descriptions = [{name=PadscId.makeid "Pint";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pint8";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pint16";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pint32";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pint64";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Puint8";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Puint16";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Puint32";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pint8_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pint16_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pint32_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pint64_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Puint8_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Puint16_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Puint32_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pb_int8";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pb_int16";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pb_int32";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pb_int64";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pb_uint8";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pb_uint16";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pb_uint32";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pbool";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pchar";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pfloat32";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pfloat64";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pstring";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pstring_lit";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pip";kind=D.Base;tyclass=D.Defined;};
                         (*{name=PadscId.makeid "Psbh_ip";kind=D.Base;tyclass=D.Defined;};
                         {name=PadscId.makeid "Pb_timestamp";kind=D.Base;tyclass=D.Defined;};*)
			 {name=PadscId.makeid "Ptimestamp_explicit_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pstring_FW";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pstring_ME";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Pstring_SE";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Peor";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Peof";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "PIP";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Popt";kind=D.Fun [
			    {name=PadscId.freshid "arg";kind=D.Base;tyclass=D.Ty_var}
			  ];tyclass=D.Defined;};
			 {name=PadscId.makeid "Poption";kind=D.Fun [
			    {name=PadscId.freshid "arg";kind=D.Base;tyclass=D.Ty_var}
			  ];tyclass=D.Defined;};
			 {name=PadscId.makeid "Ptable";kind=D.Fun [
			    {name=PadscId.freshid "arg";kind=D.Base;tyclass=D.Ty_var}
			  ];tyclass=D.Defined;};
			 {name=PadscId.makeid "Plist";kind=D.Fun [
			    {name=PadscId.freshid "arg";kind=D.Base;tyclass=D.Ty_var}
			  ];tyclass=D.Defined;};
			 {name=PadscId.makeid "Pstream";kind=D.Fun [
			    {name=PadscId.freshid "arg";kind=D.Base;tyclass=D.Ty_var}
			  ];tyclass=D.Defined;};
			 {name=PadscId.makeid "Plist_ch";kind=D.Fun [
			    {name=PadscId.freshid "arg";kind=D.Base;tyclass=D.Ty_var}
			  ];tyclass=D.Defined;};
			 {name=PadscId.makeid "Plist_re";kind=D.Fun [
			    {name=PadscId.freshid "arg";kind=D.Base;tyclass=D.Ty_var}
			  ];tyclass=D.Defined;};
			 {name=PadscId.makeid "Plist_st";kind=D.Fun [
			    {name=PadscId.freshid "arg";kind=D.Base;tyclass=D.Ty_var}
			  ];tyclass=D.Defined;};
			 {name=PadscId.makeid "Plist_nosep";kind=D.Fun [
			    {name=PadscId.freshid "arg";kind=D.Base;tyclass=D.Ty_var}
			  ];tyclass=D.Defined;};
			 {name=PadscId.makeid "Plist_np";kind=D.Fun [
			    {name=PadscId.freshid "arg";kind=D.Base;tyclass=D.Ty_var}
			  ];tyclass=D.Defined;};
			 {name=PadscId.makeid "Plist_longest";kind=D.Fun [
			    {name=PadscId.freshid "arg";kind=D.Base;tyclass=D.Ty_var}
			  ];tyclass=D.Defined;};
			 {name=PadscId.makeid "Plist_longest_pred";kind=D.Fun [
			    {name=PadscId.freshid "arg";kind=D.Base;tyclass=D.Ty_var}
			  ];tyclass=D.Defined;};
			 {name=PadscId.makeid "Precord";kind=D.Fun [
			    {name=PadscId.freshid "arg";kind=D.Base;tyclass=D.Ty_var}
			  ];tyclass=D.Defined;};
			 {name=PadscId.makeid "Ptry";kind=D.Fun [
			    {name=PadscId.freshid "arg";kind=D.Base;tyclass=D.Ty_var}
			  ];tyclass=D.Defined;};
			 {name=PadscId.makeid "Ptransform";
			  kind=D.Fun 
			     [{name=PadscId.makeid "source";kind=D.Base;tyclass=D.Ty_var};
			      {name=PadscId.makeid "target";kind=D.Base;tyclass=D.Ty_var}];
			  tyclass=D.Defined;};
			 {name=PadscId.makeid "Pcommit";kind=D.Base;tyclass=D.Defined;};
			 {name=PadscId.makeid "Punit";kind=D.Base;tyclass=D.Defined;}]

let base_type_table = List.fold_left (fun t d -> D.add_descr t d) D.new_table base_descriptions
