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

(* Phone numbers.*)
ptype pn_t = pint64

ptype extended_zip = {
  zip : puint32;
  seperator : pstring_ME("#[-/ ]#");
  suffix: puint32
}

ptype pzip = 
  Extended_zip of extended_zip
| Small_zip of puint32
| Large_zip of pint64

ptype t_summary_header = {
  "0|"; tstamp: puint32
}

ptype summary_header = t_summary_header precord 

ptype dib_ramp = 
  Ramp of pint64 
| GenRamp of "no_ii" * pint64

ptype order_header = { 
     order_num      : puint32;  
'|'; att_order_num  : puint32;  
'|'; ord_version    : puint32;  
'|'; service_tn     : pn_t popt;
'|'; billing_tn     : pn_t popt;  
'|'; nlp_service_tn : pn_t popt;  
'|'; nlp_billing_tn : pn_t popt;  
'|'; zip_code       : pzip popt;  
'|'; ramp           : dib_ramp;  
'|'; order_type     : pstring('|');  
'|'; order_details  : puint32;
'|'; unused         : pstring('|');  
'|'; stream         : pstring('|'); 
'|'
} 

ptype event = {
  state: pstring('|'); '|'; 
  tstamp: puint32
}

ptype event_seq = event plist_re("/\\|/",peor) 

ptype t_entry = order_header * event_seq

ptype entry = t_entry precord

ptype entries = entry plist_np

ptype source = summary_header * entries
