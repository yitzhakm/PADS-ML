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

ptype hn_part = pstring_SE("/[. ]/") 
ptype phostname =  hn_part plist_nosep(".")

ptype pip = pint plist_nosep(".")

ptype client_t = 
    Ip of pip         (* 135.207.23.32 *)
  | Host of phostname (* www.research.att.com *)

ptype auth_id_t =
    Unauthorized of '-'
  | Id of pstring(' ')

ptype method_t =
    Get    of "GET"
  | Put    of "PUT"
  | Post   of "POST"
  | Head   of "HEAD"
  | Delete of "DELETE"
  | Link   of "LINK"
  | Unlink of "UNLINK"

ptype version_t = {
    "HTTP/";
    major : puint8;
    '.';
    minor : puint8
  }

let chk_version = function
    ({major = 1; minor = 1},_) -> true
  | (_, Link) -> false
  | (_, Unlink) -> false
  | _ -> true  

ptype request_t = {
  '\"';  meth : method_t;     
  ' ';   req_uri : pstring(' ');
  ' ';   version : [v : version_t | chk_version(v, meth)]; 
  '\"'
}

ptype response_t = [x : puint16_FW(3) | 100 <= x && x < 600]

ptype length_t = 
    Unavailable of '-'
  | Len of pint

ptype entry_t = {
         client : client_t;
   ' ';  remoteID : auth_id_t;
   ' ';  auth : auth_id_t;
(*    " ["; date : pdate(']'); *)
   " ["; date : pstring(']');
   "] "; request : request_t;
   ' ';  response : response_t;
   ' ';  length : length_t
}

ptype clt_t = entry_t precord plist(No_sep, No_term)

(* ptype clt_t = entry_t precord * entry_t precord*)

ptype source = clt_t
