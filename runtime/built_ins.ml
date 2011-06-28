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
let peor = "/$/"

(* Built-in type constructors. Base types do not need to be included,
   as they are accessible automatically by virtue of being in the lib
   directory. However, the type constructors modules have modules
   within them, so they need to be included in order for those modules
   to be directly accesible.

   This design goes back to when type constructors were implemented
  with functors, leaving no way to introduce them directly into the
  namespace. However, given the current design, whereby no functors
  are involved, we could (and should) use a uniform approach to base
  types and type constructors.  *)

include Plist_ch
include Plist_re
include Plist_st
include Plist_nosep
include Plist_np
include Plist_longest
include Plist_longest_pred
include Plist_dt
include Ptable

include Pstream

include Ptry
include Precord

include Popt
type 'a poption = 'a popt
type 'a_pdb poption_pd_body = 'a_pdb popt_pd_body
module  Poption = Popt

include Ptransform

