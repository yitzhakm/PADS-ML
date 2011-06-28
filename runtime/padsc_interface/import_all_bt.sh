#! /bin/bash
########################################################################
#                                                                      #
#             This software is part of the padsml package              #
#           Copyright (c) 2006-2011 AT&T Knowledge Ventures            #
#                      and is licensed under the                       #
#                        Common Public License                         #
#                      by AT&T Knowledge Ventures                      #
#                                                                      #
#                A copy of the License is available at                 #
#                    www.padsproj.org/License.html                     #
#                                                                      #
#  This program contains certain software code or other information    #
#  ("AT&T Software") proprietary to AT&T Corp. ("AT&T").  The AT&T     #
#  Software is provided to you "AS IS". YOU ASSUME TOTAL RESPONSIBILITY#
#  AND RISK FOR USE OF THE AT&T SOFTWARE. AT&T DOES NOT MAKE, AND      #
#  EXPRESSLY DISCLAIMS, ANY EXPRESS OR IMPLIED WARRANTIES OF ANY KIND  #
#  WHATSOEVER, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF#
#  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, WARRANTIES OF  #
#  TITLE OR NON-INFRINGEMENT.  (c) AT&T Corp.  All rights              #
#  reserved.  AT&T is a registered trademark of AT&T Corp.             #
#                                                                      #
#                   Network Services Research Center                   #
#                          AT&T Labs Research                          #
#                           Florham Park NJ                            #
#                                                                      #
#            Yitzhak Mandelbaum <yitzhak@research.att.com>             #
#                                                                      #
########################################################################

if [ $1 = "--clean" ] ; then
    btSpecs=$2
    btDir=$3
    
    (read ptype idl_name rest
	while [ -n "$ptype" ]; do
	    if [ -z `echo $ptype | grep "^#"` ] ; then
	      ./import_bt.sh --clean $idl_name
            fi;
	    read ptype idl_name rest
	    done) < $btSpecs;
    
    exit 0;
fi

btSpecs=$1
btDir=$2

# cleanup first
(read ptype idl_name rest
while [ -n "$ptype" ]; do
    if [ -z `echo $ptype | grep "^#"` ] ; then
      ./import_bt.sh --clean $idl_name
    fi;
    read ptype idl_name rest
done) < $btSpecs

# do actual import
(read ptype idl_name rep custom_call add_params;
while [ -n "$ptype" ]; do
    if [ -z `echo $ptype | grep "^#"` ] ; then
      ./import_bt.sh $ptype $idl_name $rep $custom_call "$add_params"
    fi;
    read ptype idl_name rep custom_call add_params;
done) < $btSpecs

exit 0
