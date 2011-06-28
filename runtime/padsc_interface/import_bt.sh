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
# calling semantics:
# import_bt.sh --clean <IDL filename seed>
# import bt.sh <PADS type> <idl filename seed> [<rep type> [<additional C params> ]]

baseTypeList=base_types.mk

if [ $1 = "--clean" ] ; then
    FILE_SEED=$2
    IDL_FILE=p"$FILE_SEED"_c.idl
    WRAPPER_FILE=p"$FILE_SEED"_c_stubs_w.c

    if [ -f $IDL_FILE ] ; then 
	echo "Removing IDL file" $IDL_FILE 
	rm -f $IDL_FILE;
    fi

    if [ -f $WRAPPER_FILE ] ; then 
	rm -f $WRAPPER_FILE;
	echo "Removing C file" $WRAPPER_FILE
    fi
    exit;
fi

TYPE=$1

FILE_SEED=$2
IDL_FILE=p"$FILE_SEED"_c.idl
WRAPPER_FILE=p"$FILE_SEED"_c_stubs_w.c

if [ ! $3 = "-" ] ; then
    REP_TYPE=$3;
else
    REP_TYPE=$TYPE;
fi
if [ ! "$4" = "-" ] ; then
    CUSTOM_CALL=$4;
fi
if [ ! "$5" = "-" ] ; then
    ADD_PARAMS=$5;
fi

echo "Importing:" $TYPE
echo "Using representation:" $REP_TYPE
echo -n "Additional parameters included: "
if [ "$ADD_PARAMS" ] ; then
  echo Y;
else
  echo N;
fi

# ADD BASE TYPE TO LIST OF BASE TYPES

if [ ! -f $IDL_FILE ] ; then 
    echo "Adding p$FILE_SEED to base-type list."    
    echo "N_BASE_TYPES += p$FILE_SEED" >> $baseTypeList;
fi

# CREATE/EXTEND IDL FILE

# generate header if file does not exist.
if [ ! -f $IDL_FILE ] ; then 
    echo "Creating IDL file" $IDL_FILE 
    echo 'import "padsc.idl";' > $IDL_FILE
    echo >> $IDL_FILE;
else
    echo "Extending IDL file" $IDL_FILE 
fi

# generate read decl.
echo 'Perror_t '$TYPE'_read([in,ptr] P_t *pads,' >> $IDL_FILE 
echo '  [in,ref] const Pbase_m *m,'  >> $IDL_FILE 
echo '  [out,ref] Pbase_pd *pd,'  >> $IDL_FILE 
echo '  [out,ref]' $REP_TYPE '*res_out'  >> $IDL_FILE  
if [ "$ADD_PARAMS" ] ; then
    echo ',' $ADD_PARAMS  >> $IDL_FILE;
fi
echo -n ')'   >> $IDL_FILE;
if [ "$CUSTOM_CALL" ] ; then
    echo ' quote(call,"'$CUSTOM_CALL'");' >> $IDL_FILE;    
else
    echo ';'  >> $IDL_FILE;
fi

# generate write decl.
echo 'ssize_t '$TYPE'_write2io([in,ptr] P_t *pads, [in] SfioPtr io, [in,ref] Pbase_pd *pd,'   >> $IDL_FILE  
echo '[in,ref] '$REP_TYPE' *d' >> $IDL_FILE;
if [ "$ADD_PARAMS" ] ; then
    echo ',' $ADD_PARAMS  >> $IDL_FILE;
fi
echo ');'  >> $IDL_FILE

# CREATE C WRAPPER FILE

if [ ! -f $WRAPPER_FILE ] ; then 
    echo "Creating C file" $WRAPPER_FILE
    echo '#include "padsc.h"' > $WRAPPER_FILE
    echo '#include "p'$FILE_SEED'_c_stubs.c"' >> $WRAPPER_FILE;
fi
