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
#                 Robert Gruber <bob.gruber@gmail.com>                 #
#                                                                      #
########################################################################
# This is a GNU makefile.

###################################################################
# Default target                                                  
###################################################################

default : all

###################################################################
# Setup standard variable bindings
###################################################################
ifndef PML_HOME
  export PML_HOME=$(shell cd ../..; pwd)
endif

include $(PML_HOME)/mk/common_vars.mk

###################################################################
# Customize variables to this makefile.
###################################################################
INCLUDES+=-I $(PML_LIB_DIR)

###################################################################
#  Targets
###################################################################

all : pml

install : pml $(PML_BIN_DIR) 
	cp pml $(PML_BIN_DIR)

$(PML_BIN_DIR) :
	@(if [ ! -d $(PML_BIN_DIR) ] ; then \
	    mkdir -p $(PML_BIN_DIR); \
	  fi)

###################################################################
# Custom top-level w/ padsml compiler loaded.
# Invocation:
# pml -I $(PML_LIB_DIR) objects ...
###################################################################
# $(PML_HOME)/examples/tools/$(AST_ARCH)/debug_tool.cmo \
TESTER_CMOS := $(PML_HOME)/examples/tools/$(AST_ARCH)/xml_formatter.cmo \
	$(PML_HOME)/examples/tests/$(AST_ARCH)/tester.cmo

pml : $(XML_LIB) $(PADSC_RUNTIME) $(PMLRUNTIME) $(PMLC_LIB) $(TESTER_CMOS)
	ocamlmktop -o $@ -I +camlp5 unix.cma str.cma camlp5o.cma \
	$^

# eliminate binaries and object files
clean:
	$(RM) pml
	$(RM) *.cm[ioxa]
	$(RM) *.cmxa
	$(RM) *.o
	$(RM) .depend

# eliminate all generated files
veryclean: clean

.PHONY: default all install clean veryclean
