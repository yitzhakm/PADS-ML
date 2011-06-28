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

# set the argument of the cd command appropriately
ifndef PML_HOME
  export PML_HOME=$(shell cd ../..; pwd)
endif

CURDIR := $(shell pwd)
GEN_DIR= ../gen

# Simple descriptions that exercise only a few features each.
DESCRS := datatype_implicit intTriple intlist table salary_list \
          match_dt poly record reclist tuple

# Real data format descriptions.
DESCRS += newick regulus sirius to_do_list wsl ganglia color switch numbers

ifdef MKCISCO
DESCRS += patterns arg_map perlre cisco
endif

DESCR_CMOS=$(foreach descr,$(DESCRS),$(descr).cmo)

DESCR_SOURCES=$(foreach descr,$(DESCRS),$(GEN_DIR)/$(descr).ml)
DESCR_CMOS_FROM_SOURCES=$(foreach descr,$(DESCRS),$(GEN_DIR)/$(descr).cmo)

.PHONY: all clean

all: $(DESCR_CMOS)

include $(PML_HOME)/mk/common.mk

ifdef DEBUG
OCAMLFLAGS += -g
endif

INCLUDES+=-I $(PML_LIB_DIR)

# make the PML compiler library
$(PMLC_LIB):
	$(MAKE) -C $(PMLC_DIR) $@

# make the PML runtime
$(PMLRUNTIME):
	$(MAKE) -C $(PMLRUNTIME_DIR) $@

# generate source code from the description
$(DESCR_SOURCES) : $(GEN_DIR)/%.ml: %.pml $(PMLC_LIB)
	camlp4o  -I . -I $(PMLC_LIB_DIR) $(PMLC_LIB) pr_o.cmo  -impl $< -o $@ 

# compile the .pml to a .cmo
$(DESCR_CMOS): PREPROC=-pp "camlp4o -I $(PMLC_LIB_DIR) $(PMLC_LIB) -impl "
$(DESCR_CMOS): %.cmo :%.pml $(PMLC_LIB) $(PMLRUNTIME)
	$(OCAMLC) $(OCAMLFLAGS) $(PREPROC) -c -impl $<

# compile source version of example
$(DESCR_CMOS_FROM_SOURCES): %.cmo : %.ml $(PMLC_LIB) $(PMLRUNTIME)
	$(OCAMLC) $(OCAMLFLAGS) -c $*.ml

clean:
	$(RM) ./*~ ./*.cm[io] 
	$(RM) $(GEN_DIR)/*.cm[io] $(GEN_DIR)/*.ml $(GEN_DIR)/*.ml~
