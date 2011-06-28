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

default : all

# set the argument of the cd command appropriately
ifndef PML_HOME
  export PML_HOME=$(shell cd ../..; pwd)
endif

ifndef AST_ARCH
  export AST_ARCH=$(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
endif

CURDIR := $(shell pwd)

SRC=..
BUILD := $(AST_ARCH)
P_DIR=../../pml

# Tools: must have an explicit .mli file
TOOLS=debug_tool xml_formatter \
	cisco_xml_formatter juniper_xml_formatter ciscoIOS_xml_formatter ciscogen_xml_formatter \
	flatten_tool universal_inject_tool

TOOL_CMOS=$(foreach tool,$(TOOLS),$(tool).cmo)
TOOL_CMXS=$(foreach tool,$(TOOLS),$(tool).cmx)
TOOL_CMIS=$(foreach tool,$(TOOLS),$(tool).cmi)

ifneq ($(BUILD),$(notdir $(CURDIR)))
  include $(PML_HOME)/mk/redirect.mk
else

 # The following rules are run from the build directory

.PHONY: all default clean $(TOOLS) acctool


all:  $(TOOLS)

include $(PML_HOME)/mk/common.mk

ifdef DEBUG
OCAMLFLAGS += -g
endif

PREPROC=
INCLUDES+=-I $(PML_LIB_DIR)

# make the PML runtime
$(PMLRUNTIME):
	$(MAKE) -C $(PMLRUNTIME_DIR) $@

$(TOOL_CMOS): %.cmo :$(SRC)/%.ml $(PMLRUNTIME)
	$(OCAMLC) $(OCAMLFLAGS) -c $<
	@(if [ -f $(SRC)/$*.cmi ]; then \
	    mv $(SRC)/$*.cmi .;	\
	  fi)
	@mv $(SRC)/$*.cmo .

$(TOOL_CMXS): %.cmx :$(SRC)/%.ml $(PMLRUNTIME_OPT)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<
	@(if [ -f $(SRC)/$*.cmi ]; then \
	    mv $(SRC)/$*.cmi .;	\
	  fi)
	@mv $(SRC)/$*.cmx .
	@mv $(SRC)/$*.o .

$(TOOL_CMIS): %.cmi :$(SRC)/%.ml $(PMLRUNTIME) $(PML_LIB_DIR)/pads.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $<
	@(if [ -f $(SRC)/$*.cmi ]; then \
	    mv $(SRC)/$*.cmi .;	\
	  fi)
	@mv $(SRC)/$*.cmo .

# universal_inject_tool.cmo : universal_inject_tool.cmi
# universal_inject_tool.cmi : universal_inject_tool.mli $(PML_LIB_DIR)/pads.cmi
# 	$(OCAMLC) $(OCAMLFLAGS) -c $<
# 	@mv $(SRC)/$@ .

# Build a tool that doesn't have a .mli file
$(TOOLS): % :%.cmi %.cmo %.cmx

acctool : 

clean:
	rm -f *.cm[iox]	

# End of rules that are run from the build directory
endif
