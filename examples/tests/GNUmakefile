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

# default target 

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
DEBUG_DIR=debug
EXAMPLES_DIR=$(PML_HOME)/examples
P_DIR=$(EXAMPLES_DIR)/pml
GEN_DIR=$(EXAMPLES_DIR)/gen
DATA_DIR := $(EXAMPLES_DIR)/data

DESCRS= datatype_implicit intTriple intlist salary_list table \
	newick record regulus sirius to_do_list wsl match_dt color switch 
# MG: the following tests don't work:
# regulus (error in xSC)
# the following tests are missing .pml files
# numbers 
# the following tests are missing .txt files
# ganglia

ifdef MKCISCO
DESCRS += patterns arg_map perlre cisco
endif

DESCR_CMIS:=$(foreach descr,$(DESCRS),$(descr).cmi)
DESCR_CMOS:=$(foreach descr,$(DESCRS),$(descr).cmo)
DESCR_CMOS_D:=$(foreach descr,$(DESCRS),$(DEBUG_DIR)/$(descr).cmo)
DESCR_CMXS:=$(foreach descr,$(DESCRS),$(descr).cmx)
DESCR_OBJS:=$(DESCR_CMXS:.cmx=.o)
TEST_SOURCES:=$(foreach descr,$(DESCRS),test_$(descr).ml)
TEST_CMXS:=$(TEST_SOURCES:.ml=.cmx)
TEST_PROGS:=$(foreach descr,$(DESCRS),test_$(descr))
TEST_PROGS_DEBUG:=$(foreach descr,$(DESCRS),test_$(descr)_d)
RDONLY_PROGS:=$(foreach descr,$(DESCRS),rdonly_$(descr))
DESCR_SOURCES:=$(foreach descr,$(DESCRS),$(GEN_DIR)/$(descr).ml)

PARSE_SOURCES:=$(foreach descr,$(DESCRS),parse_$(descr).ml)
PARSE_CMXS:=$(PARSE_SOURCES:.ml=.cmx)
PARSE_PROGS:=$(foreach descr,$(DESCRS),parse_$(descr))

# DESCR_SCMOS:=$(foreach descr,$(DESCRS),$(GEN_DIR)/$(descr).cmo)
# DESCR_SCMXS:=$(foreach descr,$(DESCRS),$(GEN_DIR)/$(descr).cmx)
# DESCR_SOBJS:=$(foreach descr,$(DESCRS),$(GEN_DIR)/$(descr).o)

DESCR_REGRESSIONS:=$(foreach descr,$(DESCRS),regress_$(descr))

# Use this only for creating new regression output. For new descriptions, also
# be sure to add regression output to CVS.
MAKE_REGRESSIONS:=$(foreach descr,$(DESCRS),make_regress_$(descr))

ifneq ($(BUILD),$(notdir $(CURDIR)))
  include $(PML_HOME)/mk/redirect.mk
else

 # The following rules are run from the build directory

.PHONY: all tests clean $(DESCRS)
.PHONY: regress_all $(DESCR_REGRESSIONS) $(MAKE_REGRESSIONS)

all: $(TEST_PROGS)

run_all: $(DESCRS)

regress: $(DESCR_REGRESSIONS)

$(DESCR_REGRESSIONS): regress_% : test_%
	./test_$* ../../data/$*.txt 2>&1 | [ "`diff -q /dev/stdin ../../regress/$*.out`" = "" ]

$(MAKE_REGRESSIONS): make_regress_% : test_%
	./test_$* ../../data/$*.txt > ../../regress/$*.out  2>&1

$(DESCRS): % : test_%
	./test_$* ../../data/$*.txt 

include $(PML_HOME)/mk/common.mk

INCLUDES+=-I $(PML_LIB_DIR) -I $(OCAML_LIB_DIR) -I .

# make the PML compiler library
$(PMLC_LIB):
	$(MAKE) -C $(PMLC_DIR) $@

# make the PML runtime
$(PMLRUNTIME):
	$(MAKE) -C $(PMLRUNTIME_DIR) $@

# compile .pml to source code
$(DESCR_SOURCES) : $(GEN_DIR)/%.ml : $(P_DIR)/%.pml $(PADSMLC)
	$(PADSMLC) -o $(GEN_DIR)/$*.ml $(P_DIR)/$*.pml

# compile source version of example
$(DESCR_CMOS): %.cmo : $(GEN_DIR)/%.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $(GEN_DIR)/$*.ml
	(if [ -f $(GEN_DIR)/$*.cmi ]; then \
	    mv $(GEN_DIR)/$*.cmi .;	\
	  fi)
ifdef DEBUG
	@mv $< $(DEBUG_DIR)
else
	@mv $< .
endif

# compile optimized source version of example
$(DESCR_CMXS) : %.cmx  : $(GEN_DIR)/%.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $(GEN_DIR)/$*.ml
	(if [ -f $(GEN_DIR)/$*.cmi ]; then \
	    mv $(GEN_DIR)/$*.cmi .;	\
	  fi)
ifdef DEBUG
	mv $(GEN_DIR)/$*.cmx $(DEBUG_DIR)
	mv $(GEN_DIR)/$*.o $(DEBUG_DIR)
else
	mv $(GEN_DIR)/$*.cmx .
	mv $(GEN_DIR)/$*.o .
endif

# make byte-code version of a test program
# $(TEST_PROGS): test_%: test_%.cmx %.cmo $(TOOLS_CMXS) $(PMLRUNTIME) tester.cmo tester.cmi
# 	$(OCAMLC) $(OCAMLFLAGS) $(PMLRUNTIME) $(XML_LIB) tester.cmo \
# 	$(TOOLS_CMOS) $*.cmo $< -o $@

# make sure the spec is compiled before compiling the driver -- not
# sure why we need this, but we do
$(TEST_CMXS): test_%.cmx : %.cmx %.o

# str.cmxa is needed by some tools. e.g. cisco_xml_formatter
$(TEST_PROGS): test_% : test_%.cmx %.cmx %.o $(PMLRUNTIME_OPT) 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) unix.cmxa str.cmxa $(XML_LIB_OPT) \
	$(PADSC_RUNTIME_OPT) $(PMLRUNTIME_OPT) $(PML_TOOLS_LIB_OPT) \
        $*.cmx $< -o $@

# make sure the spec is compiled before compiling the driver -- not
# sure why we need this, but we do
$(PARSE_CMXS): parse_%.cmx : %.cmx %.o

# str.cmxa is needed by some tools. e.g. cisco_xml_formatter
$(PARSE_PROGS): parse_% : parse_%.cmx %.cmx %.o $(PMLRUNTIME_OPT) 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) unix.cmxa str.cmxa $(XML_LIB_OPT) \
	$(PADSC_RUNTIME_OPT) $(PMLRUNTIME_OPT) $(PML_TOOLS_LIB_OPT) \
        $*.cmx $< -o $@

################################################################################
# Other stuff below
################################################################################

proc_perlre.cmx : perlre.o perlre.cmx perlre.cmi arg_map.cmx arg_map.cmi

# make optimized version of perlre program
proc_perlre: PREPROC=
proc_perlre: proc_perlre.cmx 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) str.cmxa $(PADSC_RUNTIME_OPT) $(PMLRUNTIME_OPT) \
	 $(XML_LIB_OPT) $(PML_TOOLS_LIB_OPT) \
	 arg_map.cmx perlre.cmx $< -o $@

# generate cisco description

$(P_DIR)/cisco.pml : test_patterns proc_perlre $(DATA_DIR)/parser-pattern.txt $(DATA_DIR)/parser-arg_map.txt
	./test_patterns $(DATA_DIR)/parser-pattern.txt | ./proc_perlre $(DATA_DIR)/parser-arg_map.txt > $@

# make a test program with debug info.
# str.cma is needed by some tools. e.g. cisco_xml_formatter
# BROKEN rule. Needs to be fixed.
# $(TEST_PROGS_DEBUG): test_%_d: test_%.cmo $(DEBUG_DIR)/%.cmo $(TOOLS_CMOS) \
# 			$(PMLRUNTIME) $(DEBUG_DIR)/tester.cmo tester.cmi
# 	$(OCAMLC) $(OCAMLFLAGS) -g -verbose str.cma $(PMLRUNTIME) $(XML_LIB) \
# 	$(TOOLS_CMOS) $(DEBUG_DIR)/tester.cmo $(DEBUG_DIR)/$*.cmo $< -o $@

# make optimized version of read-only program
$(RDONLY_PROGS): rdonly_%: rdonly_%.cmx $(GEN_DIR)/%.o $(GEN_DIR)/%.cmx $(PMLRUNTIME_OPT) $(PML_TOOLS_LIB_OPT)
	@(if [ -f $(GEN_DIR)/$*.cmi ]; then \
	    mv $(GEN_DIR)/$*.cmi .;	\
	  fi)
	@mv $(GEN_DIR)/$*.cmx .
	@mv $(GEN_DIR)/$*.o .
	$(OCAMLOPT) $(OCAMLOPTFLAGS) str.cmxa $(XML_LIB_OPT) \
	$(PADSC_RUNTIME_OPT) $(PMLRUNTIME_OPT) $(PML_TOOLS_LIB_OPT) \
	$*.cmx $< -o $@

compact_to_do_list : $(SRC)/compact_to_do_list.ml to_do_list.o to_do_list.cmx $(PMLRUNTIME_OPT)
	$(OCAMLOPT) -verbose  $(OCAMLOPTFLAGS) $(XML_LIB_OPT) \
	 $(PADSC_RUNTIME_OPT) $(PMLRUNTIME_OPT) $(PML_TOOLS_LIB_OPT) \
	to_do_list.cmx $< -o $@


clean:
	$(RM) *.cm[iox]	
	$(RM) *.o
	$(RM) test_*
	$(RM) $(GEN_DIR)/*.cm[iox] $(GEN_DIR)/*.o $(GEN_DIR)/*.ml

# End of rules that are run from the build directory
endif
