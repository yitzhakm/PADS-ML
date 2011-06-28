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

ifndef PML_HOME
  export PML_HOME=$(shell cd ..; pwd)
endif

ifndef AST_ARCH
  export AST_ARCH=$(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
endif

SRC=..
BUILD := $(AST_ARCH)

CURDIR := $(shell pwd)

ifneq ($(BUILD),$(notdir $(CURDIR)))
  include $(PML_HOME)/mk/redirect.mk
else

 # The following rules are run from the build directory

# PREPROC=-pp "camlp5o pa_extend.cmo q_MLast.cmo -loc loc "
PREPROC=-pp "camlp5o pa_extend.cmo q_MLast.cmo -mode T "
CAMLP5_DBG=camlp5 pa_o.cmo pa_op.cmo pa_extend.cmo q_MLast.cmo pr_o.cmo pr_op.cmo 

CAMLP5_CORE_LIB_NAMES= camlp5.cmxa pa_o.cmx 
CAMLP5_LIB_NAMES = $(CAMLP5_CORE_LIB_NAMES) pr_o.cmx
CAMLP5_DUMP_LIB_NAMES = $(CAMLP5_CORE_LIB_NAMES) pr_dump.cmx
CAMLP5_PATH = $(OCAML_LIB_DIR)/camlp5
CAMLP5_LIBS_OPT = $(foreach lib,$(CAMLP5_LIB_NAMES),$(CAMLP5_PATH)/$(lib))
CAMLP5_DUMP_LIBS_OPT = $(foreach lib,$(CAMLP5_DUMP_LIB_NAMES),$(CAMLP5_PATH)/$(lib))

LIBS_OPT=str.cmxa xml-light.cmxa

# core modules all have interfaces
CORE_MODULES=padscId Description baseTypeDescriptions padscError names ast hostLanguage simpleAst \
	common \
	simpleAstTx typeChecker typeGenerator sTypeGenerator \
	genPDGenerator parserGenerator traversalGenerator sTraversalGenerator \
	sLazyTraversalGenerator printerGenerator xSchemaCompiler metadataGenerator \
	sProducerGenerator sTypeRepGenerator

MODULES=$(CORE_MODULES) compiler padsML_pp padsML_grammar

NEW_MODULES=$(CORE_MODULES) compiler padsML_grammar

# this variable is used by the depend target in common.mk
SOURCES=$(foreach module,$(CORE_MODULES),$(module).mli) \
        $(foreach module,$(MODULES),$(module).ml)

INTERFACES=$(foreach module,$(MODULES),$(module).cmi)
OBJS=$(foreach module,$(MODULES),$(module).cmo)
OBJS_OPT=$(foreach module,$(MODULES),$(module).cmx)

NEW_OBJS=$(foreach module,$(NEW_MODULES),$(module).cmo)
NEW_OBJS_OPT=$(foreach module,$(NEW_MODULES),$(module).cmx)

PROG_OBJS=$(NEW_OBJS_OPT) driver.cmx
PROG=padsmlc

all : $(PROG)

.PHONY: all complib install padsml clean install-$(PROG) install-$(PROG)-pp

include $(PML_HOME)/mk/common.mk
include .depend

#INCLUDES += $(GALAX_LIB_INCLUDES)
#OCAMLFLAGS += -g

complib : $(PMLC_LIB) $(PMLC_LIB_OPT)
install: complib install-$(PROG) install-$(PROG)-pp

padsML_grammar_comp.ml: $(SRC)/padsML_grammar.ml
	$(CAMLP5_DBG) $< > $@

padsML_pp_comp.ml : $(SRC)/padsML_pp.ml
	$(CAMLP5_DBG) $< > $@

# install compiler library and interfaces
$(PMLC_LIB): $(OBJS)
	+@(if [ ! -d "$(PMLC_LIB_DIR)" ] ; then \
		mkdir -p $(PMLC_LIB_DIR); \
	  fi)
	$(OCAMLC) -a $(OBJS) -o $@
	cp $(INTERFACES) $(PML_LIB_DIR)

# install compiler library and interfaces
$(PMLC_LIB_OPT) $(PMLC_LIB_OPT_CLIB): $(OBJS_OPT)
	+@(if [ ! -d "$(PMLC_LIB_DIR)" ] ; then \
		mkdir -p $(PMLC_LIB_DIR); \
	  fi)
	$(OCAMLOPT) -a $(OBJS_OPT) -o $(PMLC_LIB_OPT)
	cp $(INTERFACES) $(PML_LIB_DIR)

# prog1 : $(PROG1_OBJS)
#         $(OCAMLC) -o prog1 $(OCAMLFLAGS) $(PROG1_OBJS)

# manual dependencies so that the drivers rebuild appropriately
padsML_pp.cmx driver.cmx driver-pp.cmx : $(NEW_OBJS_OPT)

$(PROG) : $(NEW_OBJS_OPT) driver.cmx
	$(OCAMLOPT) -o $(PROG) $(OCAMLOPTFLAGS) $(CAMLP5_LIBS_OPT)  $(LIBS_OPT) $(NEW_OBJS_OPT) driver.cmx

$(PROG)-pp : $(NEW_OBJS_OPT) driver-pp.cmx
	$(OCAMLOPT) -o $(PROG)-pp $(OCAMLOPTFLAGS) $(CAMLP5_DUMP_LIBS_OPT) $(LIBS_OPT) $(NEW_OBJS_OPT) driver-pp.cmx

install-$(PROG) : $(PROG)
	cp $(PROG) $(PML_HOME)/scripts/$(PROG)

install-$(PROG)-pp : $(PROG)-pp
	cp $(PROG)-pp $(PML_HOME)/scripts/$(PROG)-pp

# End of rules that are run from the build directory

# Clean up
clean:
	$(RM) *.cm[iox]
	$(RM) *.o
	$(RM) .depend
	$(RM) $(PROG) $(PROG)-pp
endif

# Clean up
veryclean:
	$(RM) *~ 
	$(RM) -r $(BUILD)

