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
##################################################################
# This makefile includes all of the core variables and targets needed
# for setting up a new pads/ml project. Assumes compilation to native
# code.
#
# This is a GNU makefile.
###################################################################

#########
# TODO
########
# 1. consider changing to model where we copy source files into biuld
# directory and then build there, rather than building in source
# dir. and then moving output. Would be simpler as wouldn't need to
# track which files generated and would also allow us to correctly use
# ocamldep by simply pointing to the build dir. (assuming the src
# files have already been copied).
# 2. After above, could use ocamldep for .pml as well.
# 3. Assumes that gen exists. Should remove this assumption.
###################################################################

# set the argument of the cd command appropriately
ifndef PML_HOME
  export PML_HOME=$(shell cd ../..; pwd)
endif

ifndef AST_ARCH
  export AST_ARCH=$(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
endif

BUILD := $(AST_ARCH)


# Tell make to search in build directory.
vpath %.cmi $(BUILD)
vpath %.cmo $(BUILD)
vpath %.cmx $(BUILD)
vpath %.o $(BUILD)
vpath %.cma $(BUILD)
vpath %.cmxa $(BUILD)
vpath %.a $(BUILD)

vpath %.ml $(SRC)
vpath %.mli $(SRC)

ifdef PROGRAM
  PROGRAM_BC := $(PROGRAM).bc
  PROGRAM_OPT := $(PROGRAM)
  PROGRAM_PROF_OPT := $(PROGRAM).prof
  vpath $(PROGRAM_OPT) $(PROGRAM_BC) $(PROGRAM_PROF_OPT) $(BUILD)
endif

ifdef LIBRARY
  LIBRARY_BC = $(LIBRARY).cma
  LIBRARY_OPT = $(LIBRARY).cmxa
  LIBRARY_OPT_CLIB = $(LIBRARY).a
  LIBRARY_PROF_OPT = $(LIBRARY)_p.cmxa
  LIBRARY_PROF_OPT_CLIB = $(LIBRARY)_p.a
endif

ifdef SOURCES
  MLI_SOURCES := $(filter %.mli, $(SOURCES))
  ML_SOURCES := $(filter %.ml, $(SOURCES))
  PML_SOURCES := $(filter %.pml, $(SOURCES))
  # Remove .pml files from sources so that ocamldep doesn't get confused
  SOURCES := $(MLI_SOURCES) $(ML_SOURCES)

  SOURCE_CMIS:=$(MLI_SOURCES:.mli=.cmi)
  # .cmis that are generated automatically from .ml.
  cmi_from_ml := $(ML_SOURCES:.ml=.cmi)
  SOURCE_AUTO_CMIS:=$(filter-out $(SOURCE_CMIS), $(cmi_from_ml))

  SOURCE_CMOS:=$(ML_SOURCES:.ml=.cmo)
  SOURCE_CMOS_D:=$(ML_SOURCES:.ml=_d.cmo)

  SOURCE_CMXS:=$(ML_SOURCES:.ml=.cmx)
  SOURCE_OBJS:=$(ML_SOURCES:.ml=.o)

  SOURCE_CMXS_P:=$(ML_SOURCES:.ml=_p.cmx)
  SOURCE_OBJS_P:=$(ML_SOURCES:.ml=_p.o)
endif

###################################################################
# Default values specifying directory structure of project.
# GEN_DIR = gen directory, relative to project root directory.
# GEN_P_DIR = GEN directory relative to pml directory.
###################################################################
ifndef SRC
  # Standard layout for projects will be to place the 
  # makefile with the source code.
  SRC=.
else
  # Tell make to search in source directory.
  vpath %.mli $(SRC)
  vpath %.ml $(SRC)
endif

# Setup directory structure related to PML files.
ifdef PML_SOURCES
ifndef P_DIR
  P_DIR=pml
endif
ifndef GEN_DIR
  GEN_DIR= gen
endif
ifndef GEN_P_DIR
  GEN_P_DIR= ../gen
endif
endif

ifndef DATA_DIR
  DATA_DIR=data
endif

ifdef TOOLS
  ifndef TOOL_DIR
    TOOL_DIR := $(PML_HOME)/examples/tools
  endif
  TOOL_LIB_DIR := $(TOOL_DIR)/$(BUILD)

  TOOL_CMIS:=$(foreach tool,$(TOOLS),$(TOOL_LIB_DIR)/$(tool).cmi)
  TOOL_CMOS:=$(foreach tool,$(TOOLS),$(TOOL_LIB_DIR)/$(tool).cmo)
  TOOL_CMXS:=$(foreach tool,$(TOOLS),$(TOOL_LIB_DIR)/$(tool).cmx)
endif

# Backwards-compatability. PML_DESCRS is now deprecated in favor of
# including .pml files in SOURCES, from which PML_SOURCES is derived.
ifdef PML_DESCRS
  PML_SOURCES += $(addsuffix .pml, $(PML_DESCRS))
endif

ifdef PML_SOURCES
  PML_DESCR_CMIS:=$(PML_SOURCES:.pml=.cmi)

  PML_DESCR_CMOS:=$(PML_SOURCES:.pml=.cmo)
  PML_DESCR_CMOS_D:=$(PML_SOURCES:.pml=_d.cmo)

  PML_DESCR_CMXS:=$(PML_SOURCES:.pml=.cmx)
  PML_DESCR_OBJS:=$(PML_SOURCES:.pml=.o)

  PML_DESCR_CMXS_P:=$(PML_SOURCES:.pml=_p.cmx)
  PML_DESCR_OBJS_P:=$(PML_SOURCES:.pml=_p.o)

  PML_DESCR_SOURCES:=$(foreach src,$(PML_SOURCES),$(GEN_DIR)/$(basename $(src)).ml)
endif

ifdef TOOL_DRIVER
  ifndef TOOL_DRIVER_MK
    TOOL_DRIVER_MK = GNUmakefile
  endif
  TOOL_DRIVER_CMI:=$(TOOL_DRIVER_DIR)/$(TOOL_DRIVER).cmi

  TOOL_DRIVER_CMO:=$(TOOL_DRIVER_DIR)/$(TOOL_DRIVER).cmo
  TOOL_DRIVER_CMO_D:=$(TOOL_DRIVER_DIR)/$(TOOL_DRIVER)_d.cmo

  TOOL_DRIVER_CMX:=$(TOOL_DRIVER_DIR)/$(TOOL_DRIVER).cmx
  TOOL_DRIVER_OBJ:=$(TOOL_DRIVER_DIR)/$(TOOL_DRIVER).o

  TOOL_DRIVER_CMX_P:=$(TOOL_DRIVER_DIR)/$(TOOL_DRIVER)_p.cmx
  TOOL_DRIVER_OBJ_P:=$(TOOL_DRIVER_DIR)/$(TOOL_DRIVER)_p.o
endif

include $(PML_HOME)/mk/common_vars.mk

###################################################################
# Set up includes.
###################################################################
INCLUDES += -I $(BUILD) -I $(PML_LIB_DIR)
ifdef ADD_INCLUDES
INCLUDES += -I $(ADD_INCLUDES)
endif

ifdef GEN_DIR
DEPINCLUDES += -I $(shell pwd)/$(GEN_DIR)
endif

ifdef TOOL_DIR
# Unfortunately, can't set this b/c ocamldep assumes that the compiled
# files will be in same directory as source files.
#   DEPINCLUDES += -I $(TOOL_LIB_DIR)
  INCLUDES += -I $(TOOL_LIB_DIR)
endif

ifdef TOOL_DRIVER
# Unfortunately, can't set this b/c ocamldep assumes that the compiled
# files will be in same directory as source files.
#   DEPINCLUDES += -I $(TOOL_DRIVER_DIR)/..
  INCLUDES += -I $(TOOL_DRIVER_DIR)
endif

###################################################################
# Set default values for variables used in building project target.
###################################################################

# Variables for byte-code compilation
OBJS_BC_def = $(PML_DESCR_CMOS) $(TOOL_DRIVER_CMO) $(SOURCE_CMOS)
OBJ_DEPS_BC_def = $(OBJS_BC)
LIBS_BC_def = unix.cma str.cma $(XML_LIB) $(PADSC_RUNTIME) $(PMLRUNTIME) $(PML_TOOLS_LIB) $(TOOL_CMOS)

ifndef OBJS_BC
OBJS_BC = $(OBJS_BC_def)
endif

ifndef OBJ_DEPS_BC
OBJ_DEPS_BC = $(OBJ_DEPS_BC_def)
endif

ifndef LIBS_BC
LIBS_BC = $(LIBS_BC_def)
endif

# Variables for native compilation (default settings)
OBJS_OPT_def = $(PML_DESCR_CMXS) $(TOOL_DRIVER_CMX) $(SOURCE_CMXS)
OBJ_DEPS_OPT_def = $(OBJS_OPT)  $(PML_DESCR_OBJS) $(SOURCE_OBJS)
LIBS_OPT_def = unix.cmxa str.cmxa $(XML_LIB_OPT) $(PADSC_RUNTIME_OPT) \
               $(PMLRUNTIME_OPT) $(PML_TOOLS_LIB_OPT) $(TOOL_CMXS)

ifndef OBJS_OPT
OBJS_OPT = $(OBJS_OPT_def)
endif

ifndef OBJ_DEPS_OPT
OBJ_DEPS_OPT = $(OBJ_DEPS_OPT_def)
endif

ifndef LIBS_OPT
LIBS_OPT = $(LIBS_OPT_def)
endif

# Variables for profiled native compilation
OBJS_PROF_OPT_def = $(PML_DESCR_CMXS_P) $(TOOL_DRIVER_CMX_P) $(SOURCE_CMXS_P)
OBJ_DEPS_PROF_OPT_def = $(OBJS_PROF_OPT)  $(PML_DESCR_OBJS_P) $(SOURCE_OBJS_P)
LIBS_PROF_OPT_def = $(PADSC_RUNTIME_PROF_OPT) $(PMLRUNTIME_PROF_OPT) str.cmxa $(XML_LIB_OPT) $(TOOL_CMXS)

ifndef OBJS_PROF_OPT
OBJS_PROF_OPT = $(OBJS_PROF_OPT_def)
endif

ifndef OBJ_DEPS_PROF_OPT
OBJ_DEPS_PROF_OPT = $(OBJ_DEPS_PROF_OPT_def)
endif

ifndef LIBS_PROF_OPT
LIBS_PROF_OPT = $(LIBS_PROF_OPT_def)
endif

###################################################################
# External targets
###################################################################

# make the PML compiler library
$(PMLC_LIB):
	$(MAKE) -C $(PMLC_DIR) $@

# make the PML runtime
$(PMLRUNTIME):
	$(MAKE) -C $(PMLRUNTIME_DIR) $@

$(TOOL_CMIS) $(TOOL_CMOS) $(TOOL_CMXS):
	$(MAKE) -C $(TOOL_DIR) $(notdir $@)

###################################################################
# Misc. targets
###################################################################

# Make sure that the BUILD directory exists before compiling.
# However, don't use it as a dependency -- (just because $(BUILD) is
# updated, doesn't mean we want the targets to be updated)
# THIS FEATURE DOES NOT SEEM TO WORK ON OLDER VERSIONS OF MAKE.
# $(PML_DESCR_CMIS) $(PML_DESCR_CMOS) $(PML_DESCR_CMXS) $(PML_DESCR_OBJS) : | $(BUILD)
# $(SOURCE_CMIS) $(SOURCE_CMOS) $(SOURCE_CMXS) $(SOURCE_OBJS): | $(BUILD)
# $(PROGAM) : | $(BUILD)

ifdef TOOL_DRIVER
$(TOOL_DRIVER_CMX): $(TOOL_CMIS)
$(TOOL_DRIVER_CMI) $(TOOL_DRIVER_CMO) $(TOOL_DRIVER_CMX):
	$(MAKE) -C $(TOOL_DRIVER_DIR)/.. -f $(TOOL_DRIVER_MK) $(notdir $@)
endif

###################################################################
# PADS/ML-description targets
###################################################################

# compile .pml to source code
$(PML_DESCR_SOURCES) : $(GEN_DIR)/%.ml: $(P_DIR)/%.pml $(PADSMLC) 
	$(PADSMLC) -o $(GEN_DIR)/$*.ml $(P_DIR)/$*.pml

# compile the .ml to a .cmo
$(PML_DESCR_CMOS): %.cmo :$(GEN_DIR)/%.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $(GEN_DIR)/$*.ml
	(if [ -f $(GEN_DIR)/$*.cmi ]; then \
	    mv $(GEN_DIR)/$*.cmi $(BUILD)/$*.cmi;	\
	  fi)
	@mv $(GEN_DIR)/$@ $(BUILD)/$@

# generate the .cmi from the .pml
$(PML_DESCR_CMIS): %.cmi :$(GEN_DIR)/%.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $(GEN_DIR)/$*.ml
	@mv $(GEN_DIR)/$*.cmi $(BUILD)/$*.cmi
	@(if [ -f $(GEN_DIR)/$*.cmo ]; then \
	    $(RM) $(GEN_DIR)/$*.cmo;	\
	  fi)

# compile the .pml to debug-version .cmo
$(PML_DESCR_CMOS_D): %_d.cmo :$(GEN_DIR)/%.ml
	$(OCAMLC) $(OCAMLFLAGS) -g -c $(GEN_DIR)/$*.ml
	@(if [ -f $(GEN_DIR)/$*.cmi ]; then \
	    mv $(GEN_DIR)/$*.cmi $(BUILD)/$*.cmi;	\
	  fi)
	@mv $(GEN_DIR)/$@ $(BUILD)/$*_d.cmo

###################################################################
# ML-source targets for sources
###################################################################

$(SOURCE_CMIS) : %.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<
	@mv $(dir $<)$@ $(BUILD)/$@

$(SOURCE_AUTO_CMIS): %.cmi: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<
	@mv $(dir $<)$@ $(BUILD)/$@
	@mv $(dir $<)$*.cmo $(BUILD)/$*.cmo

$(SOURCE_CMOS): %.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<
	@(if [ -f $(dir $<)$*.cmi ]; then \
	    mv $(dir $<)$*.cmi $(BUILD)/$*.cmi; \
	  fi)
	@mv $(dir $<)$@ $(BUILD)/$@;

$(SOURCE_CMOS_D): %_d.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -g -c $<
	@(if [ -f $(dir $<)$*.cmi ]; then \
	    mv $(dir $<)$*.cmi $(BUILD)/$*.cmi; \
	  fi)
	@mv $(dir $<)$*.cmo $(BUILD)/$*_d.cmo;

# Use implicit rule to enable both targets to share rule.
# This construction avoids executing the commands twice, once
# for the .cmx and once for the .o.

# For ml files derived from pads/ml files.
%.cmx %.o: $(GEN_DIR)/%.ml 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<
	@(if [ -f $(dir $<)$*.cmi ]; then \
	    mv $(dir $<)$*.cmi $(BUILD)/$*.cmi; \
	  fi)
	@mv $(dir $<)$*.cmx $(BUILD)/$*.cmx
	@mv $(dir $<)$*.o $(BUILD)/$*.o

%.cmx %.o: %.ml 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<
	@(if [ -f $(dir $<)$*.cmi ]; then \
	    mv $(dir $<)$*.cmi $(BUILD)/$*.cmi; \
	  fi)
	@mv $(dir $<)$*.cmx $(BUILD)/$*.cmx
	@mv $(dir $<)$*.o $(BUILD)/$*.o

# Use implicit rule to enable both targets to share rule.
# This construction avoids executing the commands twice, once
# for the .cmx and once for the .o.
%_p.cmx %_p.o: %.ml 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -p -c $<
	@(if [ -f $(dir $<)$*.cmi ]; then \
	    mv $(dir $<)$*.cmi $(BUILD)/$*.cmi; \
	  fi)
	@mv $(dir $<)$*.cmx $(BUILD)/$*_p.cmx
	@mv $(dir $<)$*.o $(BUILD)/$*_p.o

###################################################################
# Top-level targets
###################################################################
define MkBuildDir
(if [ ! -d $(BUILD) ] ; then \
    mkdir -p $(BUILD); \
    if ( grep "$(BUILD)" .cvsignore ); then \
      echo -n ""; \
    else \
      echo "$(BUILD)" >> .cvsignore; \
    fi; \
  fi)
endef

$(BUILD):
	$(MkBuildDir)

ifeq ($(PROJECT_TYPE),app)

all: $(BUILD) $(PROGRAM_OPT) 
prof: $(BUILD) $(PROGRAM_PROF_OPT)

else 

ifeq ($(PROJECT_TYPE),lib)

all : $(BUILD) $(LIBRARY_OPT) $(LIBRARY_OPT_CLIB) 
prof: $(BUILD) $(LIBRARY_PROF_OPT) $(LIBRARY_PROF_OPT_CLIB)

else

all: $(BUILD) $(OBJS_OPT)
prof: $(BUILD) $(OBJS_PROF_OPT)

endif
endif

ifeq ($(PROJECT_TYPE),app)

$(PROGRAM_BC) : $(OBJ_DEPS_BC) $(LIB_DEPS_BC)
	$(MkBuildDir)
	$(OCAMLC) $(OCAMLFLAGS) $(LIBS_BC) $(OBJS_BC) -o $(BUILD)/$@

$(PROGRAM_OPT) : $(OBJ_DEPS_OPT) $(LIB_DEPS_OPT)
	$(MkBuildDir)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(LIBS_OPT) $(OBJS_OPT) -o $(BUILD)/$@

$(PROGRAM_PROF_OPT) : $(OBJ_DEPS_PROF_OPT) $(LIB_DEPS_PROF_OPT)
	$(MkBuildDir)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -p $(LIBS_PROF_OPT) $(OBJS_PROF_OPT) -o $(BUILD)/$@
endif

ifeq ($(PROJECT_TYPE),lib)
$(LIBRARY_BC): $(OBJ_DEPS_BC) $(LIB_DEPS_BC)
	$(MkBuildDir)
	$(OCAMLC) $(OCAMLFLAGS) -o $(BUILD)/$@ -a $(OBJS_BC)

$(LIBRARY_OPT) $(LIBRARY_OPT_CLIB) : $(OBJ_DEPS_OPT) $(LIB_DEPS_OPT)
	$(MkBuildDir)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -verbose -o $(BUILD)/$(LIBRARY_OPT) -a $(OBJS_OPT)
# 	(lib=$(LIBRARY_OPT_CLIB); $(FixStaticLib))

$(LIBRARY_PROF_OPT) $(LIBRARY_PROF_OPT_CLIB) : $(OBJ_DEPS_PROF_OPT) $(LIB_DEPS_PROF_OPT)
	$(MkBuildDir)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -p -verbose -o $(BUILD)/$(LIBRARY_PROF_OPT) -a $(OBJS_PROF_OPT)

install-interfaces : $(BUILD) $(SOURCE_CMIS) $(SOURCE_AUTO_CMIS) 
	(cd $(BUILD); \
	 cp $(SOURCE_CMIS) $(SOURCE_AUTO_CMIS) $(INTF_INSTALL_DIR))

install: $(BUILD) install-interfaces $(LIBRARY_BC) $(LIBRARY_OPT) $(LIBRARY_OPT_CLIB)
	(cd $(BUILD); \
	 cp $(LIBRARY_BC) $(LIB_INSTALL_DIR);\
	 cp $(LIBRARY_OPT) $(LIB_INSTALL_DIR);\
	 cp $(LIBRARY_OPT_CLIB) $(LIB_INSTALL_DIR))

install-prof: $(BUILD) install-interfaces $(LIBRARY_PROF_OPT) $(LIBRARY_PROF_OPT_CLIB)
	(cd $(BUILD); \
	 cp $(LIBRARY_PROF_OPT) $(LIB_INSTALL_DIR);\
	 cp $(LIBRARY_PROF_OPT_CLIB) $(LIB_INSTALL_DIR))
endif

clean:
	(cd $(BUILD); $(RM) *.cmi *.cmx *.cmxa *.cmo *.o *_d.cmo)
	$(RM) $(BUILD)/.depend
	$(RM) gen/*.ml
ifdef PROGRAM
	$(RM) $(BUILD)/$(PROGRAM).bc
	$(RM) $(BUILD)/$(PROGRAM)
endif

veryclean: clean
	$(RM) *~
	$(RM) $(BUILD)/*

.PHONY: default all clean veryclean prof install install-interfaces install-prof

include $(PML_HOME)/mk/common_dep.mk

# SPECIAL_TARGETS := clean veryclean $(BUILD)
# ifneq ($(filter-out $(SPECIAL_TARGETS), $(MAKECMDGOALS)),)
    include .depend
# endif
