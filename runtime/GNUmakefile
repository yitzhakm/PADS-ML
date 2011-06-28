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

ifndef PML_HOME
  export PML_HOME=$(shell cd ..; pwd)
endif

ifndef AST_ARCH
  export AST_ARCH=$(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
endif

SRC=..

BUILD := $(AST_ARCH)

CURDIR := $(shell pwd)
SUBDIRS := padsc_interface
SUBDIRS_VC := $(foreach subdir,$(SUBDIRS),$(subdir).vc)

ifneq ($(BUILD),$(notdir $(CURDIR)))
  BASE_TYPE_DIR := $(CURDIR)
else
  BASE_TYPE_DIR := $(dir $(CURDIR))
endif

BT_SPECS := $(BASE_TYPE_DIR)/base_type_specs.txt
BT_MK := $(BASE_TYPE_DIR)/base_types.mk
# BT_IMPORT := import_bt
# BT_IMPORT_ALL := import_all_bt
BT_IMPORT := $(BASE_TYPE_DIR)/import_bt
BT_IMPORT_ALL := $(BASE_TYPE_DIR)/import_all_bt



# The following rules are only run once base-type related setup is complete.

BUILT_INS := plist_dt plist_ch plist_st plist_re plist_nosep plist_np \
	plist_longest plist_longest_pred ptable pstream\
	precord popt ptry ptransform

MODULES := strhack pads  generic_common generic generic_combinators \
	pint punit pbool peof peor pstring_lit pcommit\
	generic_lazy_tool make_plist plist_gen plist_gen_pred plist_gen_spec \
	generic_tool generic_producer type padsEasy

# Extensions that depend on generated modules.
EXT_MODULES := pIP

# used in depend target of common.mk
SOURCES := $(foreach module,$(MODULES),$(module).mli) $(foreach module,$(MODULES),$(module).ml) \
	$(foreach built_in,$(BUILT_INS),$(built_in).mli) $(foreach built_in,$(BUILT_INS),$(built_in).ml) built_ins.ml \
	 $(foreach ext,$(EXT_MODULES),$(ext).mli) $(foreach ext,$(EXT_MODULES),$(ext).ml) 

# import variable GEN_MODULES from $(BT_MK)
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),veryclean)
ifneq ($(MAKECMDGOALS),genbt)

    include $(BT_MK)

endif
endif
endif
GEN_SOURCES := $(foreach module,$(GEN_MODULES),$(module).mli) $(foreach module,$(GEN_MODULES),$(module).ml)
SOURCES += $(GEN_SOURCES)
MODULES += $(GEN_MODULES) $(EXT_MODULES)

CORE_INTERFACES := $(foreach module,$(MODULES),$(module).cmi)
CORE_OBJS := $(foreach module,$(MODULES),$(module).cmo)
CORE_OBJS_OPT := $(foreach module,$(MODULES),$(module).cmx)
CORE_C_OBJS_OPT := $(foreach module,$(MODULES),$(module).o)

BUILT_IN_INTERFACES := $(foreach built_in,$(BUILT_INS),$(built_in).cmi)
BUILT_IN_OBJS := $(BUILT_IN_INTERFACES:.cmi=.cmo)
BUILT_IN_OBJS_OPT := $(BUILT_IN_INTERFACES:.cmi=.cmx)
BUILT_IN_C_OBJS_OPT := $(BUILT_IN_INTERFACES:.cmi=.o)

INTERFACES := $(CORE_INTERFACES) $(BUILT_IN_INTERFACES) built_ins.cmi
OBJS := $(CORE_OBJS) $(BUILT_IN_OBJS) built_ins.cmo
OBJS_OPT := $(CORE_OBJS_OPT) $(BUILT_IN_OBJS_OPT) built_ins.cmx
C_OBJS_OPT := $(CORE_C_OBJS_OPT) $(BUILT_IN_C_OBJS_OPT) built_ins.o

.PHONY: veryclean $(SUBDIRS_VC)

ifneq ($(BUILD),$(notdir $(CURDIR)))
  include $(PML_HOME)/mk/redirect.mk
else

 # The following rules are run from the build directory
.PHONY: all runtime runtime-bc install clean $(SUBDIRS)

all: $(SUBDIRS) $(INTERFACES) $(OBJS) $(OBJS_OPT)

$(SUBDIRS):
	$(MAKE) --print-directory -C ../$@ $(MAKECMDGOALS)

include $(PML_HOME)/mk/common.mk

$(BT_IMPORT) : import_bt.cmo
	$(OCAMLC) $(OCAMLFLAGS) $< -o $@

$(BT_IMPORT_ALL) : import_all_bt.cmo
	$(OCAMLC) $(OCAMLFLAGS) str.cma $< -o $@

$(GEN_SOURCES) $(BT_MK) : $(BT_SPECS) $(BT_IMPORT) $(BT_IMPORT_ALL)
	(cd $(BASE_TYPE_DIR); $(BT_IMPORT_ALL) $(BT_SPECS) $(BT_MK) GEN_MODULES)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),veryclean)
ifneq ($(MAKECMDGOALS),genbt)
    include .depend
endif
endif
endif

# This next line must be after including common.mk so as to use the
# settings from common.mk.
DEPINCLUDES += -I $(PADSC_RUNTIME_DIR)

# We include . instead of $(PML_LIB_DIR) because we want to reference the most
# recent versions of the .cm[ixo] files, not the installed versions.
INCLUDES += -I . -I ../padsc_interface

built_ins.cmi : built_ins.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<
	@(if [ ! "$(dir $<)" = "$(dir $@)" ]; then \
	     mv $(dir $<)$@ $@; \
         fi)

# $(BUILT_IN_OBJS):  %.cmo : %.ml
# 	$(CompileBytecode)

# $(BUILT_IN_OBJS_OPT): OCAMLOPTFLAGS+=-for-pack Built_ins
# $(BUILT_IN_OBJS_OPT) :%.cmx : %.ml
# 	$(CompileNative)

# $(BUILT_IN_C_OBJS_OPT): OCAMLOPTFLAGS+=-for-pack Built_ins
# $(BUILT_IN_C_OBJS_OPT):%.o : %.ml
# 	$(CompileNative)

# built_ins.cmi : $(BUILT_IN_INTERFACES)
# 	$(OCAMLC) -pack -o $@ $(BUILT_IN_OBJS)

# built_ins.cmo : $(BUILT_IN_OBJS)
# 	$(OCAMLC) -pack -o $@ $(BUILT_IN_OBJS)

# built_ins.cmx : $(BUILT_IN_OBJS_OPT)
# 	$(OCAMLOPT) -pack -o $@ $(BUILT_IN_OBJS_OPT)

# built_ins.o : $(BUILT_IN_OBJS_OPT)
# 	$(OCAMLOPT) -pack -o $@ $(BUILT_IN_OBJS_OPT)

$(CORE_C_OBJS_OPT) : %.o : %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<
	@(if [ ! "$(dir $<)" = "$(dir $@)" ]; then \
	     mv $(dir $<)$@ $@; \
	     mv $(dir $<)$*.cmx $*.cmx; \
          fi)

# make the pads/c runtime
$(PADSC_RUNTIME) $(PADSC_RUNTIME_OPT) :
	+$(MAKE) -C $(PADSC_RUNTIME_DIR) $@

$(PMLRUNTIME_LIB_DIR) :
	+@(if [ ! -d $(PMLRUNTIME_LIB_DIR) ] ; then \
             mkdir -p $(PMLRUNTIME_LIB_DIR); \
	  fi)

$(PMLRUNTIME) $(PMLRUNTIME_OPT) $(PMLRUNTIME_OPT_CLIB) : $(PMLRUNTIME_LIB_DIR)

ifdef NO_OCAMLFIND
# install runtime library and interfaces
$(PMLRUNTIME): $(PADSC_RUNTIME) $(OBJS)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ -a $(PADSC_RUNTIME) $(OBJS)
	cp $(INTERFACES) $(PML_LIB_DIR)

# Version of the runtime that does not link in dependencies.
$(PMLRUNTIME_NOLINK): $(PADSC_RUNTIME) $(OBJS)
	$(OCAMLC) $(OCAMLFLAGS)  \
	-o $@ -a $(PADSC_RUNTIME) $(OBJS)

# install runtime library and interfaces
$(PMLRUNTIME_OPT) $(PMLRUNTIME_OPT_CLIB) : $(PADSC_RUNTIME_OPT) $(OBJS_OPT) $(C_OBJS_OPT)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -verbose -o $(PMLRUNTIME_OPT) -a $(OBJS_OPT)
#	(lib=$(PMLRUNTIME_OPT_CLIB); $(FixStaticLib))
	cp $(INTERFACES) $(PML_LIB_DIR)
else
# install runtime library and interfaces
$(PMLRUNTIME): $(PADSC_RUNTIME) $(OBJS)
	ocamlfind ocamlc $(OCAMLFLAGS)  -package unix -linkpkg \
	  -o $@ -a $(PADSC_RUNTIME) $(OBJS)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ -a unix.cma $(PADSC_RUNTIME) $(OBJS)
	cp $(INTERFACES) $(PML_LIB_DIR)

# Version of the runtime that does not link in dependencies.
$(PMLRUNTIME_NOLINK): $(PADSC_RUNTIME) $(OBJS)
	ocamlfind ocamlc $(OCAMLFLAGS)  -package unix \
	-o $@ -a $(PADSC_RUNTIME) $(OBJS)

# install runtime library and interfaces
$(PMLRUNTIME_OPT) $(PMLRUNTIME_OPT_CLIB) : $(PADSC_RUNTIME_OPT) $(OBJS_OPT) $(C_OBJS_OPT)
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -verbose -o $(PMLRUNTIME_OPT) -a $(OBJS_OPT)
# 	(lib=$(PMLRUNTIME_OPT_CLIB); $(FixStaticLib))
	cp $(INTERFACES) $(PML_LIB_DIR)
endif





runtime-bc: $(PMLRUNTIME) $(PMLRUNTIME_NOLINK)
runtime: $(PMLRUNTIME_OPT) $(PMLRUNTIME_OPT_CLIB)

install: $(SUBDIRS) runtime-bc runtime
install-all: $(SUBDIRS) runtime-bc runtime

# Clean up
clean:
	mv import_bt.cmi import_bt.cmo import_all_bt.cmi import_all_bt.cmo ..
	$(RM) *.cm[iox]
	$(RM) *.o
	$(RM) .depend
	(cd ..; mv import_bt.cmi import_bt.cmo import_all_bt.cmi import_all_bt.cmo $(BUILD))

# End of rules that are run from the build directory
endif

# Rules not in the above conditional will override the 
# catchall rule in redirect.mk. Therefore, if specified by
# the user, they will be run from original directory. If specified
# as prereqs, then will be run from same directory as rule for which
# they are a prereq.

$(SUBDIRS_VC): %.vc : 
	$(MAKE) --print-directory -C $* veryclean

veryclean:  $(SUBDIRS_VC)
	$(RM) *~ 
	-$(BT_IMPORT_ALL) --clean $(BT_SPECS)
	$(RM) $(BT_MK)
	$(RM) $(BT_IMPORT) $(BT_IMPORT_ALL)
	$(RM) -r $(BUILD)

# Goal to explicitly generate base-type related files.
genbt:
	(cd $(BASE_TYPE_DIR); $(BT_IMPORT_ALL) $(BT_SPECS) $(BT_MK) GEN_MODULES)

# btclean:
# 	-$(BT_IMPORT_ALL) --clean $(BT_SPECS)

