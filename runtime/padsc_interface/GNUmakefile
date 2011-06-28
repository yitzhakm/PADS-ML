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
 # This makefile uses gmake extensions
 # such as $(shell ...) to execute a command

default : all


ifndef CAMLIDL
  export CAMLIDL := camlidl
endif

ifndef AST_ARCH
  export AST_ARCH=$(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
endif

.PRECIOUS: %.c

# These variables need to be set
ifndef PML_HOME
  export PML_HOME=$(shell cd ../..; pwd)
endif

ifndef PADS_HOME
  $(error Need to set shell variable PADS_HOME)
endif

# These variables should not need to change
PADSC=$(PADS_HOME)/scripts/padsc
INSTALLROOT=$(PADS_HOME)/ast-ast/arch/$(AST_ARCH)
LIBDIR=$(INSTALLROOT)/lib
LIBS = $(LIBDIR)/libpads.a $(LIBDIR)/libast.a
INCLUDES = -I$(PADS_HOME)/padsc/include -I$(INSTALLROOT)/include/ast -I.

# XXX: These variables are taken from common.mk. Should be changed to include common.mk
OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLDEP=ocamldep
# Set the PREPROC variable to use a preprocessor
OCAMLFLAGS=$(OC_INCLUDES) $(PREPROC)    
OCAMLOPTFLAGS=$(OC_INCLUDES) $(PREPROC) -inline 10 -ccopt -O3 -noassert -unsafe
PML_LIB_DIR=$(PML_HOME)/arch/$(AST_ARCH)/lib

PMLC_DIR=$(PML_HOME)/compiler
PMLC_LIB_DIR=$(PML_LIB_DIR)/comp
PMLC_LIB=$(PMLC_LIB_DIR)/pmlcomp.cma

PMLRUNTIME_DIR=$(PML_HOME)/runtime
PMLRUNTIME_LIB_DIR=$(PML_LIB_DIR)/runtime
PMLRUNTIME=$(PMLRUNTIME_LIB_DIR)/pmlruntime.cma
PMLRUNTIME_OPT=$(PMLRUNTIME_LIB_DIR)/pmlruntime.cmxa
PMLRUNTIME_OPT_CLIB=$(PMLRUNTIME_LIB_DIR)/pmlruntime.a

PADSC_RUNTIME_DIR=$(PML_HOME)/runtime/padsc_interface
PADSC_RUNTIME_LIB_DIR=$(PMLRUNTIME_LIB_DIR)
PADSC_RUNTIME=$(PADSC_RUNTIME_LIB_DIR)/padsc.cma
PADSC_RUNTIME_OPT=$(PADSC_RUNTIME_LIB_DIR)/padsc.cmxa
PADSC_RUNTIME_OPT_CLIB=$(PADSC_RUNTIME_LIB_DIR)/padsc.a

NBT_SPECS := base_type_specs.txt
BASE_TYPE_DIR := .
BT_MK := base_types.mk

# hand-code base types. Currently, there aren't any.
BASE_TYPES = 
BT_GENERATED_INTS := $(foreach base_type,$(BASE_TYPES),$(base_type)_c.mli)
BT_GENERATED_IMPLS := $(foreach base_type,$(BASE_TYPES),$(base_type)_c.ml)
BT_GENERATED_HEADERS := $(foreach base_type,$(BASE_TYPES),$(base_type)_c.h)
BT_GENERATED_STUBS := $(foreach base_type,$(BASE_TYPES),$(base_type)_c_stubs.c)
BT_GEN := $(BT_GENERATED_INTS) $(BT_GENERATED_IMPLS) $(BT_GENERATED_STUBS) $(BT_GENERATED_HEADERS)
BT_WRAPPERS := $(foreach base_type,$(BASE_TYPES),$(base_type)_c_stubs_w.c)
BT_WRAPPER_OBJS := $(BT_WRAPPERS:.c=.o)
BT_CAML_OBJS := $(foreach base_type,$(BASE_TYPES),$(base_type)_c.cmo)
BT_C_IMPL_OBJS := $(foreach base_type,$(BASE_TYPES),$(base_type)_c_impl.o)
BT_C_OBJS := $(BT_WRAPPER_OBJS) $(BT_C_IMPL_OBJS)
BT_OBJS := $(BT_CAML_OBJS) $(BT_C_OBJS)

BT_CAML_OBJS_OPT := $(BT_CAML_OBJS:.cmo=.cmx)
BT_CAML_C_OBJS_OPT := $(BT_CAML_OBJS:.cmo=.o)
BT_C_OBJS_OPT := $(BT_C_OBJS) $(BT_CAML_C_OBJS_OPT)
BT_OBJS_OPT := $(BT_CAML_OBJS_OPT) $(BT_C_OBJS_OPT)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),veryclean)
ifneq ($(MAKECMDGOALS),genbt)

# read base types from file
include $(BT_MK)

endif
endif
endif

# Base types generated based on base_type_specs.txt
NBT_IDLS := $(foreach base_type,$(N_BASE_TYPES),$(base_type)_c.idl)
NBT_GENERATED_INTS := $(foreach base_type,$(N_BASE_TYPES),$(base_type)_c.mli)
NBT_GENERATED_IMPLS := $(foreach base_type,$(N_BASE_TYPES),$(base_type)_c.ml)
NBT_GENERATED_STUBS := $(foreach base_type,$(N_BASE_TYPES),$(base_type)_c_stubs.c)
NBT_GEN := $(NBT_GENERATED_INTS) $(NBT_GENERATED_IMPLS) $(NBT_GENERATED_STUBS)
NBT_WRAPPERS := $(foreach base_type,$(N_BASE_TYPES),$(base_type)_c_stubs_w.c)
NBT_WRAPPER_OBJS := $(NBT_WRAPPERS:.c=.o)
NBT_CAML_OBJS := $(foreach base_type,$(N_BASE_TYPES),$(base_type)_c.cmo)
NBT_C_OBJS := $(NBT_WRAPPER_OBJS)
NBT_OBJS := $(NBT_CAML_OBJS) $(NBT_C_OBJS)

NBT_CAML_OBJS_OPT := $(NBT_CAML_OBJS:.cmo=.cmx)
NBT_CAML_C_OBJS_OPT := $(NBT_CAML_OBJS:.cmo=.o)
NBT_C_OBJS_OPT := $(NBT_C_OBJS) $(NBT_CAML_C_OBJS_OPT)
NBT_OBJS_OPT := $(NBT_CAML_OBJS_OPT) $(NBT_C_OBJS_OPT)

PADSC_WRAPPER := padsc_stubs_w.c
PADSC_WRAPPER_OBJ := padsc_stubs_w.o
PADSC_CAML_OBJS := padsc.cmo
PADSC_C_OBJS := $(PADSC_WRAPPER_OBJ)
PADSC_OBJS := $(PADSC_CAML_OBJS) $(PADSC_C_OBJS)

PADSC_CAML_OBJS_OPT := padsc.cmx
PADSC_CAML_C_OBJS_OPT := padsc.o
PADSC_C_OBJS_OPT := $(PADSC_C_OBJS) $(PADSC_CAML_C_OBJS_OPT)
PADSC_OBJS_OPT := $(PADSC_CAML_OBJS_OPT) $(PADSC_C_OBJS_OPT)


CAML_OBJS := $(PADSC_CAML_OBJS) $(BT_CAML_OBJS) $(NBT_CAML_OBJS)
CAML_INTERFACES := $(CAML_OBJS:.cmo=.cmi)
C_OBJS := $(PADSC_C_OBJS) $(BT_C_OBJS) $(NBT_C_OBJS) 
OBJS := $(PADSC_OBJS) $(BT_OBJS) $(NBT_OBJS)

CAML_OBJS_OPT := $(PADSC_CAML_OBJS_OPT) $(BT_CAML_OBJS_OPT) $(NBT_CAML_OBJS_OPT)
CAML_C_OBJS_OPT := $(PADSC_CAML_C_OBJS_OPT) $(BT_CAML_C_OBJS_OPT) $(NBT_CAML_C_OBJS_OPT) 
C_OBJS_OPT := $(PADSC_C_OBJS_OPT) $(BT_C_OBJS_OPT) $(NBT_C_OBJS_OPT) 
OBJS_OPT := $(PADSC_OBJS_OPT) $(BT_OBJS_OPT) $(NBT_OBJS_OPT)

GENERATED_OC_SOURCES :=  padsc.mli padsc.ml \
	$(BT_GENERATED_INTS) $(BT_GENERATED_IMPLS)\
	$(NBT_GENERATED_INTS) $(NBT_GENERATED_IMPLS)
ALL_GEN := padsc.ml padsc.mli padsc_stubs.c $(BT_GEN) $(NBT_GEN)
WRAPPERS := $(PADSC_WRAPPER) $(BT_WRAPPERS) $(NBT_WRAPPERS)
WRAPPER_OBJS := $(PADSC_WRAPPER_OBJ) $(BT_WRAPPER_OBJS) $(NBT_WRAPPER_OBJS)

# Name of padsidl library
PADSIDL_NAME := padsidl
PADSIDL_LIB_NM=$(mam_cc_PREFIX_ARCHIVE)$(PADSIDL_NAME)$(mam_cc_SUFFIX_ARCHIVE)

# Fully qualified names of object files.  Need fully qualifed file
# names for C object files so that they can be found by OCaml programs linking to
# the library.  The key point is that only their names are included in
# the OCaml lib, not the actual object files.
# Q_OBJS := $(foreach obj,$(OBJS),$(PADSIDL)/$(obj))

include $(PADS_HOME)/mk/rules.mk

# Set the location of the camlidl library if it is different than the
# location of the other ocaml libs.
ifndef CAMLIDL_LIB_DIR
CAMLIDL_LIB_DIR  :=  $(OCAML_LIB_DIR)
export CAMLIDL_LIB_DIR
endif

INCLUDES += -I$(OCAML_LIB_DIR) -I$(CAMLIDL_LIB_DIR)
OC_INCLUDES := -I $(CAMLIDL_LIB_DIR)

# Rule to make programs

all: $(PADSIDL_LIB_NM) padsc.cma padsc.cmxa

$(NBT_IDLS) $(NBT_WRAPPERS) $(BT_MK): $(NBT_SPECS) import_all_bt.sh import_bt.sh
	rm -f $(BT_MK)
	./import_all_bt.sh $(NBT_SPECS) $(BASE_TYPE_DIR)

$(BT_GENERATED_INTS):%.mli: padsc.idl %.idl
	$(CAMLIDL) -no-include -header $*.idl

$(BT_GENERATED_IMPLS):%.ml: padsc.idl %.idl
	$(CAMLIDL) -no-include -header $*.idl

$(BT_GENERATED_STUBS):%_stubs.c: padsc.idl %.idl
	$(CAMLIDL) -no-include -header $*.idl

$(BT_GENERATED_HEADERS):%.h: padsc.idl %.idl
	$(CAMLIDL) -no-include -header $*.idl

$(BT_C_IMPL_OBJS) : %_impl.o : %.c
	$(CC) -O3 $(INCLUDES)  -c $^ -o $@

$(NBT_GENERATED_INTS):%.mli: padsc.idl %.idl
	$(CAMLIDL) -no-include $*.idl

$(NBT_GENERATED_IMPLS):%.ml: padsc.idl %.idl
	$(CAMLIDL) -no-include $*.idl

$(NBT_GENERATED_STUBS):%_stubs.c: padsc.idl %.idl
	$(CAMLIDL) -no-include $*.idl

padsc.mli padsc.ml padsc_stubs.c:padsc.idl
	$(CAMLIDL) -no-include padsc.idl

$(CAML_OBJS): %.cmo: %.ml 
	$(OCAMLC) $(OCAMLFLAGS) -c $*.ml

$(CAML_OBJS_OPT): %.cmx: %.ml 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $*.ml

$(CAML_C_OBJS_OPT): %.o: %.ml 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $*.ml

%.cmo: %.ml 
	$(OCAMLC) $(OCAMLFLAGS) -c $*.ml

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $*.mli

$(PADSC_WRAPPER_OBJ) $(BT_WRAPPER_OBJS): %_stubs_w.o: %_stubs_w.c %_stubs.c %.h

$(NBT_WRAPPER_OBJS):%_stubs_w.o: %_stubs_w.c %_stubs.c

$(PADSIDL_LIB_NM) : $(C_OBJS)
	$(STATIC_LIBTOOL) $(PADSIDL_LIB_NM) $(STATIC_LIBTOOL_OPTS) $^
	(lib=$@; $(FixStaticLib))

# Note: does *not* depend on $(PADSIDL_LIB_NM) because only the options
# that mention the library are included in padsc.cma, not the actual library
# itself.
padsc.cma: $(CAML_OBJS) 
	$(OCAMLC) -a -o padsc.cma -custom \
	-ccopt -L$(PADSC_RUNTIME_LIB_DIR) -cclib -l$(PADSIDL_NAME) \
	-ccopt -L$(LIBDIR) -cclib -lpads -cclib -last  \
	-ccopt -L$(CAMLIDL_LIB_DIR) -cclib -lcamlidl \
	-cclib "$(OS_SPEC_XTRA_LIBS)" \
        $(CAML_OBJS)

padsc.cmxa padsc.a: $(CAML_OBJS_OPT)  $(CAML_C_OBJS_OPT) 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o padsc.cmxa \
	-ccopt -L$(PADSC_RUNTIME_LIB_DIR) -cclib -l$(PADSIDL_NAME) \
	-ccopt -L$(LIBDIR) -cclib -lpads  -cclib -last  \
	-ccopt -L$(CAMLIDL_LIB_DIR) -cclib -lcamlidl \
	-cclib "$(OS_SPEC_XTRA_LIBS)" \
        $(CAML_OBJS_OPT)

$(PADSC_RUNTIME_LIB_DIR):
	+@(if [ ! -d $(PADSC_RUNTIME_LIB_DIR) ] ; then \
		mkdir -p $(PADSC_RUNTIME_LIB_DIR); \
	  fi)


$(PADSC_RUNTIME_LIB_DIR)/$(PADSIDL_LIB_NM) : $(PADSC_RUNTIME_LIB_DIR) $(PADSIDL_LIB_NM)
	cp $(PADSIDL_LIB_NM) $(PADSC_RUNTIME_LIB_DIR)
	(lib=$@; $(FixStaticLib))

$(PADSC_RUNTIME) : padsc.cma $(PADSC_RUNTIME_LIB_DIR)
	cp padsc.cma $(PADSC_RUNTIME_LIB_DIR)
	cp $(CAML_INTERFACES) $(PML_LIB_DIR)

$(PADSC_RUNTIME_OPT) : padsc.cmxa $(PADSC_RUNTIME_LIB_DIR)
	cp padsc.cmxa $(PADSC_RUNTIME_LIB_DIR)
	cp $(CAML_INTERFACES) $(PML_LIB_DIR)

$(PADSC_RUNTIME_OPT_CLIB) : padsc.a $(PADSC_RUNTIME_LIB_DIR)
	cp padsc.a $(PADSC_RUNTIME_LIB_DIR)
	(lib=$@; $(FixStaticLib))

install : $(PADSC_RUNTIME_LIB_DIR)/$(PADSIDL_LIB_NM) $(PADSC_RUNTIME) $(PADSC_RUNTIME_OPT) $(PADSC_RUNTIME_OPT_CLIB)

# client: padsclib client.ml
# 	ocamlc client.ml  padsc.cma  -o client

padsc: padsc_stubs_w.o

padsclib: padsc.cma

# OCaml dependencies management
.depend: $(GENERATED_OC_SOURCES)	
	$(OCAMLDEP) $(GENERATED_OC_SOURCES) > .depend

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),veryclean)
ifneq ($(MAKECMDGOALS),genbt)

include .depend

endif
endif
endif

# NOTE: this will always return "Nothing to be done for..." as the
# file .depend will automatically be updated by make due to the earlier
# "include .depend". So, by the time this rule is executed, .depend
# will always be up to date. Still, this target is a useful dummy to force
# it to update.
depend:	.depend

# Goal to explicitly generate base-type related files.
genbt:
	rm -f $(BT_MK)
	./import_all_bt.sh $(NBT_SPECS) $(BASE_TYPE_DIR)

clean:
	$(RM) padsc.cma padsc.cmxa padsc.a
	$(RM) *.cmo *.cma *.cmi $(PADSIDL_LIB_NM)
	$(RM) *.o

veryclean:
	./import_all_bt.sh --clean $(NBT_SPECS)
	$(RM) $(BT_MK)
	$(RM) $(ALL_GEN)
	$(RM) *.cmo *.cma *.cmx *.cmxa *.cmi $(PADSIDL_LIB_NM)
	$(RM) *.o *.a
	$(RM) client a.out
	$(RM) *~
	$(RM) .depend
ifneq ($(BASE_TYPE_DIR),.)
	$(RM) -r $(BASE_TYPE_DIR)
endif

.PHONY: genbt install all clean veryclean padsc padsclib depend
