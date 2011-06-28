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
ifndef AST_ARCH
  export AST_ARCH=$(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
endif

ifdef USE_BATTERIES
OCAMLC=ocamlfind batteries/ocamlc
OCAMLOPT=ocamlfind batteries/ocamlopt
else
OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
endif

OCAMLDEP=ocamldep
INCLUDES=-I +camlp5
DEPINCLUDES=-I +camlp5
# Set the PREPROC variable to use a preprocessor
OCAMLFLAGS=$(INCLUDES) $(PREPROC)
OCAMLOPTFLAGS=$(INCLUDES) $(PREPROC) -inline 10 -ccopt -O3 -noassert
ifdef DEBUG_OCAMLOPT
#OCAMLOPTFLAGS += -ccopt "-Xlinker -Y -Xlinker 3" -verbose
OCAMLOPTFLAGS += -verbose
endif

# PMLFLAGS =
# PMLOPTFLAGS =

# if CAMLIDL_LIB_DIR, then its probably different than the default ocaml lib directory, so add it to the includes.
ifdef CAMLIDL_LIB_DIR
INCLUDES +=-I $(CAMLIDL_LIB_DIR)
endif

PML_LIB=$(PML_HOME)/arch/$(AST_ARCH)/lib
PML_LIB_DIR:=$(PML_LIB)

PMLC_DIR=$(PML_HOME)/compiler
PMLC_LIB_DIR=$(PML_LIB)/comp
PMLC_LIB=$(PMLC_LIB_DIR)/pmlcomp.cma
PMLC_LIB_OPT=$(PMLC_LIB_DIR)/pmlcomp.cmxa
PMLC_LIB_OPT_CLIB=$(PMLC_LIB_DIR)/pmlcomp.a

# PADS/ML compiler.
PMLC=$(PML_HOME)/scripts/pmlc.opt

# PADS/ML compiler, native-code.
PMLOPT=$(PML_HOME)/scripts/pmlopt.opt

# Full PADS/ML compiler, native-code.
PADSMLC=$(PML_HOME)/scripts/padsmlc

PMLRUNTIME_DIR=$(PML_HOME)/runtime
PMLRUNTIME_LIB_DIR=$(PML_LIB)/runtime

PMLRUNTIME=$(PMLRUNTIME_LIB_DIR)/pmlruntime.cma
PMLRUNTIME_NOLINK=$(PMLRUNTIME_LIB_DIR)/pmlruntime-nolink.cma

PMLRUNTIME_OPT=$(PMLRUNTIME_LIB_DIR)/pmlruntime.cmxa
PMLRUNTIME_OPT_CLIB=$(PMLRUNTIME_LIB_DIR)/pmlruntime.a

# FIX: Profiling not yet supported, so just set equal
# to standard lib.
PMLRUNTIME_PROF_OPT=$(PMLRUNTIME_LIB_DIR)/pmlruntime.cmxa
PMLRUNTIME_PROF_OPT_CLIB=$(PMLRUNTIME_LIB_DIR)/pmlruntime.a

PADSC_RUNTIME_DIR=$(PML_HOME)/runtime/padsc_interface
PADSC_RUNTIME_LIB_DIR=$(PMLRUNTIME_LIB_DIR)

PADSC_RUNTIME=$(PADSC_RUNTIME_LIB_DIR)/padsc.cma

PADSC_RUNTIME_OPT=$(PADSC_RUNTIME_LIB_DIR)/padsc.cmxa
PADSC_RUNTIME_OPT_CLIB=$(PADSC_RUNTIME_LIB_DIR)/padsc.a

# FIX: Profiling not yet supported, so just set equal
# to standard lib.
PADSC_RUNTIME_PROF_OPT=$(PADSC_RUNTIME_LIB_DIR)/padsc.cmxa
PADSC_RUNTIME_PROF_OPT_CLIB=$(PADSC_RUNTIME_LIB_DIR)/padsc.a

# Library of/for tools
PML_TOOLS_LIB_NAME=libtools

PML_TOOLS_LIB=$(PMLRUNTIME_LIB_DIR)/$(PML_TOOLS_LIB_NAME).cma

PML_TOOLS_LIB_OPT=$(PMLRUNTIME_LIB_DIR)/$(PML_TOOLS_LIB_NAME).cmxa
PML_TOOLS_LIB_OPT_CLIB=$(PMLRUNTIME_LIB_DIR)/$(PML_TOOLS_LIB_NAME).a

PML_TOOLS_LIB_PROF_OPT=$(PMLRUNTIME_LIB_DIR)/$(PML_TOOLS_LIB_NAME)_p.cmxa
PML_TOOLS_LIB_PROF_OPT_CLIB=$(PMLRUNTIME_LIB_DIR)/$(PML_TOOLS_LIB_NAME)_p.a

# XMLLIGHT_LIB_DIR specifies the installed location of the
# MotionTwin XMLLight libraries
ifndef XMLLIGHT_LIB_DIR
XMLLIGHT_LIB_DIR := $(PML_LIB)/xml
endif
INCLUDES +=-I $(XMLLIGHT_LIB_DIR)
# XML-Light library to be linked
XML_LIB:=$(XMLLIGHT_LIB_DIR)/xml-light.cma
XML_LIB_OPT:=$(XMLLIGHT_LIB_DIR)/xml-light.cmxa

PML_BIN_DIR:=$(PML_HOME)/bin
