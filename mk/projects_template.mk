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
###################################################################
# This makefile is a template for setting up a new pads/ml project
# that will compile to native code.
#
# This is a GNU makefile.
###################################################################

######################################################################
# First, set the project type: a stand-alone application, a generic
# tool, or a description from which apps should be auto-generated.
######################################################################
# PROJECT_TYPE := lib
# PROJECT_TYPE := tool
# PROJECT_TYPE := auto
PROJECT_TYPE := app

###################################################################
# Default target                                                  
###################################################################
default: all

###################################################################
# Find the root pads/ml directory.
# set the argument of the cd command appropriately
###################################################################
ifndef PML_HOME
  export PML_HOME=$(shell cd ../..; pwd)
endif

###################################################################
# !!! Tailor these variables to your project. 
# Uncomment any variables that you want to define.
###################################################################

# For app and auto: program name. Native, byte-code, debug and
# profiled versions are derived.
# PROGRAM :=

# Tools that the program uses. This should be used for stable tools
# only, as the makefile does not automatically determine dependencies
# on these tools. Alternatively, you can manually add dependencies
# if the tools are not stable.
# TOOLS :=

# For lib : library name and install directory, and interface
# (include) install directory.
# LIBRARY :=
# LIB_INSTALL_DIR :=
# INTF_INSTALL_DIR :=

# Ocaml sources (.ml,.mli) and pads/ml sources (.pml)
# SOURCES := 

# A module used to drive the tools and the directory in which the
# compiled tool-driver module is located.  Not all projects will have
# such a module. Only set this if the tool driver is managed by another 
# makefile. Otherwise, just add the source to SORUCES.
# TOOL_DRIVER :=
# TOOL_DRIVER_DIR =

# Directory structure, if different than defaults.
# For example, a one-directory project would have the following 
# settings:
# SRC=.
# P_DIR=.
# GEN_DIR=.
# GEN_P_DIR=.
# DATA_DIR=.

###################################################################
# Variables used in building
###################################################################
# Variables for byte-code compilation. See below (native
# compilation) for description.
# OBJS_BC = 
# OBJ_DEPS_BC := 
# LIBS_BC = 
# LIB_DEPS_BC =

# Variables for native compilation

# Objects linked in to the program. Default includes compiled versions
# of sources and descriptions, tools, and tool driver.
# OBJS_OPT = 

# Objects upon which the program depends.
# Default includes $(OBJS_OPT).
# OBJ_DEPS_OPT := 

# Libs linked in to the program. Default includes some standard
# pads/ml libraries. Only $(PADSC_RUNTIME_OPT) and $(PMLRUNTIME_OPT)
# are essential, though.
# LIBS_OPT = 

# Libs upon which the program depends.
# If you are frequently recompiling some libs, then include
# them here. For example:
# # NOTE: depends on dynamic binding of all the variables mentioned.
# # They will be bound in projects_common.mk
# LIB_DEPS_OPT = $(PADSC_RUNTIME_OPT) $(PMLRUNTIME_OPT) $(TOOL_CMXS)
#
# Otherwise, its probably best to leave this blank.
# LIB_DEPS_OPT =

# For OBJS_OPT, OBJS_DEPS_OPT and LIBS_OPT, you can access the default
# setting by appending "_def" to the variable name. This can be
# useful, for example, if you intend to extend the definition instead
# of overriding it.

# Additional includes passed to the OCaml compiler
# ADD_INCLUDES = 

###################################################################
# !!! Add targets/dependencies specific to your project.
# Most useful for adding dependencies on pads/ml descriptions,
# as those can't be generated automatically by ocamldep.
###################################################################

###################################################################
# Import common variables and targets
###################################################################
include $(PML_HOME)/mk/projects_common.mk

