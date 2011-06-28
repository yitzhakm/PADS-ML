###################################################################
# This makefile is a template for setting up a new pads/ml project
# that will compile to native code.
#
# This is a GNU makefile.
###################################################################

PROJECT_TYPE := lib

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
# Remove any variables that you don't need to define.
###################################################################
LIBRARY = $(PML_TOOLS_LIB_NAME)
LIB_INSTALL_DIR = $(PMLRUNTIME_LIB_DIR)
INTF_INSTALL_DIR = $(PML_LIB_DIR)

SRC=.

# Program name, pads/ml descriptions, ocaml sources (.ml).
SOURCES := debug_tool.mli debug_tool.ml \
	   universal_inject_tool.mli  universal_inject_tool.ml \
           flatten_tool.mli flatten_tool.ml \
	   xml_formatter.mli xml_formatter.ml \
	   toolDriver.mli toolDriver.ml

LIB_DEPS_BC = $(PADSC_RUNTIME) $(PMLRUNTIME)
LIB_DEPS_OPT = $(PADSC_RUNTIME_OPT) $(PMLRUNTIME_OPT)
LIB_DEPS_PROF_OPT = $(PADSC_RUNTIME_PROF_OPT) $(PMLRUNTIME_PROF_OPT)


###################################################################
# !!! Add targets/dependencies specific to your project.
# Most useful for adding dependencies on pads/ml descriptions,
# as those can't be generated automatically ocamldep.
###################################################################

veryclean : clean

###################################################################
# Import common variables and targets
###################################################################
include $(PML_HOME)/mk/projects_common.mk

