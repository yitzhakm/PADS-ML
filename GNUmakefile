########################################################################
#                                                                      #
#             This software is part of the padsml package              #
#           Copyright (c) 2006-2007 AT&T Knowledge Ventures            #
#                         All Rights Reserved                          #
#         This software is licensed by AT&T Knowledge Ventures         #
#           under the terms and conditions of the license in           #
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
#            Yitzhak Mandelbaum <yitzhak@research.att.com>>            #
#                 Robert Gruber <bob.gruber@gmail.com>                 #
#                                                                      #
########################################################################

# Default target
default : install

SUBDIRS = compiler runtime tools
SUBDIRS_INS = $(foreach subdir,$(SUBDIRS),$(subdir).ins)
SUBDIRS_DEP = $(foreach subdir,$(SUBDIRS),$(subdir).dep)
SUBDIRS_VC = $(foreach subdir,$(SUBDIRS),$(subdir).vc)
SUBDIRS_CL = $(foreach subdir,$(SUBDIRS),$(subdir).cl)

ifndef AST_ARCH
  export AST_ARCH=$(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
endif
ARCH := arch/$(AST_ARCH)

all: $(SUBDIRS)

$(ARCH):
	mkdir $(ARCH)

install: $(ARCH) $(SUBDIRS_INS)

depend: $(SUBDIRS_DEP)

clean: $(SUBDIRS_CL)

veryclean: $(SUBDIRS_VC)

$(SUBDIRS):
	$(MAKE) -C $@ all

$(SUBDIRS_INS): %.ins : 
	$(MAKE) --print-directory -C $* install

$(SUBDIRS_DEP): %.dep : 
	$(MAKE) --print-directory -C $* .depend

$(SUBDIRS_VC): %.vc : 
	$(MAKE) --print-directory -C $* veryclean

$(SUBDIRS_CL): %.cl : 
	$(MAKE) --print-directory -C $* clean

dpads-compiler :
	$(MAKE) -C dev/d/compiler

dpads-clean :
	$(MAKE) -C dev/d/compiler clean

.PHONY: all depend clean veryclean $(SUBDIRS) $(SUBDIRS_INS) $(SUBDIRS_DEP) $(SUBDIRS_VC) $(SUBDIRS_CL)
