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
ifndef BUILD
ifdef AST_ARCH
BUILD := $(AST_ARCH)
endif
endif

ifndef BUILD
%: forceabort
	@echo "BUILD must be defined before including redirect.mk"
	@exit 1

forceabort: ;
else

MAKETARGET = $(MAKE) --no-print-directory -C $@ -f $(CURDIR)/GNUmakefile $(MAKECMDGOALS)
ifndef DEBUG_BUILD
  DEBUG_BUILD=debug
endif

.SUFFIXES:

.PHONY: $(BUILD)
$(BUILD):
	+@echo "Making $(MAKECMDGOALS) in $@"
	+@(if [ ! -d $@ -o ! -d $@/$(DEBUG_BUILD) ] ; then \
		mkdir -p $@/debug; \
		if ( grep "$(BUILD)" .cvsignore ); then \
		  echo -n ""; \
		else \
		  echo "$(BUILD)" >> .cvsignore; \
		fi; \
	  fi)
	+@$(MAKETARGET)

GNUmakefile : ;
%.mk :: ;

# cause any target not otherwise mentioned to depend on $(BUILD).
# If a target is mentioned seperately in another rule, that rule
# will override this one. 
% :: $(BUILD) ;

endif
