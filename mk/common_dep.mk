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
# Dependency support. Argument: SOURCES

# Note: this target will always return "Nothing to be done for..." as
# the file .depend will automatically be updated by make, if the
# makefile contains "include .depend". So, by the time this rule is
# executed, .depend will always be up to date. Still, this target is a
# useful dummy to force it to update.
depend: .depend


# Determine location of makefile with respect to pwd.
# If a makefile has set CURDIR, then the makefile is not in 
# the pwd. Othwerwise, it is, so we use ".".
ifdef CURDIR
  MKDIR=$(CURDIR)
else
  MKDIR=.
endif

# Note: this target will always return ".depend is up to date" when
# called explicitly by user when file ".depend" is included by
# makefile. See target "depend" for explanation.
# Need to post-process depend files b/c ocamldep assumes compiled files
# will be in the same directory as source files.
GEN_FULL_ESC=$(subst /,\/,$(shell pwd)/$(GEN_DIR))
BUILD_FULL_ESC=$(subst /,\/,$(shell pwd)/$(BUILD))
#	
.depend: $(SOURCES)
ifeq ($(SRC),)
	$(OCAMLDEP) $(DEPINCLUDES) $(PREPROC) $(SOURCES) > .depend
else 

  ifneq ($(SRC),.)
	(cd $(SRC); $(OCAMLDEP) $(DEPINCLUDES) $(PREPROC) $(SOURCES) > .depend)
    ifeq ($(GEN_DIR),)
	mv $(SRC)/.depend .depend
    else
	sed -e 's/$(GEN_FULL_ESC)/$(BUILD_FULL_ESC)/g' $(SRC)/.depend > .depend
    endif

  else
	$(OCAMLDEP) $(DEPINCLUDES) $(PREPROC) $(SOURCES) > .depend
	sed -e 's/$(GEN_FULL_ESC)/$(BUILD_FULL_ESC)/g' .depend > .depend_pp
	@mv .depend_pp .depend
  endif
endif

.PHONY: depend
