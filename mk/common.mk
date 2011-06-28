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
include $(PML_HOME)/mk/common_vars.mk

ifdef SRC
  # Tell make to search in source directory.
  # We use GPATH (in addition to VPATH) so that
  # updates are done to files in the directories 
  # they are found.

  vpath %.mli $(SRC)
  vpath %.ml $(SRC)
  GPATH = $(SRC)
endif

ifndef SRC
  # override this variable to look for sources in different directory
  SRC=.
endif

# ifndef BUILD
#   # override this variable to place compiled output in different directory
#   BUILD=.
# endif

# Common rules

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<
	@(if [ ! "$(dir $<)" = "$(dir $@)" ]; then \
	     mv $(dir $<)$@ $@; \
          fi)

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<
	@(if [ ! "$(dir $<)" = "$(dir $@)" ]; then \
	     if [ -f $(dir $<)$*.cmi ]; then \
	        mv $(dir $<)$*.cmi $(dir $@);	\
	     fi; \
	     mv $(dir $<)$@ $@; \
          fi)

%.cmx: %.ml 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<
	@(if [ ! "$(dir $<)" = "$(dir $@)" ]; then \
	     if [ -f $(dir $<)$*.cmi ]; then \
	        mv $(dir $<)$*.cmi $(dir $@);	\
	     fi; \
	     mv $(dir $<)$@ $@; \
	     mv $(dir $<)$*.o $*.o; \
          fi)

define CompileInterface
$(OCAMLC) $(OCAMLFLAGS) $$AddFlags -c $<
@(if [ ! "$(dir $<)" = "$(dir $@)" ]; then \
  mv $(dir $<)$@ $@; \
fi)
endef

define CompileBytecode
$(OCAMLC) $(OCAMLFLAGS) $$AddFlags -c $<
@(if [ ! "$(dir $<)" = "$(dir $@)" ]; then \
  if [ -f $(dir $<)$*.cmi ]; then \
    mv $(dir $<)$*.cmi $(dir $@);	\
  fi; \
  mv $(dir $<)$@ $@; \
fi)
endef

define CompileNative
$(OCAMLOPT) $(OCAMLOPTFLAGS) $$AddFlags -c $<
@(if [ ! "$(dir $<)" = "$(dir $@)" ]; then \
  if [ -f $(dir $<)$*.cmi ]; then \
    mv $(dir $<)$*.cmi $(dir $@);	\
  fi; \
  mv $(dir $<)$@ $@; \
  mv $(dir $<)$*.o $*.o; \
fi)
endef

include $(PML_HOME)/mk/common_dep.mk
