# Makefile -- Makefile for the Emacs lisp. Tiny tools distribution
#
#   File id
#
#	Copyright (C)  2001-2010 Jari Aalto
#
#	This program is free software; you can redistribute it and/or
#	modify it under the terms of the GNU General Public License as
#	published by the Free Software Foundation; either version 2 of
#	the License, or (at your option) any later version.
#
#	This program is distributed in the hope that it will be useful, but
#	WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#	General Public License for more details.
#
#	You should have received a copy of the GNU General Public License
#	along with this program. If not, see <http://www.gnu.org/licenses/>.
#
#	Visit <http://www.gnu.org/copyleft/gpl.html> for more information
#
#   Description
#
#	This simplistic makefile is for Unix users. Win32 users should
#	Use makefile.pl directly. See --help for more.
#
#	    dos> perl makefile.pl --load e:/myhome/elisp/load-path.el \
#		 --binary "c:/program files/absolute-path-to/emacs.exe"
#
#	Without reading any of the instructions from doc/txt/README.txt,
#	copy lisp/tiny/load-path.el to your private place, modify and compile:
#
#	    % make rules
#	    % make help | less
#	    % make EMACS=xemacs LIB="--load ~/elisp/load-path.el"
#
#	This Makefile is designed to work only with GNU bash(1)

############################################################# &code ###

AUTHOR	    = "Jari Aalto"
EMAIL_TO    =

MAKEFILE    = Makefile

# ########################################################### &basic ###

SHELL	    =	sh
EMACS	    =	emacs
# EMACSVER    =	`${EMACS} -batch -version`
PERL	    =	perl
LIB	    =	--load ../lisp/tiny/load-path.el

FILE_DOC_MAKE =	perl ripdoc.pl
README	    = ../doc/txt/README.txt

# ############################################################ &rules ###

.SUFFIXES: .el .txt .pl .mak   .elc .html .doc .prn .std .htmlx

# Rule: suffix .txt => .prn - Print file to stdout
.txt.prn:
	@echo "--  #FILE: $< --------------------------"
	@cat $<

# Rule: all - Make and Compile all. Cleans everything first
all:	clean
	perl makefile.pl --binary "$(EMACS)" $(LIB) all

# Rule: autoload - Make autoload files
autoload:
	cd ../lisp/tiny/
	$(EMACS) -batch -nw -q $(LIB) -l tiny-setup.el \
	-f tiny-setup-autoload-batch-update ..

# Rule: readme - Print readme file
readme: $(README:.txt=.prn)

# Rule: help - Print distribution help
help:	readme

rules:
	@echo "-- #FILE: $(MAKEFILE) rules --------------------------"
	@grep 'Rule\: ' $(MAKEFILE) | grep -v maint | sed -e 's/^./ /'
	@echo ""

# Rule: clean - Delete compiled files, backups and CVS cruft
clean:
	-rm ../lisp/other/*.elc			\
	    ../lisp/other/*~			\
	    ../lisp/other/*#			\
	    ../lisp/other/.#*			\
	    ../lisp/tiny/*.elc			\
	    ../lisp/tiny/*~			\
	    ../lisp/tiny/*#			\
	    ../lisp/tiny/.#*			\
	    ../doc/txt/*.html

install:
	@echo "	 There is no automatic install."
	@echo "	 Please copy whole tiny-tools/ tree to suitable hierarchy"
	@echo "	 e.g. to /usr/share/emacs/site-lisp/packages"
	@echo "	 Refer to doc/README.txt for site wide and personal setup."

link-check:
	files=`ls ../doc/txt/*.txt`; \
	perl -S t2html.pl --Link-check-single --quiet $$files

# End of file
