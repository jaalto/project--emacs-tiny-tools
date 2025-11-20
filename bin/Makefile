# Makefile -- Makefile for the Emacs lisp. Tiny tools distribution
#
#   File id
#
#	Copyright (C)  2001-2025 Jari Aalto
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
#	Without reading any of the instructions in doc/html/index.txt,
#	copy lisp/tiny/load-path.el to your private place, modify and compile:
#
#	    make rules
#	    make help | less
#	    make EMACS=xemacs LIB="~/elisp/load-path.el"
#
#	This Makefile is designed to work only with GNU bash(1)

############################################################# &code ###

AUTHOR	    = "Jari Aalto"
EMAIL_TO    =

MAKEFILE    = Makefile

# ########################################################### &basic ###

SHELL	    = sh
EMACS	    = emacs
# EMACSVER  =`$(EMACS) -batch -version`
PERL	    = perl
LIB	    = load-path.el

FILE_DOC_MAKE =	perl ripdoc.pl
README	    = ../doc/html/install/index.txt

# ############################################################ &rules ###

.SUFFIXES: .el .txt .pl .mak .elc .html .doc .prn .std .htmlx

# Rule: suffix .txt => .prn - Print file to stdout
.txt.prn:
	@echo "--  #FILE: $< --------------------------"
	@cat $<

# Rule: all - Nothing interesting. This Makefile is for developer only.
all:
	@echo "make LIB=your-load-path.el compile"

# Rule: compile - Developer target. Make and Compile all. Cleans everything first
compile: clean compile-other compile-tiny-lib compile-tiny

# Rule: compile-other - compile lisp/other
compile-other:
	cd ../lisp/other && \
	for file in *.el; \
	do \
	    env LANG=en_US.UTF-8 $(EMACS) --no-site-file -Q --batch -f batch-byte-compile $$file; \
	done

# Rule: compile-tiny-lib - compile lisp/tiny libraries
compile-tiny-lib:
	cd ../lisp/tiny && \
	for file in \
	    tinyliba.el \
	    tinylibb.el \
	    tinylibm.el \
	    tinylibenv.el \
	    tinylibid.el \
	    tinylibo.el \
	    tinylibt.el \
	    tinylibw.el \
	    tinyliby.el \
	    tinylib.el \
	    tinylibmail.el \
	    tinylibmenu.el ; \
	do \
	    env LANG=en_US.UTF-8 $(EMACS) --no-site-file -Q --batch --load $(LIB) -f batch-byte-compile $$file; \
	done

# Rule: compile-tiny-lib - compile lisp/tiny packages
compile-tiny:
	cd ../lisp/tiny && \
	for file in `ls *.el | grep -v lib`; \
	do \
	    env LANG=en_US.UTF-8 $(EMACS) --no-site-file -Q --batch --load $(LIB) -f batch-byte-compile $$file; \
	done

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
