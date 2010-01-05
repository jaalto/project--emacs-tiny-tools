#!/usr/bin/make -f
# -*- makefile -*-
#
# makefile.mk -- Makefile for the Emacs lisp. Tiny tools distribution
#
#   File id
#
#	Copyright (C) 1997-2010 Jari Aalto
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
#	*******************************************************************
#
#	2000-05-24 This Unix makefile format is no longer used. All
#	the documentaion below is outdated and invalid. This file is
#	preserved solely for educational purposes
#
#	*******************************************************************
#
#   Foreword to PC and Unix
#
#	This Makefile supposes Unix variant operating system. If you have
#	installed CygWin bash into your Windows PC, then you already know
#	what to do with this. If you just have plain PC and Emacs in it,
#	then forget this Makefile and glance couple of lines forward.
#
#	This Makefile's will:
#
#	o   compile .el files in proper order. You can do that as well
#	    from dired buffer. Start byte compiling from the libraries
#	    and make sure you have newest custom.
#	o   To create nice html documentation from .txt files, which you
#	    can then print to printer and read the pages with coffee near you
#
#	To use the emacs-lisp/ files right away without any fuzzing with this
#	Makefile do this (WindowsNT and PC Emacs versions in general)
#
#	o   If you dropped tarball to separate ~/elisp/tiny directory,
#	    update your load path to see it. Add this to your ~/.emacs.
#
#		(add-to-list 'load-path "~/elisp/tiny") ;; 19.34+
#
#	o   See doc/ema-tiny.txt for modules that you're interested in.
#	    Follow installation instructions per module and create
#	    ~/.emacs.tiny.el where you put the customisations and from
#	    where you `require' modules. Load that file from ~/.emacs  with
#
#		(load "~/.emacs.tiny")
#
#	     There is example setup emacs-rc/ema-tiny.ini that contains my
#	     personal setup for tiny*el files. It shuould give you a kick.
#
#	o   See various other exampe files named emacs-rc/*.ini
#
#	o   See doc/ files when you have extra time
#
#	o   Get perl 5 when you don't have anything better to do and
#	    convert doc/ files into .html so that you can print nice
#	    paperback copies. See *t2html.pl*
#
#   Requirements
#
#	You need Emacs 19.30+, XEmacs 19.15+ or never in order to use this
#	Lakefile and the Lisp files included in the kit.
#
#	Your private lisp files are supposed to reside in ~/elisp or
#	~/lisp. _Important_: Modify this fle to include relevant lisp
#	paths during compilation:
#
#	    emacs-lisp/load-path.el
#
#	Make a copy, modify and save it for later use; You need it when you
#	install new versions.
#
#   Quick start
#
#	Tired of reading Makefile instructions in text format? Do this now
#	and read the produced html with your favourite web browser.
#
#	    % env URL_BASE=`pwd` make -e -f *.mak ema-tiny.mak
#
#	If you don't care to read further, here is what you do. This only
#	works if a) You want to leave out newest custom.el support and c)
#	You want to install files to ~/elisp/tiny and your private Emacs
#	lisp packages reside in ~/elisp.
#
#	    % ln -s ~/elisp/tiny ~/junk	  # let link point to anywhere
#	    % cd ~/elisp/tiny-YYMMDD
#
#      default Emacs install command
#
#	    % make -f *.mak Makefile config all install-ln help list-ini
#
#      default XEmacs install command
#
#	    % make EMACS=xemacs -f *.mak \
#		  config all install-ln help list-ini
#
#	[compilation errors]
#
#	If there were compilation errors, use following to send the compile
#	log to the maintainer. Before doing this, investigate if they really
#	were errors: An *infomational* *message* is not an error.
#
#	    % make -f *.mak email-clog
#
#	Also send your current configuration (*.el version numbers) with
#
#	    % make -f *.mak email-ver
#
#   Makefile usage for starters
#
#	You run makefile by using standard `make' program. Before you do
#	anything else, please print help which will explain everyhing you
#	need to know. You can also pipe the output to `more' or `lpr'
#	programs if you don't have `less'.
#
#	    % make -f *.mak help | less
#
#	If you're installing current kit for the first time, select
#	rule `config' and after that build everything using rule `all'. You
#	can give multiple rules in the command line. Once you have run
#	`config', you don't need to run it again for the current
#	distribution.
#
#	    % make -f *.mak Makefile config all
#
#	Print also sample Emacs rc files to printer and have nice
#	cup of coffee near you when you read the _long_ output.
#
#	    % make print-ini | lpr
#
#	If you have updated some Emacs lisp file; select rule `elc'
#	to recompile files that are newer.
#
#	    % mak elc
#
#   Some lisp files are not compiled
#
#	The rule `elc' does not compile all lisp files. Some modules are
#	special and should be left in non-compiled state. Here is list
#	of files:
#
#	    tinyad.el	    - Advice collection
#	    tinyezip.el	    - Special .el.gz compress support module.
#
#   Compiling for different Emacs versions
#
#	Check that you have run `config' rule already. Change the *EMACS*
#	varaible if you want to compile for other Emacs version:
#
#	    % make EMACS=xemacs -f *mak elc
#
#	You can override any variable setting in the Makefile in similar
#	manner. Here the variable *EMACS* was substituted (default value in
#	the makefile is "emacs").
#
#   Compiling in custom.el support
#
#	_NOTE_: If you don't have Emacs 20+ or XEmacs 20.3+ I strongly suggest
#	that you download Noah Friedman's custom.el emulation package
#	*cust-stub.el*. Place it along the load-path that is defined
#	in *load-path.el*  and use following command compile lisp files:
#
#	    % grep ../doc/elisp.txt cust-stub
#	    % make EMACS=emacs-19.30 ELFLAGS='-l cust-stub' elc
#
#	If you insist having custom.el for old Emacs, read on. Emacs versions
#	19.34, XEmacs 19.15, XEmacs 20.2 don't have the latest custom.el
#	libraries. Please get the newest custom libraries first:
#
#	    % grep /custom ../doc/elisp.txt
#
#	If you have older Emacs than 19.34 or XEmacs 19.15/20.2, then you
#	can't use the new custom libraries and you should not compile the
#	custom code in. The new custom works only with newest Emacs
#	releases.
#
#	Please check that the *custom* distribution is compiled, it can't
#	be used otherwise. Custom package support is enabled for all
#	tiny*el packages, but it is not necessary to have custom in order
#	to _use_ or _compile_ files in this kit.
#
#	If you have latest custom, define new flags which instruct loading
#	custom. Best if you modify the included *load-path.el* and
#	uses it always for next compile sessions:
#
#	    % make  PATHFLAGS='-l ~/elisp/my-tiny-load-path.el' \
#		    ELFLAGS='-l custom.elc' \
#		    -f *.mak elc
#
#   Case study: Emacs 19.34 and private custom library
#
#	Suppose you have 19.28 and 19.30 and 19.34. Your private directory
#	is ~/elisp and you have installed the newest custom to
#	~/elisp/custom and you have byte compiled custom files. Now you
#	want to build all tiny*.el lisp files in the current distribution
#	directory using the newest custom support for 19.34. You have also
#	added the custom path to the beginning of the load path by
#	modifying the `load-path.el'. Like this:
#
#	    (dolist (path
#	      '(
#		;; Define any new path HERE
#		;;
#		"~/elisp/custom"
#		...
#
#	Here is the make command
#
#	    % make EMACS=emacs-19.34 ELFLAGS='-l custom.elc' -f *.mak elc
#
#   Using suffix rules
#
#	In this makefile there are some suffix rules that you may want to
#	use. Suffix rules mean, that the by chaging the file extension, an
#	alternative rule is used for that file. Here are *some* of the
#	rules for .el extensions
#
#	    .doc	Get documentation
#	    .html	Make html documentation from text or shell files.
#	    .htmlx	(extra) Make html documentation by reading file
#	    .prn	Display the file
#	    .ver	Print version information
#
#	For example if you want to generate html documentation out of *main*
#	library tinylib.el, you change the extension of the file name to match
#	a suffix rule. Here .el --> .html suffix rule is used:
#
#	    % make -f *.mak tinylib.html
#
#	To see available suffix rules, run Makefile with `rule' keyword.
#
#   Making html documentation from the source files.
#
#	By using the suffix rule .html you can rip the documentation out of
#	the file and convert it to html page. The top button of the page
#	always refers to the ssjaaa.html page, but you may want to change
#	that. The location of the url uses protocol `file' and the top
#	button points to file:/$INSTAL_DIR/ssjaaa.html
#
#	    % make -f *.mak tinyef.html
#
#	To change the location of the button, you would redefine
#	*URL_TOP_PAGE* variable, like this
#
#	    % make URL_TOP_PAGE=http://foo_localhost/emacs/tiny/index.html \
#	       -f *.mak html
#
#	And then write the index.html. It is not usually necessary to make
#	html documentation out of each emacs lisp file. The ema-tiny.txt
#	contains all documentation for each lisp file. You simply convert this
#	file into html.
#
#	    % make -f .mak ../doc/ema-tiny.html
#
#   Html documentation and correct url references
#
#	Making an html page is quite easy with the attached perl script,
#	but getting the links and [toc] references right, you must tell
#	where you're going to keep the html pages. The default variable
#	*URL_BASE* points to *INSTALL_DIR*, which is by default:
#
#	    /usr/local/share/emacs/site-lisp/tiny
#
#	If you're installing the pages somewhere else, eg to your
#	~/elisp/tiny directory, you must change URL_BASE like this
#
#	    % make URL_BASE=file:/users/foo/elisp/tiny \
#		   -f *.mak ema-tiny.htmlx
#
#	Here is short way to contruct the references so that they point
#	to the files in current directory. This uses rule *html* which
#	builds all files.
#
#	    % make URL_BASE=`pwd` -f *.mak html
#
#	As you noticed, we used direct *file:* access protocol. If you're
#	installing the files to local http server, then use appropriate
#	*http:* access protocol instead.
#
#   Distribution install
#
#	[rule install-ln]
#
#	After you have compiled all the files, there is couple of install
#	rules that you may want to use. The firat rule `install-ln' is the
#	most simple installation. It supposes that you always want to point
#	a directory symlink to the current distribution.
#
#	    ../tiny --> your_current_unpack_directory
#
#	This rule removes old link, that may be pointing to previous
#	distribution and re(sym)links current directory to *INSTALL_DIR_LINK*,
#	which defaults to `../tiny'.
#
#	[rule install-cp]
#
#	If you want to permanently move the files to another directory, use
#	this *copy* installation method. Beware, this runs rm -rf in your
#	*INSTALL_DIR* prior copying the files. But before it does that, it
#	gives you 20 second chance to cancel the operation before engaging
#	and destroying all below *INSTALL_DIR*. It may be possible that your
#	environment variable *INSTALL_DIR* had old gargage like "/". Hm;
#	so there!
#
#	Anyway, please do not set environment variables directly, but set
#	them through `env' program and you should be safe. Here is the
#	command to do the copy install
#
#	    % mkdir ~/elisp/tiny;    # Directory must exist first
#	    % make INSTALL_DIR=~/elisp/tiny -f *.mak install-cp
#
#   Lisp byte Compiler error notes
#
#	[at least appears in XEmacs 19.14] If you see this error in, don't
#	pay attention to it. It is a bug in cl-macs::ignore-errors
#	macro. In Emacs this message is not displayed.
#
#	    ** variable G3000 bound but not referenced
#
#	If you get following error, then you have somewhere Emacs and XEmacs
#	byte compiled files mixed. This may be due to some module that is
#	included during compilation from the load-path.el. There is no way
#	of telling which file causes the touble. Bets that you delete elc
#	from emacs-lisp/ directory and start over.
#
#	    !! Invalid read syntax (("#"))
#
#   Trouble shooting make errors
#
#	[Bad character > (octal 76), line 2Make: .  Stop]
#
#	Ooops, you propably run command make -f *.mak with the star
#	windcard. Please check that the *mak files in the directory
#	are really make files. You should find only one makefile for
#	this kit.
#
#	[sh-test] *** Error exit code 1
#
#	This means that your `test' program is not standard and can't
#	be used. Check your PATH and `where' `whereis' and `which' to
#	find our the correct `test' program. This Makefile cannot
#	be used until the you have correct test function. Try this to
#	see if the test works ok:
#
#	    % rm TT; touch T; test -e TT; echo $?; test -e T; echo $?
#
#	You should get results "1", not exist, and "0", exists. You can
#	change the program used in this makefilefile with familiar syntax:
#
#	    % make TEST=/usr/bin/gnu/test -f *.mak [... the rule list ...]
#
#   Code Notes
#
#	If you wonder why there is "echo > /dev/null" commands, they are
#	purely used for filling in the gap of the `then' or `else'
#	statements. They are no-op, but required in order to have the
#	`else' case. In found out that some sysmtems didn't allow
#	"!" tests, so I couldn't write
#
#	    if [ ! -e file ]; then
#		do
#	    fi
#
#	I had to circumvent it with this which equivalent, but not as elegant:
#
#	    if [ -e file ]; then
#		echo > /dev/null
#	    else
#		do
#	    fi

############################################################# &code ###

AUTHOR	    = "Jari Aalto"
EMAIL_TO    = ""

MAKEFILE    = makefile.mak
MAKEFILE1   = Makefile				# Generated

# ########################################################### &basic ###

SHELL	    =	/bin/sh
TEST	    =	/bin/test
SRCDIR	    =	.
EMACS	    =	emacs
EMACSVER    =	`${EMACS} -batch -version`


TT_KIT	    =	tiny-tools.zip
KIT	    =	$(TT_KIT)

DIR_PERL    =	../bin
DIR_DOC	    =	../doc
DIR_EMACS_RC=	../lisp/rc

# ######################################################### &install ###

#   These variables must not have a backslash at the end!
#   The INSTALL_DIR is forced to be empty so that user must use
#   `env INSTALL_DIR=xxx make -f *.mak install-cp'
#

INSTALL_DIR	    = /usr/local/share/emacs/site-lisp/tiny/
INSTALL_DIR_LINK    = ../tiny	# See rule `install-ln'
INSTALL_DIR_HTML    = $(INSTALL_DIR)

# ######################################################## &programs ###

URL_BASE    =	file:$(INSTALL_DIR_HTML)
URL_TOP_PAGE=	$(URL)/ssjaaa.html

HTML_PRG    =	$(DIR_PERL)/t2html.pl
HTML_OPTS   =	--name-uniq						    \
		--author $(AUTHOR)					    \
		--email "$(EMAIL_TO)"					    \
		--base	$(URL_BASE)					    \
		--button-top $(URL_TOP_PAGE)

HTML_DO	    =	$(HTML_PRG) $(HTML_OPTS)
HTML_EXT    =	.html
HTML_EXT2   =	.htmlx

FILE_DOC_MAKE =	$(DIR_PERL)/ripdoc.pl
FILE_DOC_PRG  =	$(DIR_PERL)/ripdoc.pl
MKMF	      =	$(DIR_PERL)/mkmf-emacs-lisp.pl

# ########################################################### &other ###

CLOG	    =	ema-compile.log
CLOG_TMP    =	$(CLOG).tmp

DIR	    =	`pwd`
DIR_UP	    =	` pwd | 's/\(.*\)\/.*/\1/' `
FTPDIR	    =

LIST_EXE    = `ls $(DIR_PERL)/*pl | sort`

RE_LISP	    = ema*ini *el
LIST_LISP   = `ls $(RE_LISP) | sort`
LIST_LISP_OTHER = `ls *el | egrep -v 'tiny|\.ini`

RE_TXT	    = *.txt *.gui
LIST_TXT    = `ls $(RE_TXT) | sort`
LIST_T2HTML = `ls  $(DIR_DOC)/*.gui $(DIR_DOC)/*.txt | egrep -v 'versio' | sort -u`

LIST_MISC   = `ls  |							    \
		 egrep -v '\.ini|\.el|\.pl|\.tar|\.gz|:$'		    \
		 | sort -u						    \
		`
# ########################################################### &flags ###

FLAG_IGERR  =	1  # If 0, then stop on error
LIBFLAGS    =	-l ./tinyliba.el -l ./tinylibb.el \
		-l ./tinylibm.el -l ./tinylib.el
INIFLAGS    =	-batch -q -no-site-file

# - The prefix "ELPATH" is in every path variable name to refer
#   to .el files.
# - this is the reason why you can't use old Emacs and this makefile,
#   the following code will fail in old emacs releases 19.2x

PATHFLAGS   =	-l ./load-path.el


#   XPATHFLAGS	: Extra paths that user may want to include
#   LIBFLAGS	: Internal to this makefile. Don't change.
#   ELFLAGS	: Additional modules loaded, like '-l FILE1 -l FILE2'

FLAGS	    = $(INIFLAGS) $(PATHFLAGS) $(XPATHFLAGS) $(LIBFLAGS) $(ELFLAGS)

# ############################################################## &tt ###

# TT = Tiny Tools

TT_EL_LIB	=			\
	tinyliba.el			\
	tinylibb.el			\
	tinylibm.el			\
	tinylib.el			\
	tinylibmt.el			\
	tinylibo.el			\
	tinyliby.el			\
	tinylibt.el			\
	tinylibxe.el			\
	tinylibid.el			\
	tinylibmenu.el			\
	tinylibck.el

#   Following files must not be compiled
#
#	tinyad.el	- Advice collection
#	tinyezip	- Special .el.gz compress support module.

TT_EL_TINY_NOCOMPILE =			\
	tinyad.el			\
	tinyezip.el

TT_EL_TINY =				\
	tinyappend.el			\
	tinybm.el			\
	tinycache.el			\
	tinybuffer.el			\
	tinychist.el			\
	tinycom.el			\
	tinycompile.el			\
	tinydesk.el			\
	tinydiff.el			\
	tinydired.el			\
	tinyeat.el			\
	tinyef.el			\
	tinyezip.el			\
	tinygnus.el			\
	tinyhotlist.el			\
	tinyigrep.el			\
	tinyindent.el			\
	tinylisp.el			\
	tinyload.el			\
	tinylock.el			\
	tinylpr.el			\
	tinymacro.el			\
	tinymail.el			\
	tinymbx.el			\
	tinymy.el			\
	tinynbr.el			\
	tinypad.el			\
	tinypage.el			\
	tinypair.el			\
	tinypath.el			\
	tinypgp.el			\
	tinyperl.el			\
	tinypm.el			\
	tinyreplace.el			\
	tinyrlog.el			\
	tinyrmail.el			\
	tinyscroll.el			\
	tinysearch.el			\
	tinytab.el			\
	tinytag.el			\
	tinytf.el			\
	tinyurl.el			\
	tinyxreg.el


TT_EL_OTHER_MODULES =			\
	alist.el			\
	c-comment-edit2.el		\
	complete-menu.el		\
	fnexpand.el			\
	dired-sort.el			\
	date-parse.el

#   Emacs Rc files (init file examples)

TT_EL_RC	=			\
	ema-bbdb.ini			\
	ema-bup.ini			\
	ema-ding.ini			\
	ema-path.ini			\
	ema-semi.ini			\
	ema-tigr.ini			\
	ema-tilo.ini			\
	ema-tiny.ini			\
	ema-tipgp.ini			\
	ema-tm.ini


#   Text files

TT_TEXT	=				\
	$(DIR_DOC)/GNU-license.txt	\
	$(DIR_DOC)/OLD.txt		\
	$(DIR_DOC)/ema-expect.txt	\
	$(DIR_DOC)/bookmark.txt		\
	$(DIR_DOC)/t2html.txt		\
	$(DIR_DOC)/elisp.txt		\
	$(DIR_DOC)/ema-code.gui		\
	$(DIR_DOC)/ema-font.gui		\
	$(DIR_DOC)/ema-keys.gui		\
	$(DIR_DOC)/ema-pkg.gui		\
	$(DIR_DOC)/ema-tipgp.txt	\
	$(DIR_DOC)/pm-tips.txt		\
	$(DIR_DOC)/pm-code.txt		\
	$(DIR_DOC)/ema-tiny.txt		\
	$(DIR_DOC)/ema-tiny.mak		\
	$(DIR_DOC)/versions.txt


TEXT2HTML =				\
	$(DIR_DOC)/OLD.txt		\
	$(DIR_DOC)/bookmark.txt		\
	$(DIR_DOC)/t2html.txt		\
	$(DIR_DOC)/elisp.txt		\
	$(DIR_DOC)/ema-code.gui		\
	$(DIR_DOC)/ema-font.gui		\
	$(DIR_DOC)/ema-keys.gui		\
	$(DIR_DOC)/ema-pkg.gui		\
	$(DIR_DOC)/ema-tiny.txt		\
	$(DIR_DOC)/ema-tipgp.txt	\
	$(DIR_DOC)/pm-tips.txt		\
	$(DIR_DOC)/pm-code.txt

README	= ../README.txt


TT_EL_TINY_COMPILE = $(TT_EL_LIB) $(TT_EL_TINY)
TT_EL_TINY_ALL	= $(TT_EL_TINY_COMPILE) $(TT_EL_TINY_NOCOMPILE)
SRCS_ALL	= $(TT_EL_TINY_ALL) $(TT_EL_OTHER_MODULES)
SRCS		= $(TT_EL_TINY_COMPILE) $(TT_EL_OTHER_MODULES)

OBJS		= $(SRCS:.el=.elc)
HTML		= $(TEXT2HTML:.txt=$(HTML_EXT)) $(TEXT2HTML:.gui=$(HTML_EXT))

# ########################################################## &suffix ###
#==== For make

.SUFFIXES:
.SUFFIXES: .el .txt .gui .pl .mak   .ver .elc .html .doc .prn .std .htmlx

# .SILENT:

#==== For mkmf
# This defines (s)ource and (o)bject files

SUFFIX	    = .el:h .el:s .elc:o
SUFFIX_KILL = *.elc *.prn *.doc *.html *.htmlx


# Rule: suffix .el  => .elc - Compile Emacs lisp file
.el.elc:
	@r="[.el.elc]";							    \
	echo "Compiling $< ...";					    \
	if ( $(TEST) -e $< ); then					    \
	    if (  $(TEST) $(FLAG_IGERR) -ne 0 ); then			    \
		( $(EMACS) $(FLAGS) -f batch-byte-compile $< |		    \
		  tee $(CLOG_TMP));					    \
	    else							    \
		$(EMACS) $(FLAGS) -f batch-byte-compile $< |tee $(CLOG_TMP);\
	    fi ;							    \
	    cat $(CLOG_TMP) >> $(CLOG);					    \
	else								    \
	    echo "  Hm. This file does not exist. Skipped.";		    \
	fi


# ........................................................ std print ...

# Rule: suffix .el  => .ver - Print version Id string to stdout
.el.ver:
	@awk '/Id:/ {print; exit}' $<;

# Rule: suffix .el  => .prn - Print file to stdout
.el.prn:
	@echo "-------------------------------------------------"
	@echo "File: $<"
	@echo "-------------------------------------------------"
	@cat $<


# Rule: suffix .gui => .prn - Print file to stdout
.gui.prn:
	@echo "\n--  #FILE: $< --------------------------\n"
	@cat $<


# Rule: suffix .txt => .prn - Print file to stdout
.txt.prn:
	@echo "\n--  #FILE: $< --------------------------\n"
	@cat $<

# ...................................................... get raw doc ...

# Rule: suffix .pl => .doc - Grab documentation from file
.pl.doc:
	$(FILE_DOC_PRG) $<

# Rule: suffix .mak => .doc - Grab documentation from file
.mak.doc:
	$(FILE_DOC_MAKE) $<

.mak.doc:
	$(FILE_DOC_MAKE) $<

# Rule: suffix .el  => .doc - Grab documentation from file
.el.doc:
	@$(FILE_DOC_PRG) $< ;

# ............................................................. html ...

# Rule: suffix .el  => .html - Grab documentation and make html
.el.html:
	@echo "$< --> $*$(HTML_EXT)"
	@$(FILE_DOC_PRG) $< | $(HTML_DO) > $*$(HTML_EXT) ;

# Rule: suffix .txt => .html - Make html out of .txt file
.txt.html:
	@echo "$< --> $*$(HTML_EXT)"
	@$(HTML_DO)  $< > $*$(HTML_EXT)	;

# Rule: suffix .gui => .html - Make html out of .gui txt file
.gui.html:
	@echo "$< --> $*$(HTML_EXT)"
	@$(HTML_DO)  $< > $*$(HTML_EXT)	;

.pl.html:
	echo "$< --> $*$(HTML_EXT)"
	$(FILE_DOC_PRG) $< | $(HTML_DO) > $*$(HTML_EXT)	 ;

.mak.html:
	echo "$< --> $*$(HTML_EXT)"
	$(FILE_DOC_PRG) $< | $(HTML_DO) > $*$(HTML_EXT)	 ;


# Rule: suffix .mak => .htmlx - Make html out of *.mak file
.mak.htmlx:
	echo "$< --> $*$(HTML_EXT2)";
	$(FILE_DOC_PRG) $< |						    \
	$(HTML_DO) -base $(URL_BASE)/$*$(HTML_EXT2)			    \
	> $*$(HTML_EXT2) ;


# ############################################################ &rules ###

# Rule: all - Make and Compile all. Be sure to use rule `config' first
all:	sh-test elc html


# Rule: elc - Compile all Emacs lisp files in the distribution
#
#	v=`emacs19.34.motif -batch -version`
#	echo $v | awk '/19\.2|19\.14/ {print 1; exit}{print 0}'

elc:	$(OBJS:.el=.elc)
	@r="[elc]";							    \
	echo $$r Emacs Lisp file compilation done: $(EMACSVER);		    \
	echo $$r Compile base  directory: `pwd`;			    \
	echo $$r Used compile flags $(FLAGS);


#   This makefile didn't work in previously. now it does.
#   But here is the test used back then. May be handy later
#   for some other Emacs version.

elc-compile-check:
	@r="[elc-compile-check]";					    \
	v=$(EMACSVER);							    \
	if [ `echo $$v |						    \
	      awk '/19\.2/ {print 1; exit 1}{exit 0}'			    \
	     `								    \
	   ]; then							    \
	    echo "$$r Can't use Makefile; too old Emacs: $$v.";		    \
	    exit 1;							    \
	fi


#===	Other

# Rule: clobber - Remove all files that can be regenerated.
# Rule: clobber - ...You should run rule `makefile' after this.

clobber:    clean clean-tmp
	- rm -f								    \
	$(SUFFIX_KILL)							    \
	$(MAKEFILE1)

# Rule: clean - Remove compiled lisp files
clean:
	- rm -f *.elc

# Rule: clean - Remove compiled lisp files
realclean: clobber
	echo realclean done.

# Rule: clean-kit - Remove distribution kit: tgz and the like.
clean-kit:
	- rm -f $(KIT)

# Rule: clean-tmp - Remove temporary files
clean-tmp:	clog-clean

# ##################################################### &rule-others ###

# Rule: config - Configure and prepare distribution to current environment.
config:		configure
configure:	sh-test fix clobber makefile

clog-clean:
	r="[clog-clean] $(CLOG)"; echo $$r;				    \
	if ( $(TEST) -e $(CLOG) ); then					    \
	   rm $(CLOG);							    \
	fi

# Rule: email-clog - Email recent compile log to the Author of the kit
email-clog:
	@r="[clog-email]";						    \
	if ( $(TEST) -f $(CLOG) ); then					    \
	    mail $(EMAIL_TO) < $(CLOG);					    \
	    echo Compile information email set.				    \
	else								    \
	    echo "$$r No $(CLOG) to send to $(EMAIL)";			    \
	fi

# Rule: email-ver - Email package versions to the Author of the kit
email-ver:
	make -f $(MAKEFILE) ver-lisp | mail $(EMAIL_TO);
	@echo version information email sent.


# Rule: fix - Adjust the executable programs to current OS (perl/perl5 etc.)
#
#   Fix Perl intepreter in this host. Do not mind the Make run messages
#
#	Spurious backslash ignored at /tmp/perl-ea27676 line 1
#	Spurious backslash ignored a...
#
fix:
	echo								    \
	  cd $(DIR_PERL);						    \
	  perl -i.bak -p						    \
	  -e 'chop ( $$prg = `which perl` ) if $$. == 1;'		    \
	  -e 's,/bin/perl5,/bin/perl, ; s,/usr/local/bin/perl,$$prg, ;'	    \
	  $(DIR_PERL)/*pl ;
	rm -f $(DIR_PERL)/*.bak

# Rule: help - Print distribution help
help:	print-readme print-make rule

# Rule: htm - move .html files to .htm extension
htm:
	@for file in `ls *.html`;					    \
	do								    \
	    if ( $(TEST) -d $$file ); then				    \
		echo > /dev/null;					    \
	    else							    \
		name=`echo $$file | sed 's/.html/.htm/' ` ;		    \
		echo "$$file --> $$name";				    \
		mv $$file $$name;					    \
	    fi								    \
	done


# Rule: html - Generate html pages out of the text and SRCS documents.
html:	$(HTML)


# Rule: ident - Grep ident(1) regular expressions from all files.
# ident(1) dies if there is directory as argument. We have to use for loop.

ident:
	-prg=`which ident`; yes=`expr "$$prg" : "no"`;			    \
	echo ident status $$yes;					    \
	for file in `ls`;						    \
	do								    \
	if ( $(TEST) -r $$file && $(TEST) -f $$file ); then		    \
	    if ( $$yes ); then						    \
		ident $$file;						    \
	    else							    \
		echo $$file;						    \
		grep '\$$[A-Za-z].* \$$$$' $$file;			    \
	    fi								    \
	exit;								    \
	fi								    \
	done

# Rule: install - List Install choices
install:
	    @echo select install-ln or install-cp

# Rule: install-ln - softlink current dir to INSTALL_DIR_LINK
install-ln:
	r="[install-ln]";						    \
	if ( $(TEST) -e $(INSTALL_DIR_LINK) ); then			    \
	    if ( $(TEST)  -h $(INSTALL_DIR_LINK) ); then		    \
		 dir=`pwd`;						    \
		 rm $(INSTALL_DIR_LINK);				    \
		 ln -s $$dir $(INSTALL_DIR_LINK);			    \
		 echo linked  $$dir to $(INSTALL_DIR_LINK);		    \
	    else							    \
		echo "$$r $(INSTALL_DIR_ABOVE) is not a softlink. Skipped.";\
	    fi								    \
	else								    \
	    echo "$$r $(INSTALL_DIR_LINK) does not exist. Rule skipped.";   \
	fi

# Rule: install-check - Creating install directory INSTALL_DIR if needed
install-check:
	if (  $(TEST) "X$(INSTALL_DIR)" = "X" ); then			    \
	    echo "$rr INSTALL_DIR is not defined."; exit 1;		    \
	else								    \
	if ( $(TEST) -e $(INSTALL_DIR) ); then				    \
	    if ( $(TEST) -d $(INSTALL_DIR) ); then			    \
		echo > /dev/null;					    \
	    else							    \
		echo "$$r $(INSTALL_DIR) is not a directory."  exit 1 ;	    \
	    fi								    \
	else								    \
	    echo "$$r $(INSTALL_DIR) does not exist.";			    \
	    echo Making one within 10 seconds, precc C-c to abort;	    \
	    sleep 10;							    \
	    mkdir $(INSTALL_DIR);					    \
	fi								    \
	fi

# Rule: install-clean: Clean all unnecessary files (tgz) from INSTALL_DIR
install-clean: install-check
	r="[install-clean]"; echo $$r;
	- ( cd $(INSTALL_DIR); rm -f $(KIT); )

# Rule: install-cp - Copy files from current directory to INSTALL_DIR
install-cp: install-check
	r="[install-cp]";
	echo "20sec chance to press C-c before rm -rf $(INSTALL_DIR)/*";
	sleep 20;
	echo "$$r Engaged..."
	rm -rf	 $(INSTALL_DIR)/*;
	cp -rf * $(INSTALL_DIR);
	echo "$$r done.";


# Rule: list - List compile source modules.
list:
	@for file in  $(SRCS);						    \
	do								    \
	    echo $$file;						    \
	done

# Rule: list-ini - List all included Emacs startup file examples.
list-ini:
	@cd $(DIR_EMACS_RC); ls $(TT_EL_RC) | sort

# Rule: list-lisp - List all Emacs elisp files in dir, including rc files
list-lisp:
	@ls $(RE_LISP) | sort


# Rule: list-lisp-tiny -  List elisp in dir, all ti* modules
list-lisp-tiny:
	@ls tiny*el  | sort

# Rule: list-lisp-other - List elisp in dir, all except ti* modules
list-lisp-other:
	@ls *el | egrep -v 'tiny|\.ini'	 | sort


# Rule: list-txt2html - List all files that can be converted to html
list-txt2html:
	@for file in $(LIST_T2HTML) ;					    \
	do								    \
	    if ( $(TEST) -d $$file ); then				    \
		echo > /dev/null;					    \
	    else							    \
		echo $$file ;						    \
	    fi								    \
	done

# Rule: makefile - make Makefile (softlink to distribution's .mak file)
makefile:
	@if ( $(TEST) -f $(MAKEFILE1) ); then				    \
	    echo .. There is already $(MAKEFILE1),			    \
	    Ignoring rule [makefile];					    \
	else								    \
	    ln -s $(MAKEFILE) $(MAKEFILE1);				    \
	    echo created $(MAKEFILE1);					    \
	fi

# rule: mkmf: update Emacs lisp targets in the Makefile
mkmf:
	$(MKMF) -f $(MAKEFILE)

# Rule: print - Catenate all files and print to stdout
print: print-el print-sh print-ini

# Rule: print-el - Catenate and Print all Emacs package files
print-el:
	@for file in $(LIST_LISP) ;					    \
	do								    \
	  echo "--------------------------------------------------" ;	    \
	  echo "#FILE: $$file" ;					    \
	  echo "--------------------------------------------------" ;	    \
	  cat $$file ;							    \
	done


# Rule: print-sh - Catenate and Print all script files to stdout
print-sh:
	@for file in $(LIST_EXE) ;					    \
	do								    \
	  echo "--------------------------------------------------" ;	    \
	  echo "#FILE: $$file" ;					    \
	  echo "--------------------------------------------------" ;	    \
	  cat $$file ;							    \
	done

# Rule: print-ini - Print Emacs rc files (sample Emacs initialize files)
print-ini:
	@for file in `ls $(DIR_EMACS_RC)/ema*ini | sort` ;		    \
	do								    \
	  echo "--------------------------------------------------" ;	    \
	  echo "#FILE: $$file" ;					    \
	  echo "--------------------------------------------------" ;	    \
	  cat $$file ;							    \
	done

# Rule: print-make - Print makefile instructions
print-make: $(MAKEFILE:.mak=.doc)

# Rule: print-readme - Print readme file
print-readme: $(README:.txt=.prn)

# Rule: print-txt - Catenate and Print all text files stdout
print-txt: $(README:.txt=.prn)
	   @for file in $(LIST_TXT);					    \
	    do								    \
	      echo "--------------------------------------------------" ;   \
	      echo "#FILE: $$file" ;					    \
	      echo "--------------------------------------------------" ;   \
	      cat $$file ;						    \
	    done


rule:
	@echo "\n--  #FILE: $(MAKEFILE) rules --------------------------\n"
	@grep 'Rule\: ' $(MAKEFILE) | grep -v maint | sed -e 's/^./ /'
	@echo ""

# Rule: sh-test - Check if program 'test' is valid.
sh-test:
	echo "Testing if your test(1) is working correctly...";		    \
	r="[sh-test]"; f=/tmp/test$$$$.tmp;				    \
	touch $$f;							    \
	echo "$$r 'test' ... $(TEST) -e $$f";				    \
	$(TEST) -e $$f;

# Rule: tags - Run etags on SRCS (lisp) files
tags:
	@etags $(SRCS_ALL)


# Rule: ver - Output all version numbers or date information found.
#
# We use `file' because we only want to get info from text files.
# This may be slower, but at least awk won't die trying to grep
# binary (causes overflow)

ver:
	for file in `file * | grep 'ascii text' | sed 's/:.*//'`;	    \
	do								    \
	if ( $(TEST)  -r $$file ); then					    \
	awk '								    \
	    { gsub("^;+", ""); gsub("@.#.", "");			    \
	      gsub("^[ \t]", "");					    \
	    }								    \
	    /\$$\Docid:/{ printf("%s:\n\t%s\n", FILENAME, $$0);exit; }	    \
	    /\$$\Id:/	 { printf("%s:\n\t%s\n", FILENAME, $$0);exit; }	    \
	    ' $$file;							    \
	fi								    \
	done


ver-lisp:  $(SRCS:.el=.ver)


# Rule: what - Grep what(1) regular expressions from each file. [brief doc]
#
# I give '*' to what it complaints about directories and exists with 1.
# The "-@" ignores this exit behavior
#
# The "grep . " will find something if program exists. If program
# does not exist, the shell will say "what: Command not found" and
# print to stderr, which is not captured by grep.

what:
	-@prg=`what /dev/null| grep .`;					    \
	if ( $(TEST) "X$$prg" = "X" ); then				    \
	    grep '@.#.' * ;						    \
	else								    \
	    what *;							    \
	fi

# Rule: wc - Run wc (lines;characters) on SRCS files and sort by size
wc:
	@wc -cl $(SRCS_ALL) | sort +n

# Rule: wc-txt - Run wc (lines;characters) on TT_TEXT files and sort by size
wc-txt:
	@wc -cl $(TT_TEXT) | sort +n


# ##################################################### &maintenance ###
# There are only for the maintainer or the package
#

# Rule: depend - [maint] Make compilation dependencies
depend:
	$(MKMF) -f $(MAKEFILE) *el

# Rule: exist - [maint] Display files that do not exist in current dir
# Search broken symlinks
exist:
	@for file in `ls` ;						    \
	do								    \
	if [ -d $$file ]; then						    \
	    echo > /dev/null;						    \
	else								    \
	    if [ -f $$file ]; then					    \
		echo > /dev/null;					    \
	    else							    \
		echo $$file;						    \
	    fi								    \
	fi								    \
	done

# Rule: maint - [maint] Display maintenance rules
maint:
	@grep '#.*Rule\:.*maint' $(MAKEFILE) | sed -e 's/^./ /'



# Rule: list-sym -  [maint] List symlink files
list-sym:
	@for file in `ls` ;						    \
	do								    \
	if [ -d $$file ]; then						    \
	    echo > /dev/null;						    \
	else								    \
	    echo $$file;						    \
	fi								    \
	done


# Rule: list-nsym -  [maint] List non symlink files (do `clobber' first)
list-nsym:
	@for file in `ls` ;						    \
	do								    \
	if [ ! -d $$file ]; then					    \
	    if [ ! -h $$file ]; then					    \
		echo $$file ;						    \
	    fi								    \
	fi								    \
	done

# Rule: soft - [maint] make softlink one dir up for every SRCS file.
soft:
	@for file in $(SRCS_ALL);					    \
	do								    \
	if [ -f ../$$file ]; then					    \
	    if [ ! -f $$file ]; then					    \
		echo "linking $$file...";				    \
		ln -s ../$$file $$file;					    \
	    else							    \
		if [ ! -h $$file ]; then				    \
		    echo "   Regular file: $$file";			    \
		fi							    \
	    fi								    \
	else								    \
	    echo "No ../$$file";					    \
	fi								    \
	done

# Rule: soft - [maint] Test if all kit files exist before building kit.
test:
	@for file in $(SRCS_ALL) $(TT_TEXT);				    \
	do								    \
	if [ ! -f $$file ]; then					    \
	    echo $$file missing;					    \
	fi								    \
	done


# #################################################### &dependencies ###

#   $(MKMF) Perl script writes after this '###' tag
#   It is a standard mkmf(1) tag
#
#	Y O U	 MUST NOT EDIT THIS BY HAND
#
#   use makefile rule instead:
#
#	/home/foo/elisp/ % make mkmf

###


tinyad.elc:  \
	tinylibm.el


tinyappend.elc:	 \
	tinylibm.el


tinybm.elc:  \
	tinylibm.el


tinycache.elc:	\
	tinylibm.el


tinycb.elc:  \
	tinylibm.el


tinychist.elc:	\
	tinylibm.el


tinycom.elc:  \
	tinylibm.el


tinycompile.elc:  \
	tinylibm.el


tinydesk.elc:  \
	tinylibm.el


tinydiff.elc:  \
	tinylibm.el	    \
	tinymy.el


tinydired.elc:	\
	tinylibm.el


tinyeat.elc:  \
	tinylibm.el


tinyef.elc:  \
	tinylibm.el


tinyezip.elc:

tinygnus.elc:  \
	tinylibm.el


tinyhotlist.elc:  \
	tinylibm.el


tinyigrep.elc:	\
	tinylibm.el	    \
	tinycache.el


tinyindent.elc:	 \
	tinylibm.el


tinylib.elc:  \
	tinylibm.el


tinyliba.elc:

tinylibb.elc:  \
	tinyliba.el


tinylibck.elc:

tinylibid.elc:	\
	tinylibm.el


tinylibm.elc:  \
	tinylibb.el


tinylibmenu.elc:  \
	tinylibm.el


tinylibmt.elc:	\
	tinylibm.el


tinylibo.elc:  \
	tinylibm.el


tinylibt.elc:  \
	tinylibm.el


tinylibxe.elc:	\
	tinylibm.el


tinyliby.elc:  \
	tinylibm.el


tinylisp.elc:  \
	tinylibm.el


tinyload.elc:  \
	tinylibm.el	    \
	tinyload.el


tinylock.elc:  \
	tinylibm.el


tinylpr.elc:  \
	tinylibm.el


tinymacro.elc:	\
	tinylibm.el


tinymail.elc:  \
	tinylibm.el	    \
	tinylibmt.el


tinymbx.elc:  \
	tinylibm.el


tinymy.elc:  \
	tinylibm.el


tinynbr.elc:  \
	tinylibm.el


tinypad.elc:  \
	tinylibm.el


tinypage.elc:  \
	tinylibm.el


tinypair.elc:  \
	tinylibm.el


tinypath.elc:

tinyperl.elc:  \
	tinylibm.el


tinypgp.elc:  \
	tinylib.el	    \
	tinylibmt.el	    \
	tinylibm.el


tinypm.elc:  \
	tinylibm.el


tinyreplace.elc:  \
	tinylibm.el


tinyrlog.elc:  \
	tinylibm.el


tinyrmail.elc:	\
	tinylibm.el


tinyscroll.elc:	 \
	tinylibm.el


tinysearch.elc:	 \
	tinylibm.el


tinytab.elc:  \
	tinylibm.el


tinytag.elc:  \
	tinylibm.el


tinytf.elc:  \
	tinylibm.el


tinyurl.elc:  \
	tinylibm.el


tinyxreg.elc:  \
	tinylibm.el

#
# ema-tiny.mak ends here.
