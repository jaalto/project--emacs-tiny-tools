#!/bin/bash
#
#   File id
#
#       Copyright (C) 2000-2010 Jari Aalto
#
#       This program is free software; you can redistribute it and/or
#       modify it under the terms of the GNU General Public License as
#       published by the Free Software Foundation; either version 2 of
#       the License, or (at your option) any later version.
#
#       This program is distributed in the hope that it will be useful, but
#       WITHOUT ANY WARRANTY; without even the implied warranty of
#       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#       General Public License for more details.
#
#       You should have received a copy of the GNU General Public License
#       along with this program. If not, see <http://www.gnu.org/licenses/>.
#
#       Visit <http://www.gnu.org/copyleft/gpl.html> for more information
#
#   Documentation
#
#       These bash functions will help managing the project. You need:
#
#       bash        (Unix)  http://www.fsf.org/directory/bash.html
#                   (Win32) http://www.cygwin.com/
#       Perl 5.4+   (Unix)  http://www.perl.org/
#                   (Win32) Perl is included in Cygwin tools
#       t2html.pl   Perl text to HTML converter
#                   http://freshmeat.net/projects/perl-text2html
#
#       This file is for the Admin or Co-Developer of the project.
#       Download the project and Web pages with:
#
#	git clone git://git.savannah.nongnu.org/emacs-tiny-tools.git \
#	    emacs-tiny-tools.git emacs-tiny-tools.git
#
#	cvs -d :ext:<login>@cvs.sv.gnu.org:/web/emacs-tiny-tools \
#	    co emacs-tiny-tools-www.cvs
#
#       Include this file to your $HOME/.bashrc and make the necessary
#       modifications
#
#           PROJ_TINY_TOOLS_USER=<login-name>
#           PROJ_TINY_TOOLS_USER_NAME="FirstName LastName"
#           PROJ_TINY_TOOLS_ROOT=~/projects/emacs-tiny-tools
#
#           source ~/projects/emacs-tiny-tools/bin/admin.bashrc
#
#       Functions related to release maintenance contain underscore (_)
#       in function name.

VERSION="2008.0915.2131"

function prjttinit ()
{
    local id="prjttinit"

    PROJ_TINY_TOOLS_ROOT=${PROJ_TINY_TOOLS_ROOT:-"."}

    if [ ! "$PROJ_TINY_TOOLS_USER" ]; then
       echo "$id: Identity PROJ_TINY_TOOLS_USER unknown."
    fi

    if [ "$PROJ_TINY_TOOLS_USER_NAME" ]; then
       echo "$id: Identity PROJ_TINY_TOOLS_USER_NAME unknown."
    fi
}

function prjttdate ()
{
    date "+%Y.%m%d"
}

function prjtttime ()
{
    date "+%Y-%m-%d %H:%M:%S"
}

function prjttfilesizeAwk ()
{
    # This was old implementation. Found better bash solution.
    ls -la $1 | awk '{print $5}'
}

function prjttfilesize ()
{
    #   put line into array ( .. )

    local line
    line=($(ls -l "$1"))

    #   Read 4th element from array
    #   -rw-r--r--    1 root     None         4989 Aug  5 23:37 file

    echo ${line[4]}
}

function prjttask ()
{
    #   Ask question from user. RETURN answer is "no".

    local msg="$1"
    local answer
    local junk

    echo "$msg" >&2
    read -e answer junk

    case $answer in
        Y|y|yes)    return 0 ;;
        *)          return 1 ;;
    esac
}

function prjtt_copyhtml ()
{(
    #  Copy files from CURRENT_ROOT to ../emacs-tiny-tools-www.cvs

    cd ${PROJ_TINY_TOOLS_ROOT:-"."} &&
    find . -type f -name "*.html" |
    rsync ${test:+"--dry-run"} \
      --files-from=- \
      --update \
      --progress \
      --verbose \
      -r \
      . \
      ../../../emacs-tiny-tools-www.cvs/
)}

function prjtthtml ()
{
    #   To generate HTML documentation located in /doc directory, call
    #
    #       bash$ prjtthtml <FILE.txt>
    #
    #   To generate Frame based documentation
    #
    #       bash$ prjtthtml <FILE.txt> --html-frame
    #
    #   For simple page, like README.txt
    #
    #       bash$ prjtthtml <FILE.txt> --as-is

    local id="prjtthtml"

    local input="$1"
    shift

    if [ ! "$input" ]; then
        echo "id: usage is FILE [html-options]"
        return
    fi

    if [ ! -f "$input" ]; then
        echo "$id: No file found [$input]"
        return
    fi

    local opt=$*

    echo "Htmlizing .. $id $input $opt"

    perl -S t2html                                                  \
          $opt                                                      \
          --author "$PROJ_TINY_TOOLS_USER_NAME"                     \
          --url    "http://tiny-tools.sourceforge.net"              \
	  --Auto-detect                                             \
          --Out                                                     \
          $input

    prjtt_movehtml
}

function prjtthtmlall ()
{
    local id="prjtt_htmlall"

    #   loop all *.txt files and generate HTML

    local dir=$PROJ_TINY_TOOLS_ROOT/doc/html
    local file

    while read file
    do
	prjtthtml $file

    done < $(find $dir -type f -name "*.txt")

    echo "$id: done."
}

function prjtt_nice_text ()
{
    local id="prjtt_nice_text"
    local file=$1

    #  Add "Nice looking" option to the generated file.

    local tmp=$file.new

    perl -e '
        @all = <>;
        $key="#T2HTML-OPTION";
        $val="--css-code-bg";
        unless ( grep /$key\s+$val/, @all )
        {
            unshift @all, "$key $val\n";
        }
        print @all
    ' $file > $tmp

    mv $tmp $file

    echo "$id: $file done."
}

function prjtt_tinypath_doc ()
{
    # Update documentation that is generated form original files.

    local id="prjtt_doc"

    local bin=$PROJ_TINY_TOOLS_ROOT/bin
    local doc=$PROJ_TINY_TOOLS_ROOT/doc/html/tinypath
    local lisp=$PROJ_TINY_TOOLS_ROOT/lisp/tiny

    local cmd="perl $bin/ripdoc.pl $lisp/tinypath.el"
    local to="index.txt"

    echo "$cmd > $to"

    $cmd > $to

    dummy=$(prjtt_nice_text $to)

    echo "$id: done."
}

function prjtt_docLispManual ()
{
    local id="prjtt_docLispManual"

    #   Rip all documentation from lisp files
    #   and update tiny-tools.txt

    local bin=$PROJ_TINY_TOOLS_ROOT/bin
    local dir=$PROJ_TINY_TOOLS_ROOT/lisp/tiny
    local outDir=$PROJ_TINY_TOOLS_ROOT/doc/html/manual

    local out=emacs-tiny-tools
    local out1=$out-part1.src
    local out2=$out-part2.src
    local final=$out.txt

    local cmd="cd $dir && perl -S $bin/ripdoc.pl"
    local files='$(ls *.el | sort)'

    (
        echo "$id: $cmd $files > $out2"
        eval "$cmd $files > $out2"
    )

    (
        cmd="cd $outDir && cat $out1 $out2";
        echo "$id: $cmd > $final"
        eval "$cmd > $final"
    )

    echo "$id: $final done."
}

function prjtt_linkcheck ()
{
    # Check if the URL links are valid

    local id="prjtt_linkcheck"

    local dir="$PROJ_TINY_TOOLS_ROOT/doc/html"
    local cache="$HOME/tmp/prjtt-link.cache"
    local log="$HOME/tmp/prjtt-link.log"

    if [ -f "$cache" ] && prjttask "$id: Remove $cache (y/[n])?"
    then
        echo "$id: rm $cache"
        rm $cache
    fi

    local cmd="perl -S t2html.pl --Link-check-single \
 --quiet --Link-cache $cache"


    local date=$(prjtttime)

    cat >> $log <<EOF
$date Link check $dir
-----------------------------------------------------------
EOF

    (
        cd $dir

        for file in *.txt;
        do
            echo "$id: cd $dir && $cmd $dir/$file | tee $log"
            $cmd $dir/$file | tee -a $log
        done
    )

    echo "$id: Done. Results in $log"
}

function prjtt_release_check ()
{
    #   Remind that that everything has been prepared
    #   Before doing release

    if prjttask '[prjtt_doc] Generate tinypath docs (y/[n])?'
    then
        echo "Running..."
        prjtt_doc
    fi

    if prjttask '[prjtt_docLispManual] Generate complete lisp docs (y/[n])?'
    then
        echo "Running..."
        prjtt_docLispManual
    fi

    if prjttask '[prjtt_linkcheck] Run URL link check? (y/[n])'
    then
        echo "Running..."
        prjtt_linkcheck
    fi

    if prjttask '[prjtt_linkcheck] Convert all text files to HTML? (y/[n])'
    then
        echo "Running..."
        prjtt_htmlall
    fi

    if prjttask '[prjtt_linkcheck] Upload HTML to sourceforge? (y/[n])'
    then
        echo "Running..."
        prjttscp_all
    fi
}

function prjtt_release ()
{
    local id="prjtt_release"

    #   TYPE is tgz  bz2  or zip

    local type=$1
    local opt=-9
    local cmd=""
    local ext1=""
    local ext2=""


    case $type in
     tar.gz|tgz|gz) type=tar
                    ext1=.tar
                    ext2=.gz
                    cmd=gzip
                    ;;
         bz|bz2)    type=tar
                    ext1=.tar
                    ext2=.gz
                    cmd=bzip2
                    ;;
            zip)    type=zip
                    ext1=.zip
                    opt="-9 -r"
                    cmd=zip
                    ;;
             *)     echo "$id: ERROR, unknow release type [tgz|bz2|zip]"
                    return 1
                    ;;
    esac

    prjtt_release_check

    local dir=/tmp

    if [ ! -d $dir ]; then
        echo "$id: Can't make release. No directory [$dir]"
        return
    fi

    if [ ! -d "$PROJ_TINY_TOOLS_ROOT" ]; then
        echo "$id: No PROJ_TINY_TOOLS_ROOT [$PROJ_TINY_TOOLS_ROOT]"
        return
    fi


    local base=emacs-tiny-tools
    local ver=$(prjttdate)
    local tar="$base-$ver$ext1"
    local file="$base-$ver$ext1$ext2"

    if [ -f $dir/$file ]; then
        echo "$id: Removing old archive $dir/$file"
        ( rm $dir/$file )
    fi

    (


        local todir=$base-$ver
        local tmp=$dir/$todir

        if [ -d $tmp ]; then
            echo "$id: Removing old archive directory $tmp"
            rm -rf $tmp
        fi


        cp -r $PROJ_TINY_TOOLS_ROOT $dir/$todir

        cd $dir

        if [ "$type" = "tar" ]; then

            find $todir -type f                     \
                \( -name "*[#~]*"                   \
                   -o -name ".*[#~]"                \
                   -o -name ".#*"                   \
                   -o -name "*elc"                  \
                   -o -name "*tar"                  \
                   -o -name "*gz"                   \
                   -o -name "*bz2"                  \
                   -o -name .cvsignore              \
                \) -prune                           \
                -o -type d \( -name CVS \) -prune   \
                -o -type f -print                   \
                | xargs tar cvf $dir/$tar

            echo "$id: Running $cmd $opt $dir/$tar"

            $cmd $opt $dir/$tar

        elif [ $type = "zip" ]; then

            $cmd $opt $dir/$file $todir         \
            -x  \*/CVS/\* CVS/\*                \
                .cvsignore \*/.cvsignore        \
                \*.zip \*.gz \*.elc \*.tar      \
                \*.gz \*.bz2                    \
                .#\* \*.#\*                     \
                \*~ \*#

        fi

        echo "$id: Made release $dir/$file"
        ls -l $dir/$file
    )

    echo "$id: Call ncftpput upload.sourceforge.net /incoming $dir/$file"

}

prjttinit                        # Run initializer

export PROJ_TINY_TOOLS_ROOT

# End of file
