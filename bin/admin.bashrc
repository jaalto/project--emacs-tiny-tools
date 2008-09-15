#!/bin/bash
#
#   File id
#
#       Copyright (C) 2000-2009 Jari Aalto
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
#       You should have received a copy of the GNU General Public
#       License along with program. If not,
#       write to the Free Software Foundation, Inc., 51 Franklin
#       Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
#       Visit <http://www.gnu.org/copyleft/gpl.html> for more information
#
#   Documentation
#
#       These bash functions will help managing Sourceforge project. You need:
#
#       bash        (Unix)  http://www.fsf.org/directory/bash.html
#                   (Win32) http://www.cygwin.com/
#       Perl 5.4+   (Unix)  http://www.perl.org/
#                   (Win32) Perl is included in Cygwin tools
#       t2html.pl   Perl program to convert text -> HTML
#                   http://perl-text2html.sourceforge.net/
#
#       This file is for the Admin or Co-Developer of the project:
#
#           http://sourceforge.net/projects/tiny-tools
#           http://tiny-tools.sourceforge.net/
#
#       Include this file to your $HOME/.bashrc and make the necessary
#       modifications
#
#           SF_TINY_TOOLS_USER=<sourceforge-login-name>
#           SF_TINY_TOOLS_USER_NAME="FirstName LastName"
#           SF_TINY_TOOLS_ROOT=~/cvs-projects/tiny-tools
#
#           source ~/cvs-projects/tiny-tools/bin/admin.bashrc
#
#       Functions related to release maintenance contain underscore (_)
#       in function name.

VERSION="2007.0905.2127"

function sfttinit ()
{
    local id="sfttinit"

    SF_TINY_TOOLS_ROOT=${SF_TINY_TOOLS_ROOT:-"."}

    if [ "$SF_TINY_TOOLS_USER" = "" ]; then
       echo "$id: Identity SF_TINY_TOOLS_USER unknown."
    fi

    if [ "$SF_TINY_TOOLS_USER_NAME" = "" ]; then
       echo "$id: Identity SF_TINY_TOOLS_USER_NAME unknown."
    fi
}

function sfttdate ()
{
    date "+%Y.%m%d"
}

function sftttime ()
{
    date "+%Y-%m-%d %H:%M:%S"
}

function sfttfilesizeAwk ()
{
    # This was old implementation. Found better bash solution.
    ls -la $1 | awk '{print $5}'
}

function sfttfilesize ()
{
    #   put line into array ( .. )

    local line
    line=($(ls -l "$1"))

    #   Read 4th element from array
    #   -rw-r--r--    1 root     None         4989 Aug  5 23:37 file

    echo ${line[4]}
}

function sfttask ()
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

function sfttscp ()
{
    #   To upload file to project, call from shell prompt
    #
    #       bash$ sfttscp <FILE>
    #       bash$ sfttscp -d SUBDIR <FILE>

    local dir

    if [ "$1" == "-d" ]; then
       shift
       dir="$1"
       shift
    fi

    local id="sfttscp"

    local sfuser=$SF_TINY_TOOLS_USER
    local sfproject=t/ti/tiny-tools

    if [ "$sfuser" = "" ]; then
        echo "$id: SF_TINY_TOOLS_USER is not set."
        return
    fi

    echo scp -C $* $sfuser@shell.sourceforge.net:/home/groups/$sfproject/htdocs/
}

function sfttscp_all ()
{
    #   Upload all relevant files to project

    local id="sfttscp_all"

    local sfuser=$SF_TINY_TOOLS_USER
    local sfproject=t/ti/tiny-tools

    if [ "$sfuser" = "" ]; then
        echo "$id: SF_TINY_TOOLS_USER is not set."
        return
    fi

    local dir=$SF_TINY_TOOLS_ROOT/doc/html
    local target="$sfuser@shell.sourceforge.net:/home/groups/$sfproject/htdocs/"

    local args='$(ls *html | egrep -v "java|autoload")'

    echo "$id: scp $arg $target"

    (
        cd $dir
        eval "scp $arg $target"
    )

    echo "$id: Done."
}

function sftt_movehtml ()
{
    if [ -d ../../html/  ]; then
        mv *.html ../../html/
    elif [ -d ../html/  ]; then
        mv *.html ../html/
    else
        echo "$id: Can't move generated HTML to html/"
    fi
}

function sftthtml ()
{
    #   To generate HTML documentation located in /doc directory, call
    #
    #       bash$ sftthtml <FILE.txt>
    #
    #   To generate Frame based documentation
    #
    #       bash$ sftthtml <FILE.txt> --html-frame
    #
    #   For simple page, like README.txt
    #
    #       bash$ sftthtml <FILE.txt> --as-is

    local id="sftthtml"

    local input="$1"
    shift

    if [ "$input" = "" ]; then
        echo "id: usage is FILE [html-options]"
        return
    fi

    if [ ! -f "$input" ]; then
        echo "$id: No file found [$input]"
        return
    fi

    local opt=$*

    echo "Htmlizing .. $id $input $opt"

    #   perl -S  will work both in Unix and Win32. The -S causes
    #   perl to search $PATH

    perl -S t2html.pl                                               \
          $opt                                                      \
          --author "$SF_TINY_TOOLS_USER_NAME"                       \
          --url    "http://tiny-tools.sourceforge.net"              \
	  --Auto-detect                                             \
          --Out                                                     \
          $input

    sftt_movehtml
}

function sftthtmlall ()
{
    local id="sftt_htmlall"

    #   loop all *.txt files and generate HTML
    #   If filesize if bigger than 15K, generate Framed HTML page.

    local dir=$SF_TINY_TOOLS_ROOT/doc/txt

    (
        cd $dir || return
        echo "$id: Source dir " $(pwd)

        for file in *.txt;
        do

            local opt=""

            sftthtml $file "$opt"

        done
    )

    echo "$id: done."
}

function sftt_nice_text ()
{
    local id="sftt_nice_text"
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

function sfttcmd ()
{

    #   To send command to the host. This lists the htdocs directory
    #
    #       bash$ sfcvstoolscmd ls

    local sfuser=$SF_TINY_TOOLS_USER
    local sfproject=t/ti/tiny-tools
    local dir=/home/groups/$sfproject/htdocs/

    if [ "$SF_TINY_TOOLS_USER" = "" ]; then
         echo "$id: SF_TINY_TOOLS_USER is not set."
        return
    fi

    ssh $sfuser@shell.sourceforge.net "cd $dir; $*"
}


function sftt_tinypath_doc ()
{
    # Update documentation that is generated form original files.

    local id="sftt_doc"

    local bin=$SF_TINY_TOOLS_ROOT/bin
    local doc=$SF_TINY_TOOLS_ROOT/doc/txt
    local lisp=$SF_TINY_TOOLS_ROOT/lisp/tiny

    local cmd="perl $bin/ripdoc.pl $lisp/tinypath.el"
    local to="$doc/emacs-tinypath.txt"

    echo "$cmd > $to"

    $cmd > $to

    dummy=$(sftt_nice_text $to)

    echo "$id: done."
}

function sftt_docLispManual ()
{
    local id="sftt_docLispManual"

    #   Rip all documentation from lisp files
    #   and update tiny-tools.txt

    local dir=$SF_TINY_TOOLS_ROOT/lisp/tiny
    local outDir=$SF_TINY_TOOLS_ROOT/doc/txt/

    local out=emacs-tiny-tools
    local out1=${out}-part1.src
    local out2=${out}-part2.src
    local final=${out}.txt

    local cmd="cd $dir && perl -S ripdoc.pl"
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

function sftt_linkcheck ()
{
    # Check if the URL links are valid

    local id="sftt_linkcheck"

    local dir="$SF_TINY_TOOLS_ROOT/doc/txt"
    local cache="$HOME/tmp/sftt-link.cache"
    local log="$HOME/tmp/sftt-link.log"

    if test -f "$cache" && sfttask "$id: Remove $cache (y/[n])?"
    then
        echo "$id: rm $cache"
        rm $cache
    fi

    local cmd="perl -S t2html.pl --Link-check-single \
 --quiet --Link-cache $cache"


    local date=$(sftttime)

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

function sftt_release_check ()
{
    #   Remind that that everything has been prepared
    #   Before doing release

    if sfttask '[sftt_doc] Generate tinypath docs (y/[n])?'
    then
        echo "Running..."
        sftt_doc
    fi

    if sfttask '[sftt_docLispManual] Generate complete lisp docs (y/[n])?'
    then
        echo "Running..."
        sftt_docLispManual
    fi

    if sfttask '[sftt_linkcheck] Run URL link check? (y/[n])'
    then
        echo "Running..."
        sftt_linkcheck
    fi

    if sfttask '[sftt_linkcheck] Convert all text files to HTML? (y/[n])'
    then
        echo "Running..."
        sftt_htmlall
    fi

    if sfttask '[sftt_linkcheck] Upload HTML to sourceforge? (y/[n])'
    then
        echo "Running..."
        sfttscp_all
    fi
}

function sftt_release ()
{
    local id="sftt_release"

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
                    return
                    ;;
    esac

    sftt_release_check

    local dir=/tmp

    if [ ! -d $dir ]; then
        echo "$id: Can't make release. No directory [$dir]"
        return
    fi

    if [ ! -d "$SF_TINY_TOOLS_ROOT" ]; then
        echo "$id: No SF_TINY_TOOLS_ROOT [$SF_TINY_TOOLS_ROOT]"
        return
    fi


    local base=emacs-tiny-tools
    local ver=$(sfttdate)
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


        cp -r $SF_TINY_TOOLS_ROOT $dir/$todir

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

sfttinit                        # Run initializer

export SF_TINY_TOOLS_ROOT

# End of file
