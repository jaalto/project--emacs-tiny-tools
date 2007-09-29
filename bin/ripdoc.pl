#!/usr/bin/perl
#
# ripdoc.pl -- Rip file's documentation to Technical text format
#
#   File id
#
#       Copyright (C)   1997-2007 Jari Aalto
#       Created:        1997-02
#       Keywords:       Perl, text conversion
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
#	You should have received a copy of the GNU General Public License
#	along with program. If not, write to the
#	Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
#	Boston, MA 02110-1301, USA.
#
#	Visit <http://www.gnu.org/copyleft/gpl.html> for more information

use autouse 'Pod::Text'     => qw( pod2text );

use 5.004;
use strict;
use English;
use Getopt::Long;

    use vars qw ( $VERSION );

    #   This is for use of Makefile.PL and ExtUtils::MakeMaker
    #   So that it puts the tardist number in format YYYY.MMDD
    #   The REAL version number is defined later
    #
    #   The following variable is updated by Emacs setup whenever
    #   this file is saved

    $VERSION = '2007.0905.2137';

# ****************************************************************************
#
#   DESCRIPTION
#
#       Set global variables for the program
#
#   INPUT PARAMETERS
#
#       none
#
#   RETURN VALUES
#
#       none
#
# ****************************************************************************

sub Initialize ()
{
    use vars qw
    (
        $PROGNAME
        $LIB

        $FILE_ID
        $VERSION
        $CONTACT
        $URL
    );

    $PROGNAME   = "ripdoc.pl";
    $LIB        = $PROGNAME;

    $FILE_ID  = q$Id: ripdoc.pl,v 2.18 2007/05/01 17:20:31 jaalto Exp $;
    $VERSION = (split (' ', $FILE_ID))[2];
    $CONTACT = "";
    $URL     = "http://tiny-tools.sourceforge.net/";

    $OUTPUT_AUTOFLUSH = 1;
}

# ***************************************************************** &help ****
#
#   DESCRIPTION
#
#       Print help and exit.
#
#   INPUT PARAMETERS
#
#       $msg    [optional] Reason why function was called.-
#
#   RETURN VALUES
#
#       none
#
# ****************************************************************************

=pod

=head1 NAME

ripdoc.pl - Rip documentation from the beginning of file

=head1 SYNOPSIS

    ripdoc.pl FILE FILE ..

=head1 OPTIONS

=head2 General options

=over 4

=item B<--doc>

Use default seach start: 'Documentation|Commentary'

=item B<--begin-regexp REGEXP> B<-bre REGEXP>

Search any beginning regexp mathing RE istead of default 'File id|Preface'

=item B<--end-regexp REGEXP> B<-ere REGEXP>

Search any Ending regexp mathing RE istead of default 'Change Log|History'

=item B<--ignore-regexp> B<-ire REGEXP>

Ignore lines matching RE. The default value ignores shell I<!/slash/bang>
lines.

=head2 Miscellaneous options

=item B<--debug> B<-d>

Turn on debug.

=item B<--help> B<-h>

Print help page.

=item B<--verbose> B<-v>

Turn on verbose messages.

=item B<--Version> B<-V>

Print program version and contact info.

=back

=head1 README

=head2 General

Perl and Java are execptions among the programming languages, because they
include a way to embed documentating inside program. Perl interpreter knows
POD and it can ignore those lines. In other programming languages, like
Emacs lisp and Shell programs, Procmail code, you do not have anything
out of the box to help you document the program to the outside world.
The choices are:

=over 4

=item *

A Separate document(s) for the program is maintained:
Unix man pages *.1, Text files .txt, Latex *.tex, texinfo Info pages *.info,
SGML, ...

=item *

Program prints a brief/complete manual when invoked with --help or -h.

=item *

Documentation is put to the beginning of the file and distributed
with the file

=back

This is the tool for the last bullet. The documentation is maintained at
the beginning of the distributed program(s). I<ripdoc.pl> extracts the
documentation which follows TF (Technical format:
ftp://cs.uta.fi/pub/ssjaaa/t2html.html ) guidelines. The idea is that you
can generate html docs similarly that what pod2html does. The conversion
goes like this:

    % ripdoc.pl code.sh | t2html.pl > code.html

=head2 How to write documentation

In order to use this program, you must write the documentation
to the beginning of file in the following format:

    #!/bin/sh
    #
    # file.extension -- proper first line description
    #
    #   Preface starts at colum 4
    #
    #       txt txt txt at column 8
    #       txt txt txt at column 8
    #
    #           Furher example code at column 12
    #           More code examples at column 12
    #
    #   Next heading
    #
    #       txt txt txt at column 8
    #       txt txt xtx at column 8
    #       txt txt xtx at column 8

=head2 Finer specifications for the documenation format

Program reads documentation from the beginning of file.
The very first line determines the comment string in the file.
The documentation starts when a header I<Preface> or I<File> I<Id> is
found

    #   Preface         # or "File id"

Documentation ends when either of these headers are found:

    #   Change Log:     # or "History"

The I<Preface> should explain how the package springed into
existense and the rest of the documentation follows after that.

Most important is the first line or near first, if the file is a
shell script, must be exactly like the following. You _must_ not
use double dashes in any other heading.This gives the name
of the file and description string. Use what(1) marker at the
begining of sentence.

    # file.extension -- proper first line description

=head2 Notes on documentation format

Very First line determines what is the comment string that is
ripped away from the beginning of lines. You must not use
multiple of comment markers like above I<#######>, this will
handicap ripdoc.pl. Remember to start writing headings at column
four and write text at column 8. The following is not in TF format.

    #!/bin/sh
    #
    #######################################################
    #
    # file.extension -- proper first line description
    #
    #######################################################
    #
    # Preface starts at colum 1
    #
    #        txt txt txt at column 8
    #        txt txt txt at column 8
    #
    #            Furher example code at column 12
    #            More code examples at column 12
    #
    # Next heading is here
    #
    #        txt txt txt at column 8
    #        txt txt xtx at column 8
    #        txt txt xtx at column 8

=head1 SUITABILITY

You can run this program to rip out documentation from any file that
follows the 4 character indentation rule, which is the basis of TF
(technical format). The only requirement is that the comment markers are
single lined. C and Java-styled I<comment-start> I<comment-end> combination
cannot be handled, because the comment marker is determined from the
beginning of line.

          /* This comment documentation cannot be handled
           *
           */

=head1 EXAMPLES

You usually can combine the procuced clear text output to a text to html
fileter to generate html documentation out of the comments.

    % ripdoc.pl file.sh | t2html.pl > file.sh.html
    % ripdoc.pl file.cc | t2html.pl > file.cc.html

=head1 SEE ALSO

t2html(1), weblint(1), html2ps(1), ps2ascii(1)

=head1 BUGS

<known limitations>

=head1 AVAILABILITY

CPAN entry is at http://www.perl.com/CPAN-local//scripts/

=head1 SCRIPT CATEGORIES

CPAN/Administrative
html

=head1 PREREQUISITES

No additional CPAN modules required.

=head1 COREQUISITES

No optional CPAN modules needed.

=head1 OSNAMES

C<any>

=head1 VERSION

$Id: ripdoc.pl,v 2.18 2007/05/01 17:20:31 jaalto Exp $

=head1 AUTHOR

Copyright 1998-2003 Jari Aalto. All rights reserved. This program is
free software; you can redistribute it and/or modify it under the same
terms as Perl itself or in terms of Gnu General Public licence v2 or
later.

=cut

sub Help (;$)
{
    my $id  = "$LIB.Help";
    my $msg = shift;  # optional arg, why are we here...

    pod2text $PROGRAM_NAME;

    defined $msg  and  print $msg;

    exit 1;
}

# ************************************************************** &args *******
#
#   DESCRIPTION
#
#       Read and interpret command line arguments ARGV. Sets global variables
#
#   INPUT PARAMETERS
#
#       none
#
#   RETURN VALUES
#
#       none
#
# ****************************************************************************

sub HandleCommandLineArgs ()
{
    my    $id = "$LIB.HandleCommandLineArgs";

    my ( $help, $version, $doc );

    use vars qw
    (
        $BEGIN_REGEXP
        $END_REGEXP
        $IGNORE_REGEXP

        $QUIET
        $debug
        $verb

    );

    # ............................................... default values ...

    #   RCS Revision "Log" ends the description.

    $BEGIN_REGEXP   = 'File id|Preface';
    $END_REGEXP     = 'Change\s+Log|History|[$]Log: ';

    #   Ignore shebang lines

    $IGNORE_REGEXP   = '^.![/].*[/]';
    $debug           = 0;

    # .................................................... read args ...

    Getopt::Long::config( qw
    (
        no_ignore_case
        require_order
    ));

    GetOptions      # Getopt::Long
    (
          "h|help"              => \$help
        , "verbose"             => \$verb
        , "Version"             => \$version
        , "debug"               => \$debug

        , "doc"                 => \$doc
        , "bre|begin-regexp=s"  => \$BEGIN_REGEXP
        , "ere|end-regexp=s"    => \$END_REGEXP
        , "ire|ignore-regexp=s" => \$IGNORE_REGEXP
    );

    $version        and die "$VERSION $PROGNAME $CONTACT $URL\n";
    $help           and Help();
    $verb = 1       if  $debug;

    if ( $doc )
    {
        $BEGIN_REGEXP = 'Documentation:|Commentary:';
    }
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Main entry point
#
#   INPUT PARAMETERS
#
#       none
#
#   RETURN VALUES
#
#       none
#
# ****************************************************************************

sub Main ()
{
    Initialize();
    HandleCommandLineArgs();

    my $BODY                = 0;
    my $BODY_MATCH_REGEXP   = "";
    my $COMMENT             = "";
    my $PADDING             = "";

    my( $ch1, $rest, $name );
    local $ARG;

    while ( <> )
    {
        next if /$IGNORE_REGEXP/o;

        if ( $COMMENT eq "" )
        {

            #   Find out what commenting syntax is for this file

            $COMMENT = $1 if /([^\s\n]+)/;

            if ( $COMMENT =~ /^;/ )         # Emacs lisp
            {
                $COMMENT = ";";
            }

            #   We must preserve indentation when removing comments.

            $PADDING = " " x length $COMMENT;

            $BODY_MATCH_REGEXP =
                    "([-a-zA-Z0-9.])([-a-zA-Z0-9.]+" .
                    "\\s+--+\\s+.*)"
                    ;

            $debug  and print "INIT: [$COMMENT] [$ARG]";
        }

        if ( $debug and /$BODY_MATCH_REGEXP/o )
        {
            printf "!!$BODY %d [$1] [$2] $ARG", length($`);
        }

        # ..................................... first line documentation ...
        #   Get first line name

        if ( not $BODY
             and /$BODY_MATCH_REGEXP/o
             #  the match to the left size must be short
             and length($PREMATCH) < 20
           )
        {
            $debug and print "BODY: $ARG";

            # convert first character to uppercase.

            ($ch1, $rest) = ($1,$2);

            if ( $ch1 !~ /[a-zA-Z]/ )
            {
                $verb and warn "$LIB: First line does not"
                             , " begin with letter. [$ARG] \n";

                $name = "$ch1$rest";
            }
            else
            {
                $name = uc($ch1) . $rest;
            }

            $debug  and  print "BODY: $name\n";

        }

        # ....................................................... bounds ...

        if  ( /^$COMMENT+\s+(?:$BEGIN_REGEXP)/oi )
        {
            $BODY = 1;
            $debug  and  printf "BEG:[$&] [$ARG]";
        }

        if  ( /^$COMMENT+\s+(?:$END_REGEXP)/oi )
        {
            $BODY = 0;
            $debug  and  printf "END:[$&] [$ARG]";
            next;
        }

        if ( $BODY )
        {
            $BODY == 1  and  print "$name\n\n";
            $BODY++;

            # .................................................... &lisp ...
            #   Delete lisp comment and tag lines by tinybm.el
            #   ;; ............ &thisTag ...
            #
            #   Also delete 3 comment markers, because they are not
            #   text sections
            #   ;;;Commentary:
            #
            #   Also delete folding.el tags ';; }}}' and ';; {{{'

            next if /;;;|;; \.\.|^ |}}}|{{{/;

            s/^;;+[*] _//;
            s/^;;+[*]//;
            s/^;;+/  /;

            # ................................................. &general ...

            s/^($COMMENT)/$PADDING/o;

            #   Make sure that:
            #
            #      Header 1 is here
            #   ^^^ 3 spaces is converted to 4 spaces.

            if ( /^ {3,4}(\w)(.*)/  )
            {
                $ARG = "    " . uc($1) . $2 . "\n";
            }

            print "$ARG";
        }
    }
}

Main();

0;
__END__
