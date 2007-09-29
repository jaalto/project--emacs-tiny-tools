#!/usr/bin/perl
#
# Perl -- Dos to unix and unix to dos line ending converter
#
#  File id
#
#       Copyright (C) 2000-2007 Jari Aalto
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

# {{{ Initial setup

BEGIN { require 5.004 }

use autouse 'Pod::Text'     => qw( pod2text );

use strict;

use English;
use Getopt::Long;

    use vars qw ( $VERSION );

    #   This is for use of Makefile.PL and ExtUtils::MakeMaker
    #   So that it puts the tardist number in format YYYY.MMDD
    #   The REAL version number is defined later

    #   The following variable is updated by my Emacs setup whenever
    #   this file is saved

    $VERSION = '2007.0905.2133';


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

        $GO
    );

    $PROGNAME   = "ripdoc.pl";
    $LIB        = $PROGNAME;

    $FILE_ID  = q$Id: dos2unix.pl,v 2.10 2007/05/01 17:20:29 jaalto Exp $;
    $VERSION  = (split (' ', $FILE_ID))[2];
    $CONTACT  = "";
    $URL      = "http://tiny-tools.sourceforge.net/";

    $OUTPUT_AUTOFLUSH = 1;

    $GO       = "flip";
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

dos2unix.pl - Convert between Dos and Unix line endings

=head1 README

This program converts between Unix and Dos text files. Without any
options the files are converted to the opposite line endings.

=head1 SYNOPSIS

    dos2unix.pl --unix --no-backup *
    dos2unix.pl --dos  *
    dos2unix.pl --test *

=head1 OPTIONS

=head2 Gneneral options

=over 4

=item B<--dos>

Convert to Dos

=item B<--no-backup>

Delete backup after conversion (.orig)

=item B<--test>

Detect type, do no conversion. This cancels B<--dos> and
B<--unix> and turns on B<--verbose>

=item B<--unix>

Convert to Unix

=back

=head2 Miscellaneous options

=over 4

=item B<--Debug -d LEVEL>

Turn on debug with positive LEVEL number. Zero means no debug.

=item B<--help -h>

Print help

=item B<--verbose>

Print verbose mesages.

Print help

=item B<--Version>

Print contact and version information

=back

=head1 DESCRIPTION

Unix lines end to LF \n whereas DOS terminates lines with CRLF \r\n
This program converts between Unix and Dos text files. Without any
options the files are converted to the opposite line endings.

=head1 TROUBLESHOOTING

Turn on debug with B<--Debug> and run in B<--test> mode.

=head1 EXAMPLES

To check what type of text files you have in directory:

    dos2unix.pl --test *

=head1 ENVIRONMENT

No environment settings.

=head1 FILES

Original files are moved to .orig unless backup is supressed
with B<--no-backup>

=head1 SEE ALSO

flip(1)

=head1 BUGS

None known.

=head1 AVAILABILITY

http://cpan.perl.org/modules/by-authors/id/J/JA/JARIAALTO/

=head1 SCRIPT CATEGORIES

CPAN/Administrative

=head1 COREQUISITES

<what CPAN modules are needed to run this program>

=head1 OSNAMES

C<any>

=head1 VERSION

$Id: dos2unix.pl,v 2.10 2007/05/01 17:20:29 jaalto Exp $

=head1 AUTHOR

Copyright (C) 2000-2007 Jari Aalto. All rights reserved.
This program is free software; you can redistribute and/or modify program
under the same terms as Perl itself or in terms of Gnu General Public
licence v2 or later.

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

    use vars qw
    (
        $NO_BACKUP
        $TEST
        $QUIET
        $debug
        $verb
    );

    $debug           = 0;

    # .................................................... read args ...

    Getopt::Long::config( qw
    (
        no_ignore_case
        require_order
    ));


    my ( $dos, $unix, $help, $ver );

    GetOptions      # Getopt::Long
    (
          "h|help"              => \$help
        , "verbose"             => \$verb
        , "Version"             => \$ver
        , "Debug"               => \$debug
        , "test"                => \$TEST

        , "dos"                 => \$dos
        , "unix"                => \$unix
        , "no-backup"           => \$NO_BACKUP
    );

    $ver            and die "$VERSION $PROGNAME $CONTACT $URL\n";
    $help           and Help();
    $verb = 1       if  $debug;

    $GO = "unix" if $unix;
    $GO = "dos"  if $dos;
    $GO = ""     if $TEST;

    $verb = 1    if $TEST;

}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Recognize the line ending type. The $ARG must have been set already.
#
#   INPUT PARAMETERS
#
#       $       [optional] If set, return opposite type.
#
#   RETURN VALUES
#
#       "dos" or "unix"
#
# ****************************************************************************


sub Type ( ;$ )
{
    my $ret;

    if ( /\r\n/ )
    {
        $ret = @ARG ? "unix" : "dos";
    }
    else
    {
        $ret = @ARG ? "dos" : "unix";
    }

    $ret;
}


# ****************************************************************************
#
#   DESCRIPTION
#
#       Convert th eline to given TYPE
#
#   INPUT PARAMETERS
#
#       $       type. "unix" or "dos"
#
#   RETURN VALUES
#
#       none
#
# ****************************************************************************


sub Convert( $ )
{
    my ( $type ) = @ARG;

    if( $type eq "unix" )
    {
        s/\r\n/\n/g;
    }
    else
    {
        s/\n/\r\n/g  unless /\r\n\Z/ ;
    }
}

# ............................................................. &main ...

    Initialize();
    HandleCommandLineArgs(),

    my $id = "$LIB.main";
    my ( @files, $source, $prev );


    for ( @ARGV )
    {
        #       Win32 can't expand "*". We must do it here.
        #       Grep only FILES, not directories.

        push @files, grep -f, glob $ARG;
    }


    local ( *IN, *OUT );

    for $source ( @files )
    {
        my $out = "$source.out";
        my $err;

        unless ( $TEST )
        {
            open OUT, ">$out"  or $err++;
            binmode OUT;
        }

        open IN,  $source  or $err++;

        if ( $err )
        {
            warn "$id: Can't open $ARG or $out";
        }
        else
        {
            my ( $detected, $otype, $skip );
            my $type = $GO;                     # the wanted conversion


            binmode IN;
            while ( defined($ARG = <IN>) )
            {
                unless ( $detected )
                {
                    $otype = Type();
                    if ( $GO eq "flip" )
                    {
                        $type = Type( "flip" );
                    }
                }

                $otype eq $type     and $skip = "skipped" , last;
                $TEST               and $skip = "test", last;

                if ( $type eq "dos" )
                {
                    not  /\r\n/  and s/\n/\r\n/;
                }
                else
                {
                    s/\r+//g;
                }

                print OUT $ARG;
            }

            close IN;
            close OUT   unless $TEST;

            my $orig = "$source.orig";
            my $file = sprintf "%-25s", $source;

            $debug and
                   print "$otype $type $source -> $orig $out -> $source\n";

            unless ( $skip )
            {

                if ( $NO_BACKUP  or  rename $source, $orig )
                {
                    rename $out, "$source";
                }
                else
                {
                    print "Rename $source --> $orig failed";
                }
            }

            if ( (!$skip and $source ne $prev) or $verb )
            {
                print "$file ($otype) $type  $skip\n";
            }

            #   Make sure this temporary file is gone.

            unlink $out  if -e $out;

            $prev = $source;

        }
    }


0;
__END__
