#!/usr/bin/perl
#
# find-version.pl -- Find version number from files
#
#  File id
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
#	You should have received a copy of the GNU General Public License
#	along with program. If not, write to the
#	Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
#	Boston, MA 02110-1301, USA.
#
#	Visit <http://www.gnu.org/copyleft/gpl.html> for more information
#
#   Description
#
#        Print version numbers from files. The -S option searches the
#        executable along PATH (You need this option in Win32)
#
#           perl -S find-version.pl *

use autouse 'Pod::Text'     => qw( pod2text );

use 5.004;
use strict;
use English;
use File::Find;
use Getopt::Long;

    use vars qw ( $VERSION );

    #   This is for use of Makefile.PL and ExtUtils::MakeMaker
    #   So that it puts the tardist number in format YYYY.MMDD
    #   The REAL version number is defined later

    #   The following variable is updated by my Emacs setup whenever
    #   this file is saved

    $VERSION = '2007.0902.0850';

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
        $debug

        $PROGNAME
        $LIB

        $FILE_ID
        $VERSION
        $CONTACT
        $URL
        $WIN32
    );

    $PROGNAME   = "find-version.pl";
    $LIB        = $PROGNAME;
    my $id      = "$LIB.Initialize";

    $FILE_ID  = q$Id: find-version.pl,v 2.14 2007/05/01 17:20:30 jaalto Exp $;
    $VERSION  = (split (' ', $FILE_ID))[2];
    $CONTACT  = "";
    $URL      = "http://tiny-tools.sourceforge.net/";
    $WIN32    = 1   if  $OSNAME =~ /win32/i;

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

find-version.pl - Find version number from files

=head1 README

<short overall description here. This section is ripped by CPAN>

=head1 SYNOPSIS

    find-version.pl *

=head1 OPTIONS

=head2 Gneneral options

=over 4

=item B<--help -h>

Print help

=item B<--Version>

Print contact and version information

=back

=head2 Miscellaneous options

=over 4

=item B<--debug -d LEVEL>

Turn on debug with positive LEVEL number. Zero means no debug.

=back

=head1 DESCRIPTION

<Longer program description>

=head1 TROUBLESHOOTING

None.

=head1 EXAMPLES

None.

=head1 ENVIRONMENT

<any environment variable settings>

=head1 FILES

<what files program generates uses>

=head1 SEE ALSO

<references to other programs>

=head1 BUGS

No known limitations.

=head1 AVAILABILITY

http://tiny-tools.sourceforge.net/

=head1 SCRIPT CATEGORIES

CPAN/Administrative

=head1 COREQUISITES

Uses tandard Perl modules.

=head1 OSNAMES

C<any>

=head1 VERSION

$Id: find-version.pl,v 2.14 2007/05/01 17:20:30 jaalto Exp $

=head1 AUTHOR

Copyright (C) 2000-2009 Jari Aalto. All rights reserved.
This program is free software; you can redistribute and/or modify program
under the same terms as Perl itself or in terms of Gnu General Public
license v2 or later.

This file is part of http://tiny-tools.sourceforge.net/

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
        $debug
        $verb
        $test
        $RECURSE
    );


    my ( $version, $help );

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
        , "test"                => \$test
        , "recurse"             => \$RECURSE
    );

    $version        and die "$VERSION $PROGNAME $CONTACT $URL\n";
    $help           and Help();
    $verb = 1       if  $debug;
    $verb = 1       if  $test;

}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Find() calls us.
#
#   INPUT PARAMETERS
#
#
#
#   RETURN VALUES
#
#       none
#
# ****************************************************************************

sub wanted (@)
{
    my    $id = "$LIB.wanted";
    my (@dir) = @ARG;
}


# ............................................................ &main ...

    Initialize();
    HandleCommandLineArgs();

    my $id  = "$LIB.main";

    # .......................................... expand command line ...

    my @files;

    for ( @ARGV )
    {
        #       Win32 can't expand "*". We must do it here.
        #       Grep only FILES, not directories.

        push @files, grep { -f and -r } glob $ARG;
    }

    local ( *FILE, $ARG );
    my    ( @content, @all, $file);


    for $file ( @files )
    {

        # ..................................................... read ...

        unless ( open FILE, $file )
        {
            print "Cannot open $file\n";
        }
        else
        {
            binmode FILE;
            @content = <FILE>; close FILE;
        }

        # ................................ Find one line description ...

        chomp $content[0];
        $content[0] =~ s/^.*\(#\)\s*|\n$//g;

        my $synop = $content[0];

        #       Not in a first line? Next non-blank then

        if ( $synop !~ /--/ )
        {
            for ( @content )
            {
                if ( /--/ )
                {
                    #   Remove beginning comment "#" and ";"
                    #   Removed SCCS styled what(1) id  @(#)

                    ( $synop = $ARG )  =~ s/^.*\([#;]\)\s*|\n$|\Q@(#)//g;
                    last;
                }
            }
        }

        $debug and print "$id: synop>> $synop\n";



        my $len = 80;

        if (length $synop > $len )
        {
            $synop = substr $synop,0, $len ;
        }

        my @s = map { s/[\r\n]//; $ARG } split /\s*--+\s*/, $synop;

        # ............................................. Grep version ...


        my ( $FILE, $ver, $date, $time, $user );

        if ( $ver = (grep(/\$Id:/, @content))[0] )
        {

            $ver =~ s/^.*id://i;
            chomp $ver;

            ($FILE, $ver, $date, $time, $user ) = split ' ', $ver;

            $FILE =~ s/,v//;

            if ( $date =~ m,(\d\d\d\d)/(\d\d)/(\d\d), )
            {
                $date = "$1-$2-$3";
            }

        }
        elsif ( $ver = (grep(/\Version:\s+(\d+\.[\d.]+)/, @content))[0] )
        {
            $date = "yyyy-mm-dd";
        }
        else
        {
            $ver  = "?.?";
            $date = "yyyy-mm-dd";
        }

        my $str = sprintf "%-15s %-5s %10s %-14s %s\n"
                , $file, $ver, $date, $s[1] ;

        push @all, $str ;

    }

    print sort @all;

0;
__END__
