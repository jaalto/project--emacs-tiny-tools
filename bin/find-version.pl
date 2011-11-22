#!/usr/bin/perl
#
#  File id
#
#	find-version.pl -- Find version number from files
#
#   Copyright
#
#       Copyright (C) 2000-2010 Jari Aalto
#
#   License
#
#       This program is free software; you can redistribute it and/or modify
#       it under the terms of the GNU General Public License as published by
#       the Free Software Foundation; either version 2 of the License, or
#       (at your option) any later version.
#
#       This program is distributed in the hope that it will be useful,
#       but WITHOUT ANY WARRANTY; without even the implied warranty of
#       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#       GNU General Public License for more details.
#
#       You should have received a copy of the GNU General Public License
#       along with this program. If not, see <http://www.gnu.org/licenses/>.
#
#   Description
#
#        Print version numbers from files. The -S option searches the
#        executable along PATH (You need this option in Win32)
#
#           perl -S find-version.pl *

use autouse 'Pod::Text' => qw( pod2text );

use strict;
use English qw(-no_match_vars);
use File::Find;
use Getopt::Long;

#   The following variable is updated by developer's Emacs whenever
#   this file is saved

our $VERSION = '2010.1205.2200';

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
        $CONTACT
        $URL
        $WIN32
    );

    $PROGNAME   = "find-version.pl";
    $LIB        = $PROGNAME;
    my $id      = "$LIB.Initialize";

    $CONTACT	= "";
    $URL	= "";
    $WIN32	= 1   if  $OSNAME =~ /win32/i;

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

=head1 SYNOPSIS

    find-version.pl FILE ...

=head1 DESCRIPTION

Extract version information from files.

=head1 OPTIONS

=head2 Gneneral options

=over 4

=item B<-h, --help>

Print help

=item B<-V, --Version>

Print contact and version information

=back

=head2 Miscellaneous options

=over 4

=item B<-d, --debug LEVEL>

Turn on debug with positive LEVEL number. Zero means no debug.

=back

=head1 TROUBLESHOOTING

None.

=head1 EXAMPLES

None.

=head1 BUGS

None known.

=head1 ENVIRONMENT

None.

=head1 FILES

None.

=head1 SEE ALSO

licensecheck(1) program in Debian.

=head1 EXIT STATUS

Not defined.

=head1 DEPENDENCIES

Uses standard Perl modules.

=head1 BUGS AND LIMITATIONS

None.

=head1 AUTHOR

Jari Aalto

=head1 LICENSE AND COPYRIGHT

Copyright (C) 2000-2010 Jari Aalto

This program is free software; you can redistribute and/or modify
program under the terms of GNU General Public license either version 2
of the License, or (at your option) any later version.

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
#	See Find()
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

sub Main ()
{
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

    local (*FILE, $ARG);
    my (@content, @all, $file);


    for $file ( @files )
    {

        # ..................................................... read ...

        unless ( open FILE, "<", $file )
        {
            print "Cannot open $file\n";
        }
        else
        {
            binmode FILE;
            @content = <FILE>;
	    close FILE;
        }

        # ................................ Find one line description ...

	my $firstline = $content[0];
        chomp $firstline;
        $firstline =~ s/^.*\(#\)\s*|\n$//g;

        my $synopsis = $firstline;

        #  Not in a first line? Next non-blank then

        if ( $synopsis !~ /--/ )
        {
            for ( @content )
            {
                if ( /--/ )
                {
                    #   Remove beginning comment "#" and ";"
                    #   Removed SCCS styled what(1) id "@(#)"

                    ( $synopsis = $ARG )  =~ s/^.*\([#;]\)\s*|\n$|\Q@(#)//g;
                    last;
                }
            }
        }

        $debug and print "$id: synop>> $synopsis\n";



        my $len = 80;

        if (length $synopsis > $len )
        {
            $synopsis = substr $synopsis,0, $len ;
        }

        my @s = map { s/[\r\n]//; $ARG } split /\s*--+\s*/, $synopsis;

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
}

Main();

0;
__END__
